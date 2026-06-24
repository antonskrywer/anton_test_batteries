# ============================================================
# analysis.R — SLT_calibration Monitor
# Datenladen & Aufbereitung (nach Vorbild Klaus Frielers longgold_monitor)
# ============================================================

library(tidyverse)
library(lubridate)

messagef <- function(...) message(sprintf(...))

# ------------------------------------------------------------
# Pfade
# ACHTUNG: result_path muss noch verifiziert werden (offenes
# Root-Cause-1-Problem aus der alten App — Pfad-Mismatch
# Server vs. Nextcloud-Mac). Diagnose-Snippet siehe Notizen
# am Ende des Chats.
# ------------------------------------------------------------
on_server <- Sys.info()[["sysname"]] != "Darwin"

result_path <- if (on_server) {
  "/srv/shiny_server/SLT_calibration/output/results/"
} else {
  "/Users/Anton/Nextcloud/anton_test_batteries/output/results/SLT/"
}

session_path <- stringr::str_replace(result_path, "results", "sessions")

messagef("Monitor läuft auf %s — result_path = %s",
         ifelse(on_server, "Server", "lokal (Mac)"), result_path)

# ------------------------------------------------------------
# Block -> Schwierigkeit Mapping (Fallback)
# Spiegelt die Zuordnung aus SLT_Kalibrierung_Design.md (Mai 2026).
# Wird NUR verwendet, falls SLT_item_bank2 keine eigene
# `difficulty`-Spalte mitbringt (siehe extract_item_data()).
# Falls die Blockzuordnung sich ändert, bitte HIER anpassen.
# ------------------------------------------------------------
block_difficulty_map <- tibble::tribble(
  ~block, ~difficulty,
  3,    "easy",
  4,    "easy",
  7,    "easy",
  8,    "easy",
  1,    "medium",
  2,    "medium",
  5,    "medium",
  6,    "medium",
  9,    "medium",
  10,    "medium",
  13,    "medium",
  14,    "medium",
  11,    "hard",
  12,    "hard",
  15,    "hard",
  16,    "hard"
)

is_debug_session <- function(prolific_pid) {
  is.na(prolific_pid) | prolific_pid == ""
}

# ------------------------------------------------------------
# Personen-Ebene: 1 Zeile pro Versuchsperson
# ------------------------------------------------------------
extract_person_data <- function(file) {
  x <- readRDS(file)
  
  session_time <- x$session$time_started
  attr(session_time, "tzone") <- "UTC"
  session_time <- lubridate::with_tz(session_time, "Europe/Berlin")
  
  df_person <- tibble(
    id = x$session$p_id,
    
    session_datetime = session_time,
    session_date     = format(session_time, "%Y-%m-%d"),
    session_clock    = format(session_time, "%H:%M:%S"),
    session_duration_min = round(as.numeric(
      difftime(x$session$current_time, x$session$time_started, units = "mins")
    ), 1),
    
    # Prolific-URL-Parameter — liegen unter x$results$..., NICHT unter
    # x$passive$results$results$... (das wäre die Struktur laufender
    # Sessions, nicht fertiger Result-Dateien)
    prolific_pid        = if (!is.null(x$results$prolific_pid))        x$results$prolific_pid        else NA_character_,
    prolific_study_id   = if (!is.null(x$results$prolific_study_id))   x$results$prolific_study_id   else NA_character_,
    prolific_session_id = if (!is.null(x$results$prolific_session_id)) x$results$prolific_session_id else NA_character_,
    
    session.complete = if (!is.null(x$session$complete)) x$session$complete else NA,
    
    DEG.school_degree            = x$DEG$`School Degree`,
    DEG.country_of_residence     = x$DEG$`Country of Residence`,
    DEG.second_language          = x$DEG$`Second Language`,
    DEG.first_language           = x$DEG$`First Language`,
    DEG.country_formative_years  = x$DEG$`Country Formative Years`,
    DEG.age                      = as.numeric(x$DEG$Age),
    DEG.gender                   = as.numeric(x$DEG$Gender),
    
    SLT.score = as.numeric(x$SLT$score),
    
    GMS.general               = as.numeric(x$GMS$General),
    GMS.perceptual_abilities  = as.numeric(x$GMS$`Perceptual Abilities`),
    GMS.musical_training      = as.numeric(x$GMS$`Musical Training`),
    GMS.active_engagement     = as.numeric(x$GMS$`Active Engagement`),
    GMS.instrument            = as.numeric(x$GMS$Instrument),
    GMS.start_age             = as.numeric(x$GMS$`Start Age`),
    GMS.absolute_pitch        = as.numeric(x$GMS$`Absolute Pitch`)
  )
  
  df_person %>%
    mutate(
      DEG.age = round(DEG.age / 12, 1),
      DEG.gender = factor(DEG.gender,
                          levels = c(1, 2, 3, 4),
                          labels = c("male", "female", "diverse", "rather not say")),
      GMS.absolute_pitch = factor(GMS.absolute_pitch,
                                  levels = c(1, 2),
                                  labels = c("yes", "no")),
      GMS.instrument = factor(GMS.instrument,
                              levels = 1:21,
                              labels = c("no instrument", "voice", "piano", "guitar", "drums",
                                         "xylophone", "flute", "oboe", "clarinet", "basoon",
                                         "trumpet", "trombone", "tuba", "saxophone", "horn",
                                         "violin", "cello", "viola", "double bass", "harp", "other"))
    )
}

# ------------------------------------------------------------
# Item-Ebene (long format): 1 Zeile pro Trial
# Grundlage für die Block-/Schwierigkeits-Aufschlüsselung.
# ------------------------------------------------------------
extract_item_data <- function(file) {
  x <- readRDS(file)
  items <- x$SLT$items
  
  if (is.null(items) || is.null(items$block) || is.null(items$correct)) {
    return(NULL)
  }
  
  df <- tibble(
    id      = x$session$p_id,
    block   = as.numeric(items$block),
    style   = if (!is.null(items$style)) as.character(items$style) else NA_character_,
    correct = as.numeric(items$correct)
  )
  
  # Bevorzugt: direkte difficulty-Spalte aus SLT_item_bank2.
  # Fallback: Mapping-Tabelle oben (block_difficulty_map).
  if (!is.null(items$difficulty)) {
    df$difficulty <- as.character(items$difficulty)
  } else {
    df <- df %>% dplyr::left_join(block_difficulty_map, by = "block")
  }
  
  df
}

# ------------------------------------------------------------
# Sessions inkl. Abbrecher (Dropout-Tracking)
# ------------------------------------------------------------
read_sessions <- function(session_path) {
  dirs <- list.files(session_path, full.names = TRUE)
  
  purrr::map_dfr(dirs, function(d) {
    data_file <- file.path(d, "data.RDS")
    ts_file   <- file.path(d, "timestamp.RDS")
    
    if (!file.exists(data_file) || !file.exists(ts_file)) return(NULL)
    
    data_f     <- readRDS(data_file)
    time_stamp <- readRDS(ts_file)
    
    tibble(
      p_id                  = data_f$passive$p_id,
      time_started          = data_f$passive$time_started,
      time_last_modified    = time_stamp,
      session_duration_min  = round(as.numeric(
        difftime(time_stamp, data_f$passive$time_started, units = "mins")), 1),
      num_restarts          = data_f$passive$num_restarts,
      tests_finished        = paste(data_f$passive$results %>% as.list() %>% names(), collapse = "; ")
    )
  })
}

# ------------------------------------------------------------
# Master-Setup: lädt + bündelt alle .rds-Dateien zu Personen-
# und Item-Ebene. Analog zu setup_workspace() im alten Monitor,
# bewusst OHNE globalenv-Assignment (mehrere parallele Shiny-
# Sessions auf dem Server würden sich sonst eine globale
# Variable teilen).
# ------------------------------------------------------------
setup_workspace <- function(result_path) {
  files <- list.files(result_path, pattern = "\\.rds$", full.names = TRUE, recursive = TRUE)
  
  list(
    person  = purrr::map_dfr(files, extract_person_data),
    items   = purrr::map_dfr(files, extract_item_data),
    n_files = length(files)
  )
}