
# Load R packages
library(shiny)
library(shinythemes)
library(tidyverse)
library(purrr)
library(DT)

on_server <- Sys.info()[["sysname"]] != "Darwin"

path <- if (on_server) {
  "/srv/shiny_server/SLT_calibration/output/results/"
} else {
  "/Users/Anton/Nextcloud/anton_test_batteries/output/results/SLT/"
}
messagef <- function(...) message(sprintf(...))
messagef("Monitor läuft auf %s — path = %s", ifelse(on_server, "Server", "lokal (Mac)"), path)


files <- list.files(path, pattern = "\\.rds$", full.names = TRUE)


extract_person_data <- function(file) {
  x <- readRDS(file)  
  session_time <- x$session$time_started
  attr(session_time, "tzone") <- "UTC"
  session_time <- lubridate::with_tz(session_time, "Europe/Berlin")
  # Basis-Tibble erstellen
  df_person <- tibble(
    id = x$session$p_id,
    session_datetime = session_time,
    session_date     = format(session_time, "%Y-%m-%d"),
    session_clock    = format(session_time, "%H:%M:%S"),
    session_duration_min = round(as.numeric(
      difftime(x$session$current_time, x$session$time_started, units = "mins")
    ), 1),
    ### GEÄNDERT START — Prolific-URL-Parameter einlesen ###
    prolific_pid         = if(!is.null(x$results$prolific_pid))       x$results$prolific_pid       else NA_character_,
    prolific_study_id    = if(!is.null(x$results$prolific_study_id))  x$results$prolific_study_id  else NA_character_,
    prolific_session_id  = if(!is.null(x$results$prolific_session_id)) x$results$prolific_session_id else NA_character_,
    ### GEÄNDERT ENDE ###
    session.complete = if(!is.null(x$session$complete)) x$session$complete else NA,
    DEG.school_degree = x$DEG$`School Degree`,
    DEG.country_of_residence = x$DEG$`Country of Residence`,
    DEG.second_language = x$DEG$`Second Language`,
    DEG.first_language = x$DEG$`First Language`,
    DEG.country_formative_years = x$DEG$`Country Formative Years`,
    DEG.age = as.numeric(x$DEG$Age),
    DEG.gender = as.numeric(x$DEG$Gender),
    SLT.score = as.numeric(x$SLT$score),
    GMS.general = as.numeric(x$GMS$General),
    GMS.perceptual_abilities = as.numeric(x$GMS$`Perceptual Abilities`),
    GMS.musical_training = as.numeric(x$GMS$`Musical Training`),
    GMS.active_engagement = as.numeric(x$GMS$`Active Engagement`),
    GMS.instrument = as.numeric(x$GMS$Instrument),
    GMS.start_age = as.numeric(x$GMS$`Start Age`),
    GMS.absolute_pitch = as.numeric(x$GMS$`Absolute Pitch`)
  )
  
  # Transformationen und Labels zuweisen
  df_person <- df_person %>%
    mutate(
      # Alter von Monaten in Jahre umrechnen (auf 1 Nachkommastelle gerundet)
      DEG.age = round(DEG.age / 12, 1),
      
      # Geschlecht rekodieren
      DEG.gender = factor(DEG.gender, 
                          levels = c(1, 2, 3, 4), 
                          labels = c("male", "female", "diverse", "rather not say")),
      
      # GMS Absolutes Gehör rekodieren
      GMS.absolute_pitch = factor(GMS.absolute_pitch, 
                                  levels = c(1, 2), 
                                  labels = c("yes", "no")),
      
      # GMS Instrument rekodieren
      GMS.instrument = factor(GMS.instrument, 
                              levels = 1:21, 
                              labels = c("no instrument", "voice", "piano", "guitar", "drums", 
                                         "xylophone", "flute", "oboe", "clarinet", "basoon", 
                                         "trumpet", "trombone", "tuba", "saxophone", "horn", 
                                         "violin", "cello", "viola", "double bass", "harp", "other"))
    )
  
  return(df_person)
}

extract_item_data <- function(file) {
  x <- readRDS(file)
  if(!is.null(x$SLT$items$block) && !is.null(x$SLT$items$correct)) {
    tibble(
      id = x$session$p_id,
      block = as.numeric(x$SLT$items$block),
      correct = as.numeric(x$SLT$items$correct)
    )
  } else {
    NULL
  }
}
is_debug_session <- function(prolific_pid) {
  is.na(prolific_pid) | prolific_pid == ""
}
read_sessions <- function(session_path) {
  dirs <- list.files(session_path, full.names = TRUE)
  purrr::map_dfr(dirs, function(d) {
    data_file <- file.path(d, "data.RDS")
    ts_file   <- file.path(d, "timestamp.RDS")
    if (!file.exists(data_file) || !file.exists(ts_file)) return(NULL)
    
    data_f     <- readRDS(data_file)
    time_stamp <- readRDS(ts_file)
    
    # Extraktion der Prolific PID aus den URL-Parametern der Session (Struktur y)
    p_pid <- if(!is.null(data_f$passive$url_params$PROLIFIC_PID)) {
      data_f$passive$url_params$PROLIFIC_PID
    } else {
      NA_character_
    }
    
    tibble(
      p_id                  = data_f$passive$p_id,
      prolific_pid          = p_pid, # Wichtig für die Filter-Funktion!
      time_started          = data_f$passive$time_started,
      time_last_modified    = time_stamp,
      session_duration_min  = round(as.numeric(
        difftime(time_stamp, data_f$passive$time_started, units = "mins")), 1),
      num_restarts          = data_f$passive$num_restarts,
      tests_finished        = paste(data_f$passive$results %>% as.list() %>% names(), collapse = "; ")
    )
  })
}

data <- map_df(files, extract_person_data)
slt_items_data <- map_df(files, extract_item_data)

# -------------------------------------------------------------------------
# Datenvorbereitung & Variablen-Cluster definieren
# -------------------------------------------------------------------------

cols_deg <- c("DEG.age") 
cols_gms <- names(data)[startsWith(names(data), "GMS.") & map_lgl(data, is.numeric)]
cols_slt <- "SLT.score"

all_numeric_cols <- c(cols_slt, cols_deg, cols_gms)

# -------------------------------------------------------------------------
# Define UI
# -------------------------------------------------------------------------
ui <- fluidPage(
  theme = shinytheme("yeti"),
  navbarPage(
    "Learning By Listening Monitor",
    
    # 1. TAB: Deskriptive Übersicht
    tabPanel(
      "Deskriptive Ergebnisse",
      sidebarPanel(
        h3("Stichproben-Übersicht"),
        wellPanel(
          htmlOutput("sample_info")
        ),
        checkboxInput("exclude_debug", "Nur echte Prolific-Teilnehmer anzeigen", value = TRUE),
        hr(),
        selectInput(
          "construct",
          "Wähle einen Bereich:",
          choices = c(
            "SLT (Statistical Learning)" = "slt",
            "DEG (Demografie)" = "deg",
            "GMS (Gold-MSI)" = "gms"
          )
        )
      ),
      mainPanel(
        h2("Deskriptive Statistiken (Gesamt-Scores)"),
        tableOutput("desc_table"),
        
        # Dynamische Einblendung für die detaillierte SLT Block-Struktur
        conditionalPanel(
          condition = "input.construct == 'slt'",
          hr(),
          h2("SLT Performance aufgeschlüsselt nach Blöcken"),
          p("Hier siehst du die akkumulierte Performance aller Versuchspersonen pro Block:"),
          tableOutput("slt_block_table")
        ),
        
        hr(),
        h2("Verteilungen (Histogramme)"),
        uiOutput("plots_ui")
      )
    ),
    
    # 2. TAB: Multivariate Zusammenhänge
    tabPanel(
      "Multivariate Results",
      sidebarPanel(
        selectInput(
          "covariate1",
          "Wähle Variable X:",
          choices = all_numeric_cols,
          selected = "SLT.score"
        ),
        selectInput(
          "covariate2",
          "Wähle Variable Y:",
          choices = all_numeric_cols,
          selected = if(length(cols_gms) > 0) cols_gms[1] else "SLT.score"
        )
      ),
      mainPanel(
        h2("Streudiagramm mit Regressionslinie"),
        plotOutput("corplot_ui")
      )
    ),
    tabPanel(
      "Prolific-Zuordnung",
      sidebarPanel(
        h3("Prolific-Matching"),
        p("Ordnet die interne psychTestR-ID jeder Versuchsperson der",
          "zugehörigen Prolific-ID (inkl. Study- und Session-ID) zu."),
        downloadButton("download_prolific", "Tabelle als CSV exportieren")
      ),
      mainPanel(
        h2("ID-Zuordnung (psychTestR ID <-> Prolific)"),
        tableOutput("prolific_table"),
        hr(),
        h2("Duplikat-Prüfung"),
        htmlOutput("duplicate_warning"),
        conditionalPanel(
          condition = "output.has_duplicates == true",
          tableOutput("duplicate_table")
        )
      )
    ),
    ### GEÄNDERT START — Sessions inkl. Abbrecher ###
    tabPanel(
      "Sessions (inkl. Abbrecher)",
      mainPanel(
        width = 12,
        p("Zeigt ALLE gestarteten Sessions, auch abgebrochene — basierend auf",
          code("output/sessions/"), "statt nur auf gespeicherten Ergebnisdateien."),
        DT::DTOutput("session_table")
      )
    )
    ### GEÄNDERT ENDE ###
  )
)

# -------------------------------------------------------------------------
# Define Server
# -------------------------------------------------------------------------
server <- function(input, output, session) {
  live_data <- shiny::reactivePoll(
    intervalMillis = 5000,
    session = session,
    checkFunc = function() {
      fs <- list.files(path, pattern = "\\.rds$", full.names = TRUE, recursive = TRUE)
      if (length(fs) == 0) return("")
      paste(length(fs), max(file.info(fs)$mtime, na.rm = TRUE))
    },
    valueFunc = function() {
      fs <- list.files(path, pattern = "\\.rds$", full.names = TRUE, recursive = TRUE)
      list(
        person = purrr::map_df(fs, extract_person_data),
        items  = purrr::map_df(fs, extract_item_data)
      )
    }
  )
  ### GEÄNDERT START — reaktive Session-Daten inkl. Abbrecher ###
  session_path <- stringr::str_replace(path, "results", "sessions")
  
  live_sessions <- shiny::reactivePoll(
    intervalMillis = 5000,
    session = session,
    checkFunc = function() {
      ds <- list.files(session_path, full.names = TRUE)
      if (length(ds) == 0) return("")
      paste(length(ds), max(file.info(ds)$mtime, na.rm = TRUE))
    },
    valueFunc = function() read_sessions(session_path)
  )
  
  output$session_table <- DT::renderDT({
    finished_ids <- live_data()$person$id
    live_sessions() %>%
      mutate(is_finished = p_id %in% finished_ids) %>%
      arrange(desc(time_started))
  })
  ### GEÄNDERT ENDE ###
  filtered_data <- reactive({
    ld <- live_data()
    if (!isTRUE(input$exclude_debug)) return(ld)
    
    # Expliziter Verweis auf die Spalte innerhalb des person-Tibbles
    real_persons <- ld$person %>% filter(!is_debug_session(prolific_pid))
    
    list(
      person = real_persons,
      items  = ld$items %>% filter(id %in% real_persons$id)
    )
  })
  # 1. Stichproben-Größe (N) tracken
  output$sample_info <- renderText({
    ### GEÄNDERT START ###
    df <- filtered_data()$person
    n_total <- nrow(df)
    
    if ("session.complete" %in% names(df)) {
      n_complete <- sum(df$session.complete == TRUE, na.rm = TRUE)
      n_incomplete <- sum(df$session.complete == FALSE | is.na(df$session.complete), na.rm = TRUE)
    } else {
      n_complete <- sum(complete.cases(df[, c("SLT.score", cols_gms)]), na.rm = TRUE)
      n_incomplete <- n_total - n_complete
    }
    ### GEÄNDERT ENDE ###
    n_with_prolific <- sum(!is.na(df$prolific_pid) & df$prolific_pid != "")
    erste_erhebung  <- format(min(df$session_datetime, na.rm = TRUE), "%d.%m.%Y %H:%M")
    letzte_erhebung <- format(max(df$session_datetime, na.rm = TRUE), "%d.%m.%Y %H:%M")
    paste0(
      "<b>Gesamt-Datensätze (N):</b> ", n_total, "<br>",
      "<span style='color: green;'><b>Vollständig:</b> ", n_complete, "</span><br>",
      "<span style='color: orange;'><b>Unvollständig:</b> ", n_incomplete, "</span>",
      "<span style='color: #337ab7;'><b>Mit Prolific-ID:</b> ", n_with_prolific, "</span>"
    )
  })
  
  # Reaktive Auswahl des Sub-Datensatzes basierend auf Dropdown
  selected_vars <- reactive({
    req(input$construct)
    if (input$construct == "slt") {
      return(cols_slt)
    } else if (input$construct == "deg") {
      return(cols_deg)
    } else {
      return(cols_gms)
    }
  })
  
  # 2. Deskriptive Tabelle für numerische Gesamt-Werte
  output$desc_table <- renderTable({
    vars <- selected_vars()
    req(length(vars) > 0)
    
    filtered_data()$person %>%
      select(all_of(vars)) %>%
      keep(is.numeric) %>% 
      pivot_longer(cols = everything(), names_to = "Variable", values_to = "Wert") %>%
      group_by(Variable) %>%
      summarise(
        N = sum(!is.na(Wert)),
        Mittelwert = mean(Wert, na.rm = TRUE),
        SD = sd(Wert, na.rm = TRUE),
        Min = min(Wert, na.rm = TRUE),
        Max = max(Wert, na.rm = TRUE),
        .groups = 'drop'
      )
  }, digits = 2)
  
  # 3. NEU: Live-Berechnung der Performance pro Block aus 'slt_items_data'
  output$slt_block_table <- renderTable({
    req(input$construct == "slt", nrow(slt_items_data) > 0)
    
    filtered_data()$items %>%
      group_by(block) %>%
      summarise(
        "Geleistete Items (Gesamt)" = n(),
        "Richtige Antworten (Mittelwert)" = round(sum(correct == 1, na.rm = TRUE) / length(unique(id)), 2),
        "Mittlere Korrektheit (%)" = paste0(round((sum(correct == 1, na.rm = TRUE) / n()) * 100, 1), "%"),
        .groups = 'drop'
      ) %>%
      rename("Block-Nummer" = block) %>%
      arrange(`Block-Nummer`)
  }, align = "c")
  
  # 4. Dynamische UI-Generierung für Histogramme
  output$plots_ui <- renderUI({
    vars <- selected_vars()
    req(length(vars) > 0)
    
    plot_output_list <- lapply(vars, function(varname) {
      plotname <- paste0("plot_", varname)
      plotOutput(plotname, height = 260)
    })
    
    tagList(plot_output_list)
  })
  
  # Server-seitiges Rendern der Histogramme
  observe({
    vars <- selected_vars()
    req(length(vars) > 0)
    
    lapply(vars, function(varname) {
      plotname <- paste0("plot_", varname)
      
      output[[plotname]] <- renderPlot({
        df_clean <- filtered_data()$person %>% filter(!is.na(.data[[varname]]))
        
        ggplot(df_clean, aes(x = .data[[varname]])) +
          geom_histogram(bins = 20, fill = "#337ab7", color = "white", alpha = 0.8) +
          labs(
            title = paste("Verteilung von", varname),
            x = varname,
            y = "Häufigkeit"
          ) +
          theme_minimal(base_size = 13)
      })
    })
  })
  
  # 5. Multivariate Resultate: Scatterplot
  output$corplot_ui <- renderPlot({
    req(input$covariate1, input$covariate2)
    
    df_plot <- filtered_data()$person %>%   ### GEÄNDERT ###
      filter(!is.na(.data[[input$covariate1]]), !is.na(.data[[input$covariate2]]))
    
    ggplot(df_plot, aes(x = .data[[input$covariate1]], y = .data[[input$covariate2]])) +
      geom_point(alpha = 0.6, color = "#2c3e50", size = 2.5) +
      geom_smooth(method = "lm", color = "#e74c3c", fill = "#ecf0f1", se = TRUE) +
      labs(
        title = paste("Zusammenhang zwischen", input$covariate1, "und", input$covariate2),
        x = input$covariate1,
        y = input$covariate2
      ) +
      theme_minimal(base_size = 14)
  })
  ### GEÄNDERT START — Prolific-Zuordnungstabelle + CSV-Download ###
  output$prolific_table <- renderTable({
    dup_ids <- duplicate_pids()$prolific_pid
    
    live_data()$person %>%
      arrange(desc(session_datetime)) %>%
      select(id, session_date, session_clock, session_duration_min,
             prolific_pid, prolific_study_id, prolific_session_id,
             session.complete, SLT.score) %>%
      mutate(duplicate = ifelse(prolific_pid %in% dup_ids, "⚠ JA", ""))
  })
  
  output$download_prolific <- downloadHandler(
    filename = function() paste0("prolific_mapping_", Sys.Date(), ".csv"),
    content = function(file) {
      mapping <- live_data()$person %>%
        select(id, prolific_pid, prolific_study_id, prolific_session_id,
               session.complete, SLT.score)
      write.csv(mapping, file, row.names = FALSE)
    }
  )
  ### GEÄNDERT START — Duplikat-Erkennung (Server) ###

  # Reaktive Liste aller Prolific-IDs, die mehr als einmal vorkommen
  # (leere/NA-IDs werden ausgeschlossen, da lokale Testläufe sonst
  #  fälschlich als "Duplikate" markiert würden)
  duplicate_pids <- reactive({
    live_data()$person %>%
      filter(!is.na(prolific_pid), prolific_pid != "") %>%
      count(prolific_pid, name = "n_sessions") %>%
      filter(n_sessions > 1)
  })

  # Flag, das die UI steuert (zeigt Tabelle nur, wenn Duplikate existieren)
  output$has_duplicates <- reactive({
    nrow(duplicate_pids()) > 0
  })
  outputOptions(output, "has_duplicates", suspendWhenHidden = FALSE)

  # Textuelle Warnung über der Tabelle
  output$duplicate_warning <- renderText({
    n_dup <- nrow(duplicate_pids())
    if (n_dup == 0) {
      "<span style='color: green;'><b>Keine Duplikate gefunden.</b></span>"
    } else {
      paste0(
        "<span style='color: red;'><b>Achtung:</b> ", n_dup,
        " Prolific-ID(s) mit mehreren Sessions gefunden. ",
        "Bitte vor der Analyse prüfen (z. B. doppelte Teilnahme, Reload).</span>"
      )
    }
  })

  # Detailtabelle: alle Sessions zu den betroffenen Prolific-IDs
  output$duplicate_table <- renderTable({
    dup_ids <- duplicate_pids()$prolific_pid
    req(length(dup_ids) > 0)
    
    live_data()$person %>%
      filter(prolific_pid %in% dup_ids) %>%
      select(id, session_date, session_clock, session_duration_min,
             prolific_pid, prolific_study_id, prolific_session_id,
             session.complete, SLT.score) %>%
      arrange(prolific_pid, desc(session_datetime))
  })
}

# Create Shiny object
shinyApp(ui = ui, server = server)