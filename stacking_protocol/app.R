# Stacking-Lernprotokoll (Seminar "Musikalisches Lernen messen")
#
# Wöchentliches Protokoll der Stacking-Gruppe (Cup Stacking), umgesetzt nach
# der SoSci-Umfrage "stacking_group". Eigenständige psychTestR-Batterie:
# Texte in stacking_protocol_dict.csv, Design (Stufenkarten) in www/.
#
# Unterschiede zur SoSci-Umfrage: kontinuierliche Messung ohne Filterfragen
# (Pre-/Mid-/Post-/Follow-Up-Test), Links zu den Videos statt eingebettetem
# Video, eine Frage pro Seite, Zahlenfelder mit Prüfung, alle Fragen außer
# dem Kommentar sind Pflicht. Die Instruktion erscheint nur beim ersten
# Protokoll einer Person (ermittelt aus den gespeicherten Ergebnissen zur
# Matrikelnummer).
#
# Start: shiny::runApp("stacking_protocol")

library(psychTestR)

# ---- Einstellungen ----
# Admin-Passwort: steht nicht im Code (öffentliches Repository), sondern kommt
# aus der Umgebungsvariable PROTOCOL_ADMIN_PASSWORD (auf dem Server in
# /home/shiny/.Renviron). Ohne Variable gilt ein zufälliges Passwort, das
# Admin-Panel ist dann nicht erreichbar.
admin_password <- Sys.getenv("PROTOCOL_ADMIN_PASSWORD")
if (admin_password == "") admin_password <- paste(sample(c(letters, 0:9), 24, TRUE), collapse = "")
results_dir    <- file.path("output", "results")

# ---- Dictionary ----
sp_dict <- i18n_dict$new(read.csv2("stacking_protocol_dict.csv", fileEncoding = "UTF-8",
                                   stringsAsFactors = FALSE))

# ---- Fortschritt ----
n_total <- 6   # Welcome + 5 Fragen
pct <- function(pos) 100 * pos / n_total

# ---- Seitenbausteine ----
# Hülle für jede Seite: Fortschrittsbalken und Fragetext.
# `type` steuert das Aussehen: "item" (nummerierte Stufenkarten, einfarbig),
# "text" (Eingabefeld), "intro" (Infoseite).
sp_prompt <- function(prompt, progress, type = c("item", "text", "intro")) {
  type <- match.arg(type)
  classes <- paste(c("mlp-page", paste0("mlp-", type),
                     if (type == "item") "mlp-numbered mlp-neutral"), collapse = " ")
  shiny::div(
    class = classes,
    shiny::div(class = "mlp-progress",
               shiny::div(style = sprintf("width: %d%%;", as.integer(round(progress))))),
    shiny::div(class = "mlp-prompt", prompt)
  )
}

sp_intro_page <- function(key, pos) {
  one_button_page(
    body        = sp_prompt(i18n(key), pct(pos), "intro"),
    button_text = i18n("CONTINUE")
  )
}

# Zahleneingabe (Pflicht). `decimal`: Kommazahlen erlaubt (Komma oder Punkt).
# Gespeichert wird die Zahl, nicht der eingegebene Text.
sp_number_page <- function(label, key, pos, decimal = FALSE, hint = TRUE) {
  pattern <- if (decimal) "^[0-9]+([.,][0-9]+)?$" else "^[0-9]+$"
  text_input_page(
    label       = label,
    prompt      = sp_prompt(
      shiny::tagList(
        shiny::p(i18n(paste0(key, "_PROMPT"))),
        if (hint) shiny::p(class = "mlp-hint", i18n(paste0(key, "_HINT")))
      ),
      pct(pos), "text"
    ),
    width       = "100%",
    button_text = i18n("CONTINUE"),
    save_answer = FALSE,
    validate    = function(answer, ...) {
      if (grepl(pattern, trimws(answer))) TRUE
      else if (decimal) "Bitte eine Zahl eingeben (z.B. 12,5)."
      else "Bitte eine ganze Zahl eingeben."
    },
    on_complete = function(answer, state, ...) {
      save_result(place = state, label = label,
                  value = as.numeric(sub(",", ".", trimws(answer), fixed = TRUE)))
    }
  )
}

# Erstes Protokoll dieser Person? Gezählt werden nur vollständig gespeicherte
# Ergebnisse früherer Sitzungen mit derselben Matrikelnummer.
is_first_time <- function(p_id) {
  files <- list.files(results_dir, pattern = "\\.rds$")
  done  <- grepl(paste0("&p_id=", p_id, "&"), files, fixed = TRUE) &
           grepl("complete=true", files, fixed = TRUE)
  !any(done)
}

# ---- Timeline ----
elts <- join(
  new_timeline(
    join(
      # Eigene ID-Seite statt get_p_id(): get_p_id() setzt eine gespeicherte
      # Sitzung mit derselben ID fort. Bei wöchentlicher Wiederholung landet man
      # so auf der Schlussseite der Vorwoche, statt ein neues Protokoll zu beginnen.
      text_input_page(
        label       = "p_id",
        prompt      = i18n("ENTER_ID"),
        placeholder = paste(i18n("E.G."), "10492817"),
        button_text = i18n("CONTINUE"),
        save_answer = FALSE,
        validate    = function(answer, ...) {
          if (grepl("^[A-Za-z0-9_]{1,100}$", trimws(answer))) TRUE
          else "Bitte nur Ziffern und Buchstaben eingeben."
        },
        on_complete = function(answer, state, ...) {
          set_p_id <- utils::getFromNamespace("p_id<-", "psychTestR")
          set_p_id(state, trimws(answer))
        }
      ),

      begin_module(label = "stacking_protocol"),

      # Erstes Protokoll ja/nein -> steuert die Instruktion
      code_block(function(state, ...) {
        first <- is_first_time(p_id(state))
        set_local("first_time", first, state)
        save_result(place = state, label = "firsttime", value = if (first) "yes" else "no")
      }),
      # 1) Welcome
      sp_intro_page("WELCOME", 0),

      # Instruktion, nur beim ersten Protokoll
      conditional(
        test  = function(state, ...) isTRUE(get_local("first_time", state)),
        logic = sp_intro_page("INSTRUCTION", 0)
      ),

      # 2) Automatisierung, 7-stufige Zustimmungsskala (gespeichert als 1-7)
      NAFC_page(
        label   = "automatic_controlled",
        prompt  = sp_prompt(
          shiny::tagList(
            shiny::p(i18n("AUTOMATIC_PROMPT")),
            shiny::p(class = "mlp-statement", i18n("AUTOMATIC_ITEM"))
          ),
          pct(1), "item"
        ),
        choices = paste0("CHOICE", 1:7),
        labels  = sapply(1:7, function(k) i18n(paste0("AUTOMATIC_CHOICE", k))),
        save_answer = FALSE,
        arrange_vertically = TRUE,
        on_complete = function(answer, state, ...) {
          save_result(place = state, label = "automatic_controlled",
                      value = as.integer(sub("CHOICE", "", answer)))
        }
      ),

      # 3) Zeit für den Speed-Stack-Cycle in Sekunden
      sp_number_page("time_sec", "TIME", 2, decimal = TRUE),

      # 4) Fehler (umgefallene Cups)
      sp_number_page("errors", "ERROR", 3, hint = FALSE),

      # 5) Übezeit der Woche in Minuten
      sp_number_page("practice_week_min", "PRACTICE", 4),

      # 6) Kommentar, darf leer bleiben
      text_input_page(
        label       = "comment",
        prompt      = sp_prompt(i18n("OPEN_PROMPT"), pct(5), "text"),
        one_line    = FALSE,
        width       = "100%",
        button_text = i18n("CONTINUE")
      ),

      elt_save_results_to_disk(complete = TRUE),
      end_module(),

      final_page(shiny::div(
        shiny::p(shiny::strong(i18n("THANKS"))),
        shiny::p(i18n("RESULTS_SAVED"))
      ))
    ),
    dict = sp_dict
  )
)

make_test(
  elts,
  opt = test_options(
    title              = sp_dict$translate("TITLE", "de"),
    admin_password     = admin_password,
    demo               = FALSE,
    languages          = "de",
    # Keine Sitzungs-Fortsetzung über die URL, siehe ID-Seite
    enable_resume_session = FALSE,
    logo               = "uhh-logo.svg",
    logo_width         = "110px",
    logo_height        = "auto",
    display            = display_options(css = file.path("www", "mlp.css")),
    additional_scripts = file.path("www", c("mlp-standalone.js", "mlp.js"))
  )
)
