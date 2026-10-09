# Lernprotokoll 10-Finger-Schreiben (Seminar "Musikalisches Lernen messen")
#
# Wöchentliches Protokoll der Typing-Gruppe, umgesetzt nach der SoSci-Umfrage
# "typing_group". Eigenständige psychTestR-Batterie: Texte in
# typing_protocol_dict.csv, Design (Stufenkarten) in www/.
#
# Unterschiede zur SoSci-Umfrage: kontinuierliche Messung ohne Filterfragen
# (Pre-/Mid-/Post-/Follow-Up-Test), kurzer Hinweis auf den Test bei jedem
# Ausfüllen, eine Frage pro Seite, Zahlenfelder mit Prüfung, alle Fragen außer
# dem Kommentar sind Pflicht. Die Instruktion erscheint nur beim ersten
# Protokoll einer Person (ermittelt aus den gespeicherten Ergebnissen zur
# Matrikelnummer).
#
# Start: shiny::runApp("typing_protocol")

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
tp_dict <- i18n_dict$new(read.csv2("typing_protocol_dict.csv", fileEncoding = "UTF-8",
                                   stringsAsFactors = FALSE))

# ---- Fortschritt ----
n_total <- 7   # Welcome + 6 Fragenseiten
pct <- function(pos) 100 * pos / n_total

# ---- Seitenbausteine ----
# Hülle für jede Seite: Fortschrittsbalken und Fragetext.
# `type` steuert das Aussehen: "item" (nummerierte Stufenkarten, einfarbig),
# "text" (Eingabefeld), "intro" (Infoseite).
tp_prompt <- function(prompt, progress, type = c("item", "text", "intro")) {
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

# Eine Aussage der 7-stufigen Zustimmungsskala (gespeichert als 1-7)
tp_scale_page <- function(label, item_key, pos) {
  NAFC_page(
    label   = label,
    prompt  = tp_prompt(
      shiny::tagList(
        shiny::p(i18n("AUTOMATIC_PROMPT")),
        shiny::p(class = "mlp-statement", i18n(item_key))
      ),
      pct(pos), "item"
    ),
    choices = paste0("CHOICE", 1:7),
    labels  = sapply(1:7, function(k) i18n(paste0("AUTOMATIC_CHOICE", k))),
    save_answer = FALSE,
    arrange_vertically = TRUE,
    on_complete = function(answer, state, ...) {
      save_result(place = state, label = label,
                  value = as.integer(sub("CHOICE", "", answer)))
    }
  )
}

is_whole_number <- function(x) length(x) == 1 && grepl("^[0-9]+$", trimws(x))

# Zahleneingabe (Pflicht, ganze Zahl). Gespeichert wird die Zahl, nicht der Text.
tp_number_page <- function(label, key, pos) {
  text_input_page(
    label       = label,
    prompt      = tp_prompt(
      shiny::tagList(
        shiny::p(i18n(paste0(key, "_PROMPT"))),
        shiny::p(class = "mlp-hint", i18n(paste0(key, "_HINT")))
      ),
      pct(pos), "text"
    ),
    width       = "100%",
    button_text = i18n("CONTINUE"),
    save_answer = FALSE,
    validate    = function(answer, ...) {
      if (is_whole_number(answer)) TRUE else "Bitte eine ganze Zahl eingeben."
    },
    on_complete = function(answer, state, ...) {
      save_result(place = state, label = label, value = as.numeric(trimws(answer)))
    }
  )
}

# Fehlerfrage: eine Seite mit zwei Zahlenfeldern (Halb-richtig, Falsch)
tp_error_page <- function(pos) {
  page(
    ui = shiny::div(
      tp_prompt(i18n("ERROR_PROMPT"), pct(pos), "text"),
      shiny::textInput("errors_half", label = i18n("ERROR_HALF"), width = "100%"),
      shiny::textInput("errors_wrong", label = i18n("ERROR_WRONG"), width = "100%"),
      trigger_button("next", i18n("CONTINUE"))
    ),
    label       = "errors",
    get_answer  = function(input, ...) list(half = input$errors_half, wrong = input$errors_wrong),
    save_answer = FALSE,
    validate    = function(answer, ...) {
      if (is_whole_number(answer$half) && is_whole_number(answer$wrong)) TRUE
      else "Bitte in beide Felder eine ganze Zahl eingeben."
    },
    on_complete = function(answer, state, ...) {
      save_result(place = state, label = "errors_half", value = as.numeric(trimws(answer$half)))
      save_result(place = state, label = "errors_wrong", value = as.numeric(trimws(answer$wrong)))
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

      begin_module(label = "typing_protocol"),

      # Erstes Protokoll ja/nein -> steuert die Instruktion
      code_block(function(state, ...) {
        first <- is_first_time(p_id(state))
        set_local("first_time", first, state)
        save_result(place = state, label = "firsttime", value = if (first) "yes" else "no")
      }),
      # 1) Welcome mit Hinweis auf den Test
      one_button_page(
        body = tp_prompt(
          shiny::tagList(shiny::p(i18n("WELCOME")), shiny::p(i18n("WELCOME_TEST"))),
          pct(0), "intro"
        ),
        button_text = i18n("CONTINUE")
      ),

      # Instruktion, nur beim ersten Protokoll
      conditional(
        test  = function(state, ...) isTRUE(get_local("first_time", state)),
        logic = one_button_page(
          body        = tp_prompt(i18n("INSTRUCTION"), pct(0), "intro"),
          button_text = i18n("CONTINUE")
        )
      ),

      # 2-3) Automatisierung, zwei Aussagen
      tp_scale_page("automatic", "AUTOMATIC_ITEM1", 1),
      tp_scale_page("look_at_keyboard", "AUTOMATIC_ITEM2", 2),

      # 4) Wörter pro Minute
      tp_number_page("words_per_min", "WPM", 3),

      # 5) Fehler
      tp_error_page(4),

      # 6) Übezeit der Woche in Minuten
      tp_number_page("practice_week_min", "PRACTICE", 5),

      # 7) Kommentar, darf leer bleiben
      text_input_page(
        label       = "comment",
        prompt      = tp_prompt(i18n("OPEN_PROMPT"), pct(6), "text"),
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
    dict = tp_dict
  )
)

make_test(
  elts,
  opt = test_options(
    title              = tp_dict$translate("TITLE", "de"),
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
