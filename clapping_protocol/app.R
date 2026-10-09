# Lernprotokoll Clapping Music (Seminar "Musikalisches Lernen messen")
#
# Wöchentliches Protokoll der Clapping-Music-Gruppe (Steve Reich), umgesetzt
# nach der SoSci-Umfrage "clapping_music_group". Eigenständige
# psychTestR-Batterie: Texte in clapping_protocol_dict.csv, Design in www/.
#
# Unterschiede zur SoSci-Umfrage: kontinuierliche Messung ohne Filterfragen
# (Pre-/Mid-/Post-/Follow-Up-Test), eine Frage pro Seite, alle Fragen außer
# dem Kommentar sind Pflicht, Link zum Video statt eingebettetem Video.
# Die Instruktion erscheint nur beim ersten Protokoll einer Person, die
# relative Bewertung (Vergleich zur letzten Session) erst ab dem zweiten.
# Ob es das erste Protokoll ist, ergibt sich aus den gespeicherten
# Ergebnissen zur Matrikelnummer.
#
# Start: shiny::runApp("clapping_protocol")

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
cp_dict <- i18n_dict$new(read.csv2("clapping_protocol_dict.csv", fileEncoding = "UTF-8",
                                   stringsAsFactors = FALSE))

# ---- Fortschritt ----
# Position in der längstmöglichen Abfolge. Übersprungene Seiten (Instruktion,
# relative Bewertung) lassen den Balken nur springen.
pos_abs      <- 2    # nach Welcome und Instruktion
pos_rel      <- pos_abs + 3
pos_ac       <- pos_rel + 3
pos_practice <- pos_ac + 5
n_total      <- pos_practice + 2
pct <- function(pos) 100 * pos / n_total

# ---- Seitenbausteine ----
# Hülle für jede Seite: Fortschrittsbalken und Fragetext.
# `type` steuert das Aussehen: "item" (nummerierte Stufenkarten in
# Ampelfarben), "neutral" (nummerierte Karten, einfarbig), "scale"
# (Zahlenreihe), "text" (Eingabefeld), "intro" (Infoseite).
cp_prompt <- function(prompt, progress, type = c("item", "neutral", "scale", "text", "intro")) {
  type <- match.arg(type)
  classes <- switch(type,
    item    = "mlp-page mlp-item mlp-numbered",
    neutral = "mlp-page mlp-item mlp-numbered mlp-neutral",
    paste0("mlp-page mlp-", type)
  )
  shiny::div(
    class = classes,
    shiny::div(class = "mlp-progress",
               shiny::div(style = sprintf("width: %d%%;", as.integer(round(progress))))),
    shiny::div(class = "mlp-prompt", prompt)
  )
}

cp_intro_page <- function(key, pos) {
  one_button_page(
    body        = cp_prompt(i18n(key), pct(pos), "intro"),
    button_text = i18n("CONTINUE")
  )
}

# Eine Aussage auf einer n-stufigen Skala (gespeichert als ganze Zahl 1..n).
# `key`: Präfix im Dictionary (<key>_PROMPT, <key>_ITEMk, <key>_CHOICEk).
# Bei type = "scale" sind nur die Endpunkte beschriftet (<key>_MIN/_MAX).
cp_scale_page <- function(label, key, item, n, pos, type) {
  NAFC_page(
    label   = label,
    prompt  = cp_prompt(
      shiny::tagList(
        shiny::p(i18n(paste0(key, "_PROMPT"))),
        shiny::p(class = "mlp-statement", i18n(paste0(key, "_ITEM", item))),
        if (type == "scale") shiny::p(class = "mlp-hint", i18n(paste0(key, "_MIN")),
                                      shiny::br(), i18n(paste0(key, "_MAX")))
      ),
      pct(pos), type
    ),
    choices = paste0("CHOICE", seq_len(n)),
    labels  = if (type == "scale") as.character(seq_len(n))
              else sapply(seq_len(n), function(k) i18n(paste0(key, "_CHOICE", k))),
    save_answer = FALSE,
    arrange_vertically = type != "scale",
    on_complete = function(answer, state, ...) {
      save_result(place = state, label = label,
                  value = as.integer(sub("CHOICE", "", answer)))
    }
  )
}
cp_scale_block <- function(labels, key, n, pos, type) {
  lapply(seq_along(labels), function(i) cp_scale_page(labels[i], key, i, n, pos + i - 1, type))
}

# Erstes Protokoll dieser Person? Gezählt werden nur vollständig gespeicherte
# Ergebnisse früherer Sitzungen mit derselben Matrikelnummer.
cp_is_first_time <- function(p_id) {
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

      begin_module(label = "clapping_protocol"),

      # Erstes Protokoll ja/nein -> steuert Instruktion und relative Bewertung
      code_block(function(state, ...) {
        first <- cp_is_first_time(p_id(state))
        set_local("first_time", first, state)
        save_result(place = state, label = "firsttime", value = if (first) "yes" else "no")
      }),

      # 1) Welcome
      cp_intro_page("WELCOME", 0),

      # 2) Instruktion, nur beim ersten Protokoll
      conditional(
        test  = function(state, ...) isTRUE(get_local("first_time", state)),
        logic = cp_intro_page("INSTRUCTION", 1)
      ),

      # 3-5) Absolute Bewertung, Skala 1-10
      cp_scale_block(c("abs_overall", "abs_individual", "abs_sync"),
                     "ABS", 10, pos_abs, "scale"),

      # 6-8) Relative Bewertung (Vergleich zur letzten Session), ab dem
      #      zweiten Protokoll
      conditional(
        test  = function(state, ...) !isTRUE(get_local("first_time", state)),
        logic = join(cp_scale_block(c("rel_overall", "rel_individual", "rel_sync"),
                                    "REL", 5, pos_rel, "item"))
      ),

      # 9-13) Automatisierung, 7-stufige Zustimmungsskala
      cp_scale_block(c("ac_tempo_slower", "ac_memorized", "ac_distraction",
                       "ac_focus_expression", "ac_automatic"),
                     "AC", 7, pos_ac, "neutral"),

      # 14) Übezeit der Woche in Minuten (Pflicht, ganze Zahl)
      text_input_page(
        label       = "practice_week_min",
        prompt      = cp_prompt(
          shiny::tagList(shiny::p(i18n("PRACTICE_PROMPT")),
                         shiny::p(class = "mlp-hint", i18n("PRACTICE_HINT"))),
          pct(pos_practice), "text"
        ),
        width       = "100%",
        button_text = i18n("CONTINUE"),
        save_answer = FALSE,
        validate    = function(answer, ...) {
          if (grepl("^[0-9]+$", trimws(answer))) TRUE else "Bitte eine ganze Zahl eingeben."
        },
        on_complete = function(answer, state, ...) {
          save_result(place = state, label = "practice_week_min",
                      value = as.numeric(trimws(answer)))
        }
      ),

      # 15) Kommentar, darf leer bleiben
      text_input_page(
        label       = "comment",
        prompt      = cp_prompt(i18n("OPEN_PROMPT"), pct(pos_practice + 1), "text"),
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
    dict = cp_dict
  )
)

make_test(
  elts,
  opt = test_options(
    title              = cp_dict$translate("TITLE", "de"),
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
