# Instrumental-Lernprotokoll (Seminar "Musikalisches Lernen messen")
#
# Selbsteinschätzung des eigenen Instrumentalspiels, abgeleitet aus dem
# Musical Learning Protocol (MLP, psyquest/R/MLP.R, Stand 06.10.2026).
# Eigenständige psychTestR-Batterie: Texte in instrumental_protocol_dict.csv,
# Design (Stufenkarten) in www/.
#
# Unterschiede zum MLP: Matrikelnummer als ID, kein Lehrkraft-Code, keine
# Fragen zu Erstbeurteilung, Ferien, Literaturstufe, Hauptstückwechsel und
# Alter, keine normbezogene Ebene (L2), eine einzige Frage zur Übezeit.
# Pro Item wird L3 (kriterienbezogen) immer gezeigt, L1 (Vergleich zur
# letzten Woche) erst ab dem zweiten Protokoll einer Person.
# Ob es das erste Protokoll ist, ergibt sich aus den gespeicherten
# Ergebnissen zur Matrikelnummer.
#
# Start: shiny::runApp("instrumental_protocol")

library(psychTestR)

# ---- Einstellungen ----
shortscale     <- TRUE        # TRUE: nur Items der Kurzskala, FALSE: alle Items
# Admin-Passwort: steht nicht im Code (öffentliches Repository), sondern kommt
# aus der Umgebungsvariable PROTOCOL_ADMIN_PASSWORD (auf dem Server in
# /home/shiny/.Renviron). Ohne Variable gilt ein zufälliges Passwort, das
# Admin-Panel ist dann nicht erreichbar.
admin_password <- Sys.getenv("PROTOCOL_ADMIN_PASSWORD")
if (admin_password == "") admin_password <- paste(sample(c(letters, 0:9), 24, TRUE), collapse = "")
results_dir    <- file.path("output", "results")

# ---- Dictionary ----
ip_dict_df <- read.csv2("instrumental_protocol_dict.csv", fileEncoding = "UTF-8",
                        stringsAsFactors = FALSE)
# "**...**" im Dictionary -> fett. psychTestR maskiert HTML in Antwort-Labels,
# www/mlp.js wandelt dort <strong> wieder zurück.
ip_dict_df$de <- gsub("\\*\\*(.+?)\\*\\*", "<strong>\\1</strong>", ip_dict_df$de, perl = TRUE)
ip_dict <- i18n_dict$new(ip_dict_df)

# ---- Items ----
# Item-IDs pro Block (wie mlp_item_lists() im MLP)
item_lists <- list(
  general    = c("0003", "0004"),
  technique  = c("0005", "0006", "0007", "0009", "0010", "0011"),
  expression = c("0000", "0001", "0002"),
  automatic  = c("0035", "0036", "0037", "0038"),
  instrument = list(
    Woodwind = c("0012", "0013", "0014", "0015", "0016", "0017"),
    Guitar   = c("0018", "0019", "0020", "0021"),
    Brass    = c("0022", "0023", "0024", "0025"),
    Strings  = c("0026", "0027", "0028", "0029", "0030"),
    Piano    = c("0031", "0032", "0033")
  )
)

# Kurzskalen-Filter: feste Liste der Item-IDs mit shortscale == "T" in
# MLP_Itembank_3Ebenen.xlsx (wie im MLP von Hand nachzuziehen).
shortscale_items <- c(
  "0004", "0011", "0002",               # general / technique / Expression
  "0012", "0013", "0014",               # woodwind
  "0018", "0019", "0021",               # guitar
  "0022", "0023", "0025",               # brass
  "0026", "0027", "0028",               # strings
  "0031", "0032", "0033",               # piano
  "0035", "0036", "0037", "0038"        # automatic-controlled
)
apply_shortscale <- function(item_ids) {
  if (!isTRUE(shortscale)) return(item_ids)
  item_ids[item_ids %in% shortscale_items]
}

general_items    <- apply_shortscale(item_lists$general)
technique_items  <- apply_shortscale(item_lists$technique)
expression_items <- apply_shortscale(item_lists$expression)
automatic_items  <- apply_shortscale(item_lists$automatic)
instrument_items <- lapply(item_lists$instrument, apply_shortscale)

# ---- Fortschritt ----
# Jede Seite bekommt ihre Position in der längstmöglichen Abfolge (beide
# Ebenen, größter Instrumentenblock). Übersprungene Seiten (L1, Instrument)
# lassen den Balken nur springen.
n_levels <- 2
n_instr_max <- max(lengths(instrument_items))
pos_general    <- 3                                             # nach Welcome, Instrument, Intro
pos_technique  <- pos_general + n_levels * length(general_items) + 1
pos_expression <- pos_technique + n_levels * length(technique_items) + 1
pos_group      <- pos_expression + n_levels * length(expression_items)
pos_instrument <- pos_group + 2                                 # nach Gruppenwahl + Intro
pos_automatic  <- pos_instrument + n_levels * n_instr_max
pos_practice   <- pos_automatic + n_levels * length(automatic_items)
n_total        <- pos_practice + 1
pct <- function(pos) 100 * pos / n_total

# ---- Seitenbausteine ----
# Hülle für jede Seite: Fortschrittsbalken und Fragetext.
# `type` steuert das Aussehen: "item" (5 nummerierte Stufenkarten),
# "choice" (Karten ohne Nummer), "text" (Eingabefeld), "intro" (Infoseite).
ip_prompt <- function(prompt, progress, type = c("item", "choice", "text", "intro")) {
  type <- match.arg(type)
  classes <- paste(c("mlp-page", paste0("mlp-", type),
                     if (type == "item") "mlp-numbered"), collapse = " ")
  shiny::div(
    class = classes,
    shiny::div(class = "mlp-progress",
               shiny::div(style = sprintf("width: %d%%;", as.integer(round(progress))))),
    shiny::div(class = "mlp-prompt", prompt)
  )
}

# Speichert die Antwort umkodiert (z.B. "CHOICE3" -> 3L)
ip_save_recoded <- function(label, recode) {
  function(answer, state, ...) {
    save_result(place = state, label = label, value = recode(answer))
  }
}
level_number <- function(answer) as.integer(sub("CHOICE", "", answer))

# Ein "Item" -> bis zu 2 NAFC-Seiten: L3 (kriterienbezogen) immer, L1
# (individuumsbezogen) nur ab dem zweiten Protokoll (Local "first_time" == FALSE).
ip_item_pages <- function(item_id, pos) {
  lvls <- c("L3", "L1")
  lapply(seq_along(lvls), function(j) {
    lvl <- lvls[j]
    prefix <- paste0("TMLP_", item_id, "_", lvl)
    page <- NAFC_page(
      label       = prefix,
      prompt      = ip_prompt(i18n(paste0(prefix, "_PROMPT")), pct(pos + j - 1), "item"),
      choices     = paste0("CHOICE", 1:5),
      labels      = sapply(1:5, function(k) i18n(paste0(prefix, "_CHOICE", k))),
      save_answer = FALSE,
      arrange_vertically = TRUE,
      on_complete = ip_save_recoded(prefix, level_number)
    )
    if (lvl == "L1") {
      conditional(
        test  = function(state, ...) !isTRUE(get_local("first_time", state)),
        logic = page
      )
    } else {
      page
    }
  })
}
# conditional() liefert eine Liste von Elementen, eine Seite ist selbst ein
# Element -> auf eine flache Liste von Test-Elementen bringen.
ip_block_pages <- function(item_ids, pos) {
  flat <- list()
  for (k in seq_along(item_ids)) {
    for (el in ip_item_pages(item_ids[k], pos + n_levels * (k - 1))) {
      if (is.null(attr(el, "class"))) flat <- c(flat, el)   # conditional(): einfache Liste
      else flat <- c(flat, list(el))                        # einzelne Seite
    }
  }
  flat
}

# Auswahlseite ohne Stufennummern
# `recode`: Funktion CHOICEk -> gespeicherter Wert (statt "CHOICEk")
ip_choice_page <- function(label, key, n_choices, pos, on_complete = NULL, recode = NULL) {
  NAFC_page(
    label   = label,
    prompt  = ip_prompt(i18n(paste0(key, "_PROMPT")), pct(pos), "choice"),
    choices = paste0("CHOICE", seq_len(n_choices)),
    labels  = lapply(seq_len(n_choices), function(k) i18n(paste0(key, "_CHOICE", k))),
    save_answer = is.null(recode),
    arrange_vertically = TRUE,
    on_complete = if (is.null(recode)) on_complete else {
      save <- ip_save_recoded(label, recode)
      function(answer, state, ...) {
        save(answer, state, ...)
        if (!is.null(on_complete)) on_complete(answer, state, ...)
      }
    }
  )
}
ip_text_page <- function(label, key, pos, validate = NULL) {
  text_input_page(
    label       = label,
    prompt      = ip_prompt(i18n(key), pct(pos), "text"),
    width       = "100%",
    button_text = i18n("CONTINUE"),
    validate    = validate
  )
}
ip_intro_page <- function(key, pos) {
  one_button_page(
    body        = ip_prompt(i18n(key), pct(pos), "intro"),
    button_text = i18n("CONTINUE")
  )
}
ip_instrument_block <- function(group) {
  conditional(
    test  = function(state, ...) identical(get_local("instrument_group", state), group),
    logic = join(
      ip_intro_page("TMLP_0042_INTRO_INSTRUMENT_PROMPT", pos_group + 1),
      ip_block_pages(instrument_items[[group]], pos_instrument)
    )
  )
}

# Zwischenspeichern nach jedem Block: überschreibt die vorige (unvollständige)
# Datei der Sitzung, die vollständige Speicherung am Ende ersetzt sie.
ip_save_partial <- function() elt_save_results_to_disk(complete = FALSE)

num_validate <- function(answer, ...) {
  if (grepl("^[0-9]+$", trimws(answer))) TRUE
  else "Bitte eine Zahl eingeben."
}

# Erstes Protokoll dieser Person? Gezählt werden nur vollständig gespeicherte
# Ergebnisse früherer Sitzungen mit derselben Matrikelnummer.
ip_is_first_time <- function(p_id) {
  files <- list.files(results_dir, pattern = "\\.rds$")
  done  <- grepl(paste0("&p_id=", p_id, "&"), files, fixed = TRUE) &
           grepl("complete=true", files, fixed = TRUE)
  !any(done)
}

# ---- Timeline ----
elts <- join(
  new_timeline(
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
    dict = ip_dict
  ),

  new_timeline(
    join(
      begin_module(label = "instrumental_protocol"),

      # Erstes Protokoll ja/nein -> steuert L1, die Instruktion und die
      # Instrument-Frage
      code_block(function(state, ...) {
        first <- ip_is_first_time(p_id(state))
        set_local("first_time", first, state)
        save_result(place = state, label = "firsttime", value = if (first) "yes" else "no")
      }),

      # 1) Welcome
      ip_intro_page("TMLP_0001_PROMPT", 0),

      # 2) Instruktion und Instrument, nur beim ersten Protokoll
      conditional(
        test  = function(state, ...) isTRUE(get_local("first_time", state)),
        logic = join(
          ip_intro_page("INSTRUCTION", 0),
          ip_text_page("instrument", "TMLP_INSTRUMENT_PROMPT", 1)
        )
      ),
      ip_save_partial(),

      # general
      ip_intro_page("TMLP_0039_INTRO_OVERALL_PROMPT", pos_general - 1),
      ip_block_pages(general_items, pos_general),
      ip_save_partial(),

      # technique
      ip_intro_page("TMLP_0040_INTRO_TECHNIQUE_PROMPT", pos_technique - 1),
      ip_block_pages(technique_items, pos_technique),
      ip_save_partial(),

      # expression
      ip_intro_page("TMLP_0041_INTRO_EXPRESSION_PROMPT", pos_expression - 1),
      ip_block_pages(expression_items, pos_expression),
      ip_save_partial(),

      # Instrumentengruppe
      ip_choice_page("instrument_group", "TMLP_INSTRUMENT_GROUP", 5, pos_group,
                     recode = function(answer) {
                       c(CHOICE1 = "woodwind", CHOICE2 = "brass", CHOICE3 = "piano",
                         CHOICE4 = "guitar", CHOICE5 = "strings")[[answer]]
                     },
                     on_complete = function(answer, state, ...) {
                       group <- c(CHOICE1 = "Woodwind", CHOICE2 = "Brass", CHOICE3 = "Piano",
                                  CHOICE4 = "Guitar", CHOICE5 = "Strings")[[answer]]
                       set_local("instrument_group", group, state)
                     }),

      # instrumentenspezifische Items (nur die gewählte Gruppe)
      ip_instrument_block("Woodwind"),
      ip_instrument_block("Brass"),
      ip_instrument_block("Piano"),
      ip_instrument_block("Guitar"),
      ip_instrument_block("Strings"),
      ip_save_partial(),

      # automatic-controlled
      ip_block_pages(automatic_items, pos_automatic),
      ip_save_partial(),

      # Übezeit der Woche, Minuten mit Validierung
      ip_text_page("practice_week_min", "TMLP_PRACTICE_PROMPT", pos_practice, validate = num_validate),

      elt_save_results_to_disk(complete = TRUE),
      end_module()
    ),
    dict = ip_dict
  ),

  new_timeline(
    final_page(shiny::p(i18n("RESULTS_SAVED"), i18n("CLOSE_BROWSER"))),
    dict = ip_dict
  )
)

make_test(
  elts,
  opt = test_options(
    title              = ip_dict$translate("TMLP_0000_PROMPT", "de"),
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
