# ============================================================
# app.R — SLT_calibration Monitor
# Struktur nach Vorbild Klaus Frielers longgold_monitor /
# old_monitor_app.R, erweitert um Prolific-Matching,
# Dropout-Tracking und Duplikat-Prüfung.
# ============================================================

library(shiny)
library(shinythemes)
library(DT)

source("analysis.R")
source("plot_util.R")

# ------------------------------------------------------------
# Variablen-Metadaten für Univariate/Bivariate/Netzwerk-Tabs
# Bewusst statisch (statt dynamisch aus den Daten abgeleitet wie
# im alten Monitor) — verhindert das Henne-Ei-Problem beim
# App-Start, wenn noch keine .rds-Dateien existieren.
# ------------------------------------------------------------
var_data <- tibble::tribble(
  ~variable,                  ~type,         ~label,
  "SLT.score",                "numeric",     "SLT Gesamt-Score",
  "session_duration_min",     "numeric",     "Bearbeitungszeit (Min)",
  "DEG.age",                  "numeric",     "Alter (Jahre)",
  "DEG.gender",                "categorial",  "Geschlecht",
  "GMS.general",              "numeric",     "GMS General",
  "GMS.perceptual_abilities", "numeric",     "GMS Perceptual Abilities",
  "GMS.musical_training",     "numeric",     "GMS Musical Training",
  "GMS.active_engagement",    "numeric",     "GMS Active Engagement",
  "GMS.start_age",            "numeric",     "GMS Start Age (Instrument)",
  "GMS.absolute_pitch",       "categorial",  "Absolutes Gehör",
  "GMS.instrument",           "categorial",  "Hauptinstrument"
)

cols_numeric    <- var_data %>% filter(type == "numeric")    %>% pull(variable)
cols_categorial <- var_data %>% filter(type == "categorial") %>% pull(variable)

# ------------------------------------------------------------
# UI
# ------------------------------------------------------------
ui <- fluidPage(
  theme = shinythemes::shinytheme("yeti"),
  navbarPage(
    "SLT Kalibrierung — Monitor",
    
    tabPanel("Übersicht",
             sidebarLayout(
               sidebarPanel(
                 h3("Stichproben-Übersicht"),
                 wellPanel(htmlOutput("sample_info")),
                 checkboxInput("exclude_debug", "Nur echte Prolific-Teilnehmer anzeigen", value = TRUE),
                 checkboxInput("dec_de", "CSV-Export im deutschen Format (Komma statt Punkt)", value = FALSE),
                 downloadButton("download_master", "Gesamtdaten als CSV exportieren")
               ),
               mainPanel(
                 h2("Deskriptive Statistik (numerische Variablen)"),
                 tableOutput("desc_table")
               )
             )
    ),
    
    tabPanel("SLT Performance",
             sidebarLayout(
               sidebarPanel(
                 radioButtons("perf_view", "Aufschlüsselung:",
                              choices = c("Gesamt-Score" = "overall",
                                          "Nach Schwierigkeit (Easy/Medium/Hard)" = "difficulty",
                                          "Nach Block-ID (1–16)" = "block"))
               ),
               mainPanel(
                 tableOutput("perf_table"),
                 plotOutput("perf_plot", height = 400)
               )
             )
    ),
    
    tabPanel("Univariate",
             sidebarLayout(
               sidebarPanel(
                 selectInput("uv_var", "Variable:", choices = setNames(var_data$variable, var_data$label))
               ),
               mainPanel(plotOutput("uv_plot", height = 400))
             )
    ),
    
    tabPanel("Bivariate",
             sidebarLayout(
               sidebarPanel(
                 selectInput("biv_x", "Variable X:", choices = setNames(var_data$variable, var_data$label)),
                 selectInput("biv_y", "Variable Y:", choices = setNames(var_data$variable, var_data$label),
                             selected = "GMS.general"),
                 selectInput("biv_group", "Gruppierung (optional):",
                             choices = c("Keine" = "--", setNames(cols_categorial, cols_categorial)))
               ),
               mainPanel(
                 plotOutput("biv_plot", height = 450),
                 tableOutput("biv_cor")
               )
             )
    ),
    
    tabPanel("Korrelationsnetzwerk",
             sidebarLayout(
               sidebarPanel(
                 selectizeInput("net_vars", "Variablen:", choices = cols_numeric,
                                selected = cols_numeric, multiple = TRUE),
                 selectInput("net_group", "Gruppierung (optional):",
                             choices = c("Keine" = "--", setNames(cols_categorial, cols_categorial))),
                 radioButtons("net_output", "Darstellung:",
                              choices = c("Matrix" = "matrix", "Netzwerk" = "network", "Tabelle" = "text"))
               ),
               mainPanel(
                 conditionalPanel("input.net_output != 'text'", plotOutput("network_plot", height = 500)),
                 conditionalPanel("input.net_output == 'text'", tableOutput("network_table"))
               )
             )
    ),
    
    tabPanel("Prolific-Zuordnung",
             sidebarLayout(
               sidebarPanel(
                 h3("Prolific-Matching"),
                 p("Ordnet die interne psychTestR-ID jeder Versuchsperson der zugehörigen Prolific-ID zu."),
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
             )
    ),
    
    tabPanel("Sessions (inkl. Abbrecher)",
             mainPanel(
               width = 12,
               p("Zeigt ALLE gestarteten Sessions, auch abgebrochene — basierend auf",
                 code("output/sessions/"), "statt nur auf gespeicherten Ergebnisdateien."),
               DT::DTOutput("session_table")
             )
    )
  )
)

# ------------------------------------------------------------
# Server
# ------------------------------------------------------------
server <- function(input, output, session) {
  
  # -- Live-Daten (Personen + Items) -------------------------
  live_data <- shiny::reactivePoll(
    intervalMillis = 5000,
    session = session,
    checkFunc = function() {
      fs <- list.files(result_path, pattern = "\\.rds$", full.names = TRUE, recursive = TRUE)
      if (length(fs) == 0) return("")
      paste(length(fs), max(file.info(fs)$mtime, na.rm = TRUE))
    },
    valueFunc = function() setup_workspace(result_path)
  )
  
  # -- Live-Sessions (inkl. Abbrecher) ------------------------
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
  
  # -- Zentraler, gehärteter Datenzugriff ---------------------
  # ALLE anderen Outputs greifen über filtered_data() zu, nie
  # direkt über live_data() — verhindert den Crash bei 0 .rds-
  # Dateien (0-Spalten-Tibble ohne prolific_pid) an jeder Stelle,
  # nicht nur punktuell.
  filtered_data <- reactive({
    ld <- live_data()
    shiny::validate(
      shiny::need(
        nrow(ld$person) > 0 && "prolific_pid" %in% names(ld$person),
        paste0("Keine .rds-Ergebnisdateien gefunden unter:\n", result_path,
               "\n\nBitte Pfad prüfen (Server vs. Nextcloud-Mac).")
      )
    )
    
    if (!isTRUE(input$exclude_debug)) return(ld)
    
    real_persons <- ld$person %>% filter(!is_debug_session(prolific_pid))
    list(
      person = real_persons,
      items  = ld$items %>% filter(id %in% real_persons$id)
    )
  })
  
  # -- Übersicht ----------------------------------------------
  output$sample_info <- renderText({
    df <- filtered_data()$person
    n_total <- nrow(df)
    
    if ("session.complete" %in% names(df)) {
      n_complete   <- sum(df$session.complete == TRUE, na.rm = TRUE)
      n_incomplete <- n_total - n_complete
    } else {
      n_complete <- NA; n_incomplete <- NA
    }
    
    n_with_prolific <- sum(!is.na(df$prolific_pid) & df$prolific_pid != "")
    erste_erhebung  <- format(min(df$session_datetime, na.rm = TRUE), "%d.%m.%Y %H:%M")
    letzte_erhebung <- format(max(df$session_datetime, na.rm = TRUE), "%d.%m.%Y %H:%M")
    
    paste0(
      "<b>Gesamt-Datensätze (N):</b> ", n_total, "<br>",
      "<span style='color: green;'><b>Vollständig:</b> ", n_complete, "</span><br>",
      "<span style='color: orange;'><b>Unvollständig:</b> ", n_incomplete, "</span><br>",
      "<span style='color: #337ab7;'><b>Mit Prolific-ID:</b> ", n_with_prolific, "</span><br>",
      "<b>Zeitraum:</b> ", erste_erhebung, " – ", letzte_erhebung
    )
  })
  
  output$desc_table <- renderTable({
    filtered_data()$person %>%
      select(dplyr::all_of(cols_numeric)) %>%
      tidyr::pivot_longer(cols = everything(), names_to = "Variable", values_to = "Wert") %>%
      group_by(Variable) %>%
      summarise(
        N = sum(!is.na(Wert)),
        Mittelwert = mean(Wert, na.rm = TRUE),
        SD = sd(Wert, na.rm = TRUE),
        Min = min(Wert, na.rm = TRUE),
        Max = max(Wert, na.rm = TRUE),
        .groups = "drop"
      )
  }, digits = 2)
  
  output$download_master <- downloadHandler(
    filename = function() paste0("SLT_calibration_master_", Sys.Date(), ".csv"),
    content = function(file) {
      sep <- if (isTRUE(input$dec_de)) ";" else ","
      dec <- if (isTRUE(input$dec_de)) "," else "."
      write.table(filtered_data()$person, file, row.names = FALSE, sep = sep, dec = dec, quote = TRUE)
    }
  )
  
  # -- SLT Performance (Gesamt / Schwierigkeit / Block) -------
  output$perf_table <- renderTable({
    fd <- filtered_data()
    view <- input$perf_view
    
    if (view == "overall") {
      req(nrow(fd$person) > 0)
      fd$person %>%
        summarise(N = sum(!is.na(SLT.score)),
                  Mittelwert = round(mean(SLT.score, na.rm = TRUE), 3),
                  SD = round(sd(SLT.score, na.rm = TRUE), 3),
                  Min = min(SLT.score, na.rm = TRUE),
                  Max = max(SLT.score, na.rm = TRUE))
    } else if (view == "difficulty") {
      req(nrow(fd$items) > 0)
      fd$items %>%
        filter(!is.na(difficulty)) %>%
        group_by(difficulty) %>%
        summarise(N_Personen = n_distinct(id),
                  N_Items = n(),
                  Accuracy = round(mean(correct, na.rm = TRUE), 3),
                  .groups = "drop") %>%
        mutate(difficulty = factor(difficulty, levels = c("easy", "medium", "hard"))) %>%
        arrange(difficulty)
    } else {
      req(nrow(fd$items) > 0)
      fd$items %>%
        group_by(block) %>%
        summarise(N_Personen = n_distinct(id),
                  N_Items = n(),
                  Accuracy = round(mean(correct, na.rm = TRUE), 3),
                  .groups = "drop") %>%
        arrange(block)
    }
  }, digits = 3)
  
  output$perf_plot <- renderPlot({
    fd <- filtered_data()
    view <- input$perf_view
    
    if (view == "overall") {
      req(nrow(fd$person) > 0)
      univariate_plot_numeric(fd$person %>% filter(!is.na(SLT.score)), "SLT.score")
    } else if (view == "difficulty") {
      req(nrow(fd$items) > 0)
      plot_data <- fd$items %>%
        filter(!is.na(difficulty)) %>%
        group_by(difficulty) %>%
        summarise(Accuracy = mean(correct, na.rm = TRUE), .groups = "drop") %>%
        mutate(difficulty = factor(difficulty, levels = c("easy", "medium", "hard")))
      ggplot(plot_data, aes(x = difficulty, y = Accuracy)) +
        geom_col(fill = def_colour1) +
        geom_hline(yintercept = 0.5, linetype = "dashed", colour = "indianred") +
        labs(x = "Schwierigkeit", y = "Accuracy") + ylim(0, 1) +
        get_default_theme()
    } else {
      req(nrow(fd$items) > 0)
      plot_data <- fd$items %>%
        group_by(block) %>%
        summarise(Accuracy = mean(correct, na.rm = TRUE), .groups = "drop")
      ggplot(plot_data, aes(x = factor(block), y = Accuracy)) +
        geom_col(fill = def_colour1) +
        geom_hline(yintercept = 0.5, linetype = "dashed", colour = "indianred") +
        labs(x = "Block-ID", y = "Accuracy") + ylim(0, 1) +
        get_default_theme()
    }
  })
  
  # -- Univariate -----------------------------------------------
  output$uv_plot <- renderPlot({
    req(input$uv_var)
    df <- filtered_data()$person %>% filter(!is.na(.data[[input$uv_var]]))
    req(nrow(df) > 0)
    type <- get_var_type(var_data, input$uv_var)
    if (type == "numeric") {
      univariate_plot_numeric(df, input$uv_var)
    } else {
      univariate_plot_categorial(df, input$uv_var, coord_flip = dplyr::n_distinct(df[[input$uv_var]]) > 4)
    }
  })
  
  # -- Bivariate ------------------------------------------------
  output$biv_plot <- renderPlot({
    req(input$biv_x, input$biv_y)
    if (input$biv_x == input$biv_y) return(NULL)
    group_var <- if (input$biv_group == "--") NULL else input$biv_group
    bivariate_plot_auto(filtered_data()$person, input$biv_x, input$biv_y, var_data, group_var = group_var)
  })
  
  output$biv_cor <- renderTable({
    req(input$biv_x, input$biv_y, input$biv_x != input$biv_y)
    type_x <- get_var_type(var_data, input$biv_x)
    type_y <- get_var_type(var_data, input$biv_y)
    req(type_x == "numeric", type_y == "numeric")
    df <- filtered_data()$person %>% filter(!is.na(.data[[input$biv_x]]), !is.na(.data[[input$biv_y]]))
    req(nrow(df) > 2)
    ct <- cor.test(df[[input$biv_x]], df[[input$biv_y]])
    tibble("Korrelation (r)" = round(ct$estimate, 3),
           "p-Wert" = round(ct$p.value, 4),
           "N" = nrow(df))
  })
  
  # -- Korrelationsnetzwerk --------------------------------------
  output$network_plot <- renderPlot({
    req(length(input$net_vars) >= 2)
    req(input$net_output %in% c("matrix", "network"))
    plot_cor_networks_with_grouping(
      filtered_data()$person,
      cor_vars = input$net_vars,
      grouping_var = if (input$net_group == "--") NULL else input$net_group,
      output_type = input$net_output
    )
  })
  
  output$network_table <- renderTable({
    req(length(input$net_vars) >= 2, input$net_output == "text")
    plot_cor_networks_with_grouping(
      filtered_data()$person,
      cor_vars = input$net_vars,
      grouping_var = if (input$net_group == "--") NULL else input$net_group,
      output_type = "text"
    )
  })
  
  # -- Prolific-Zuordnung -----------------------------------------
  duplicate_pids <- reactive({
    filtered_data()$person %>%
      filter(!is.na(prolific_pid), prolific_pid != "") %>%
      dplyr::count(prolific_pid, name = "n_sessions") %>%
      filter(n_sessions > 1)
  })
  
  output$has_duplicates <- reactive({ nrow(duplicate_pids()) > 0 })
  outputOptions(output, "has_duplicates", suspendWhenHidden = FALSE)
  
  output$prolific_table <- renderTable({
    dup_ids <- duplicate_pids()$prolific_pid
    filtered_data()$person %>%
      arrange(desc(session_datetime)) %>%
      select(id, session_date, session_clock, session_duration_min,
             prolific_pid, prolific_study_id, prolific_session_id,
             session.complete, SLT.score) %>%
      mutate(duplicate = ifelse(prolific_pid %in% dup_ids, "⚠ JA", ""))
  })
  
  output$download_prolific <- downloadHandler(
    filename = function() paste0("prolific_mapping_", Sys.Date(), ".csv"),
    content = function(file) {
      sep <- if (isTRUE(input$dec_de)) ";" else ","
      dec <- if (isTRUE(input$dec_de)) "," else "."
      mapping <- filtered_data()$person %>%
        select(id, prolific_pid, prolific_study_id, prolific_session_id,
               session.complete, SLT.score)
      write.table(mapping, file, row.names = FALSE, sep = sep, dec = dec, quote = TRUE)
    }
  )
  
  output$duplicate_warning <- renderText({
    n_dup <- nrow(duplicate_pids())
    if (n_dup == 0) {
      "<span style='color: green;'><b>Keine Duplikate gefunden.</b></span>"
    } else {
      paste0("<span style='color: red;'><b>Achtung:</b> ", n_dup,
             " Prolific-ID(s) mit mehreren Sessions gefunden. ",
             "Bitte vor der Analyse prüfen (z. B. doppelte Teilnahme, Reload).</span>")
    }
  })
  
  output$duplicate_table <- renderTable({
    dup_ids <- duplicate_pids()$prolific_pid
    req(length(dup_ids) > 0)
    filtered_data()$person %>%
      filter(prolific_pid %in% dup_ids) %>%
      select(id, session_date, session_clock, session_duration_min,
             prolific_pid, prolific_study_id, prolific_session_id,
             session.complete, SLT.score) %>%
      arrange(prolific_pid, desc(session_datetime))
  })
}

shinyApp(ui = ui, server = server)