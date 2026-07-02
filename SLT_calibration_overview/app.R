library(shiny)
library(shinythemes)
library(tidyverse)
library(DT)
library(corrr)
#local
data_path <- "C:/Users/a_schreiber/Nextcloud/Promotion/Tests/SLT_analysis_and_publications/SLT_analysis"

#server
#data_path <- "/srv/shiny_server/SLT_calibration/app"

d_long_filtered <- read.csv(file.path(data_path, "d_long_filtered.csv"), stringsAsFactors = FALSE)
d_wide_filtered <- read.csv(file.path(data_path, "d_wide_filtered.csv"), stringsAsFactors = FALSE)

d_long_filtered$time_started <- as.POSIXct(d_long_filtered$time_started)
d_long_filtered$current_time <- as.POSIXct(d_long_filtered$current_time)
d_wide_filtered$session_datetime <- as.POSIXct(d_wide_filtered$session_datetime)

hist_vars <- c("DEG.age", "GMS.general", "GMS.perceptual_abilities",
               "GMS.active_engagement", "GMS.musical_training", "SLT.score")

deg_vars <- names(d_wide_filtered)[startsWith(names(d_wide_filtered), "DEG.")]

block_summary <- d_long_filtered %>%
  group_by(block) %>%
  summarise(
    num_tones = first(num_tones),
    prob = first(prob),
    difficulty = first(difficulty),
    n = n(),
    mean_acc = mean(correct, na.rm = TRUE),
    se = sqrt(mean_acc * (1 - mean_acc) / n),
    ci_lower = mean_acc - 1.96 * se,
    ci_upper = mean_acc + 1.96 * se,
    .groups = "drop"
  ) %>%
  mutate(block_label = sprintf("B%d (tc%d,p%d)", block, num_tones, prob)) %>%
  arrange(block)

block_choices <- setNames(block_summary$block, block_summary$block_label)

N_SAMPLE <- 5

ui <- fluidPage(
  theme = shinytheme("flatly"),
  navbarPage(
    "SLT Kalibrierungsstudie — Monitor",
    
    tabPanel(
      "Verteilungen",
      sidebarLayout(
        sidebarPanel(
          selectInput("hist_var", "Variable:", choices = hist_vars, selected = hist_vars[1]),
          width = 3
        ),
        mainPanel(
          plotOutput("hist_plot"),
          tableOutput("hist_table")
        )
      )
    ),
    
    tabPanel(
      "Korrelationsmatrix",
      mainPanel(
        width = 12,
        plotOutput("corr_plot", height = "650px"),
        DTOutput("corr_table")
      )
    ),
    
    tabPanel(
      "Demografie",
      sidebarLayout(
        sidebarPanel(
          selectInput("deg_var", "Demografische Variable:", choices = deg_vars, selected = deg_vars[1]),
          width = 3
        ),
        mainPanel(
          plotOutput("deg_plot")
        )
      )
    ),
    
    tabPanel(
      "SLT Blocks",
      mainPanel(
        width = 12,
        plotOutput("block_plot", height = "600px"),
        DTOutput("block_table")
      )
    ),
    
    tabPanel(
      "Lernkurven",
      sidebarLayout(
        sidebarPanel(
          selectInput("lc_block", "Block:", choices = block_choices, selected = block_choices[1]),
          radioButtons("lc_method", "Glättungsmethode:",
                       choices = c("loess" = "loess", "glm (binomial)" = "glm"),
                       selected = "loess"),
          actionButton("resample_btn", sprintf("Neue Stichprobe (n = %d)", N_SAMPLE)),
          width = 3
        ),
        mainPanel(
          width = 9,
          h4(sprintf("Individuelle Lernkurven — Zufallsstichprobe (n = %d)", N_SAMPLE)),
          plotOutput("lc_sample_plot", height = "450px"),
          hr(),
          h4("Gemittelte Lernkurve — alle VPn dieses Blocks"),
          plotOutput("lc_all_plot")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  output$hist_plot <- renderPlot({
    df <- d_wide_filtered %>% filter(!is.na(.data[[input$hist_var]]))
    ggplot(df, aes(x = .data[[input$hist_var]])) +
      geom_histogram(bins = 20, fill = "#2c7fb8", color = "white", alpha = 0.85) +
      labs(x = input$hist_var, y = "Häufigkeit", title = paste("Verteilung von", input$hist_var)) +
      theme_minimal(base_size = 14)
  })
  
  output$hist_table <- renderTable({
    df <- d_wide_filtered %>% filter(!is.na(.data[[input$hist_var]]))
    tibble(
      N = nrow(df),
      Mittelwert = mean(df[[input$hist_var]], na.rm = TRUE),
      SD = sd(df[[input$hist_var]], na.rm = TRUE),
      Min = min(df[[input$hist_var]], na.rm = TRUE),
      Max = max(df[[input$hist_var]], na.rm = TRUE)
    )
  }, digits = 2)
  
  corr_long <- reactive({
    d_wide_filtered %>%
      select(all_of(hist_vars)) %>%
      cor(use = "pairwise.complete.obs") %>%
      as.data.frame() %>%
      rownames_to_column("var1") %>%
      pivot_longer(-var1, names_to = "var2", values_to = "r")
  })
  
  output$corr_plot <- renderPlot({
    ggplot(corr_long(), aes(x = var1, y = var2, fill = r)) +
      geom_tile(color = "white") +
      geom_text(aes(label = sprintf("%.2f", r)), size = 5) +
      scale_fill_gradient2(low = "indianred4", mid = "white", high = "skyblue3",
                           midpoint = 0, limits = c(-1, 1)) +
      labs(x = NULL, y = NULL, fill = "r") +
      theme_minimal(base_size = 15) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$corr_table <- renderDT({
    d_wide_filtered %>%
      select(all_of(hist_vars)) %>%
      correlate() %>%
      fashion() %>%
      as_tibble()
  })
  
  output$deg_plot <- renderPlot({
    var <- input$deg_var
    df <- d_wide_filtered %>% filter(!is.na(.data[[var]]))
    if (is.numeric(df[[var]])) {
      ggplot(df, aes(x = .data[[var]])) +
        geom_histogram(bins = 20, fill = "#2c7fb8", color = "white", alpha = 0.85) +
        labs(x = var, y = "Häufigkeit", title = paste("Verteilung von", var)) +
        theme_minimal(base_size = 14)
    } else {
      ggplot(df, aes(x = .data[[var]])) +
        geom_bar(fill = "#2c7fb8", color = "white", alpha = 0.85) +
        labs(x = var, y = "Anzahl", title = paste("Verteilung von", var)) +
        theme_minimal(base_size = 14) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
  })
  
  output$block_plot <- renderPlot({
    ggplot(block_summary, aes(x = reorder(block_label, block), y = mean_acc)) +
      geom_point(size = 3, color = "#2c7fb8") +
      geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, color = "#2c7fb8") +
      geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray50") +
      labs(x = "Block", y = "Mean Accuracy", title = "SLT Accuracy pro Block (95% CI, Wald)") +
      theme_minimal(base_size = 14) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$block_table <- renderDT({
    block_summary %>%
      select(block, block_label, difficulty, n, mean_acc, ci_lower, ci_upper) %>%
      mutate(across(c(mean_acc, ci_lower, ci_upper), ~round(.x, 3)))
  })
  
  block_data <- reactive({
    d_long_filtered %>% filter(block == as.numeric(input$lc_block))
  })
  
  sample_ids <- eventReactive(list(input$resample_btn, input$lc_block), {
    ids <- unique(block_data()$p_id)
    sample(ids, size = min(N_SAMPLE, length(ids)))
  })
  
  smooth_args <- reactive({
    if (input$lc_method == "glm") {
      list(method = "glm", method.args = list(family = "binomial"), se = TRUE)
    } else {
      list(method = "loess", se = TRUE)
    }
  })
  
  output$lc_sample_plot <- renderPlot({
    ids <- sample_ids()
    df <- block_data() %>%
      filter(p_id %in% ids) %>%
      mutate(vp_label = factor(p_id, levels = ids, labels = paste0("VP ", seq_along(ids))))
    
    sa <- smooth_args()
    p <- ggplot(df, aes(x = seq_id, y = correct)) +
      geom_line(alpha = 0.4, color = "#2c7fb8") +
      facet_wrap(~vp_label, nrow = 1) +
      labs(x = "Trial-Position (seq_id)", y = "Korrekt (0/1)",
           title = paste0("Individuelle Lernkurven — Zufallsstichprobe (n = ", N_SAMPLE, "), Block ", input$lc_block)) +
      theme_minimal(base_size = 13) +
      theme(strip.text = element_text(face = "bold")) +
      ylim(-0.05, 1.05)
    
    do.call(geom_smooth, c(list(color = "indianred3"), sa)) %>% { p + . }
  })
  
  output$lc_all_plot <- renderPlot({
    df <- block_data()
    mean_df <- df %>%
      group_by(seq_id) %>%
      summarise(mean_correct = mean(correct, na.rm = TRUE), n = n(), .groups = "drop")
    sa <- smooth_args()
    n_total <- n_distinct(df$p_id)
    p <- ggplot() +
      geom_line(data = mean_df, aes(x = seq_id, y = mean_correct), color = "gray40") +
      geom_point(data = mean_df, aes(x = seq_id, y = mean_correct), color = "gray40", size = 2) +
      labs(x = "Trial-Position (seq_id)", y = "Mittlere Korrektheit",
           title = paste0("Gemittelte Lernkurve — Block ", input$lc_block, " (N = ", n_total, " VPn)")) +
      theme_minimal(base_size = 14) +
      ylim(-0.05, 1.05)
    
    do.call(geom_smooth, c(list(data = df, mapping = aes(x = seq_id, y = correct), color = "indianred3"), sa)) %>%
      { p + . }
  })
}

shinyApp(ui = ui, server = server)