
# Load R packages
library(shiny)
library(shinythemes)
library(tidyverse)
library(purrr)


# path_wm <- "/srv/shiny-server/learning_by_listening/WM/output/results/"
# path_mindset <- "/srv/shiny-server/learning_by_listening/mindset/output/results/"
# path_miq <- "/srv/shiny-server/learning_by_listening/mindset/output/results/"
path_wm <- "/Users/a_schreiber/Nextcloud/anton_test_batteries/learning_by_listening_app/WM/output/results/"
path_mindset <- "/Users/a_schreiber/Nextcloud/anton_test_batteries/learning_by_listening_app/mindset/output/results/"
path_miq <- "/Users/a_schreiber/Nextcloud/anton_test_batteries/learning_by_listening_app/MIQ/output/results/"

files_wm <- list.files(path_wm, pattern = "\\.rds$", full.names = TRUE)
files_mindset <- list.files(path_mindset, pattern = "\\.rds$", full.names = TRUE)
files_miq <- list.files(path_miq, pattern = "\\.rds$", full.names = TRUE)
extract_wm <- function(file) {
  
  x <- readRDS(file)
  
  tibble(
    id = x$session$p_id,
    BDS.score = x$BDS$score,
    JAJ.ability = x$JAJ$ability
  )
}

extract_mindset <- function(file) {
  
  x <- readRDS(file)
  
  tibble(
    id = x$results$id,
    TOM.Incremental = x$TOM$Incremental,
    TOM.Entity = x$TOM$Entity
  )
}
extract_miq <- function(file) {
  
  x <- readRDS(file)
  
  tibble(
    id = x$results$id,
    MIQ.Ability = x$MIQ$ability
  )
}
data_wm <- map_df(files_wm, extract_wm)
data_mindset <- map_df(files_mindset, extract_mindset)
data_miq <- map_df(files_miq, extract_miq)
df_list <- list(data_wm, data_mindset, data_miq)

data <- reduce(df_list, left_join, by = "id")


cols_wm <- c("BDS.score", "JAJ.ability")

cols_mindset <- c("TOM.Incremental", "TOM.Entity")

cols_miq <- c("MIQ.Ability")

# Define UI
ui <- fluidPage(theme = shinytheme("yeti"),
                navbarPage(
                  "Learning By Listening Monitor",
                  tabPanel("Raw Results",
                           sidebarPanel(
                             selectInput(
                               "construct",
                               "Wähle einen Datensatz:",
                               choices = c("WM-Daten" = "wm" ,
                                           "Mindset-Daten" = "tom",
                                           "Intelligenz-Daten" = "miq")
                             ),
                             
                             textInput("txt1", "Gib Deine Matrikelnummer ein:")
                           ),
                           mainPanel(
                             h1("Scores"),
                             tableOutput("table"),
                             uiOutput("plots_ui")
                           )
                           
                  ),
                  tabPanel("Multivariate Results",
                           sidebarPanel(
                             selectInput("covariate1",
                                         "Wähle eine Variable:",
                                         choices = c("BDS-Score" = "BDS.score" ,
                                                     "JAJ-Score" = "JAJ.ability",
                                                     "Growth-Mindset" = "TOM.Incremental",
                                                     "Fixed-Mindset" = "TOM.Entity",
                                                     "Intelligenz" = "MIQ.Ability")),
                             selectInput("covariate2",
                                         "Wähle eine zweite Variable:",
                                         c("BDS-Score" = "BDS.score" ,
                                           "JAJ-Score" = "JAJ.ability",
                                           "Growth-Mindset" = "TOM.Incremental",
                                           "Fixed-Mindset" = "TOM.Entity",
                                           "Intelligenz" = "MIQ.Ability")),
                           ),
                           mainPanel(
                             h1("Streudiagramm"),
                             plotOutput("corplot_ui")
                           ))
)) # fluidPage


# Define server function  
server <- function(input, output, session) {
  selected_rows <- reactive({
    req(input$txt1)
    id_input <- trimws(as.character(input$txt1))
    
    data %>% 
      mutate(id = trimws(as.character(id))) %>%
      filter(id == id_input)
  })
  
  # all_subset: benutze 'data' (deinen zusammengeführten data frame), nicht 'df'
  all_subset <- reactive({
    req(input$construct)  # sicherstellen, dass Input existiert
    
    if (input$construct == "wm") {
      data %>% select(all_of(cols_wm))
    } else if (input$construct == "tom") {
      data %>% select(all_of(cols_mindset))
    } else {
      data %>% select(all_of(cols_miq))
    }
  })
  
  # entferne browser() im Produktiv-Code
  # browser()
  
  selected_subset <- reactive({
    data_sel <- selected_rows()  # lokaler Name 'data_sel' vermeidet Verwirrung
    
    if (input$construct == "wm") {
      data_sel %>% select(all_of(cols_wm))
    } else if (input$construct == "tom") {
      data_sel %>% select(all_of(cols_mindset))
    } else {
      data_sel %>% select(all_of(cols_miq))
    }
  })
  
  output$table <- renderTable({
    selected_subset()
  })
  
  output$plots_ui <- renderUI({
    df_sub <- all_subset()
    
    plot_output_list <- lapply(names(df_sub), function(varname) {
      plotname <- paste0("plot_", varname)
      plotOutput(plotname, height = 300)
    })
    
    tagList(plot_output_list)
  })
  scatter_data <- reactive({
    req(input$covariate1, input$covariate2)
    data[, c(input$covariate1, input$covariate2)]
  })
  
  ## --- Plot rendern ---
  output$corplot_ui <- renderPlot({
    df <- scatter_data()
    ggplot(df, aes(x = df[[1]], y = df[[2]])) +
      geom_point() +
      geom_smooth(method = "lm")

  })
  
  # 5) Generiere die Plots serverseitig
  observe({
    df_sub <- all_subset()
    
    lapply(names(df_sub), function(varname) {
      plotname <- paste0("plot_", varname)
      
      output[[plotname]] <- renderPlot({
        x <- df_sub[[varname]]
        person_data <- selected_rows()
        if (nrow(person_data) > 0 && varname %in% names(person_data)) {
          person_value <- person_data[[varname]]
        } else {
          person_value <- NA
        }
        
        hist(x,
             main = paste("Histogramm:", varname, "— In rot siehst Du Deinen Wert im Vergleich zur Gruppe"),
             xlab = varname,
             breaks = 20)
        if (!is.na(person_value)) {
          abline(v = person_value, col = "red", lwd = 2)
        }
      })
    })
  })
}


 # server


# Create Shiny object
shinyApp(ui = ui, server = server)