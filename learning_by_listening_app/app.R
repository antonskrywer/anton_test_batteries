
# Load R packages
library(shiny)
library(shinythemes)
library(tidyverse)
library(purrr)


path_wm <- "/srv/shiny-server/learning_by_listening/WM/output/results/"
path_mindset <- "/srv/shiny-server/learning_by_listening/mindset/output/results/"

files_wm <- list.files(path_wm, pattern = "\\.rds$", full.names = TRUE)
files_mindset <- list.files(path_mindset, pattern = "\\.rds$", full.names = TRUE)
extract_wm <- function(file) {
  
  x <- readRDS(file)
  
  tibble(
    id = x$session$p_id,
    BDS.score = x$BDS$score,
    JAJ.ability = x$JAJ$ability
  )
}

data_mindset <- map_df(files_wm, extract_wm)
extract_mindset <- function(file) {
  
  x <- readRDS(file)
  
  tibble(
    id = x$id,
    TOM.Incremental = x$TOM$Incremental,
    TOM.Entity = x$TOM$Entity
  )
}

data_wm <- map_df(files_wm, extract_wm)
data_mindset <- map_df(files_mindset, extract_mindset)

data <- left_join(data_wm, data_mindset, by = "id")


cols_wm <- c("BDS.score", "JAJ.ability")

cols_mindset <- c("TOM.Incremental", "TOM.Entity")

# Define UI
ui <- fluidPage(theme = shinytheme("yeti"),
                navbarPage(
                  "Learning By Listening Monitor",
                  tabPanel("Results",
                           sidebarPanel(
                             selectInput(
                               "construct",
                               "Wähle einen Datensatz:",
                               choices = c("WM-Daten" = "wm" ,
                                           "Mindset-Daten" = "tom")
                             ),
                             
                             textInput("txt1", "Gib Deine Matrikelnummer ein:")
                           ),
                           mainPanel(
                             h1("Scores"),
                             tableOutput("table"),
                             uiOutput("plots_ui")
                           )
                           
                  )
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
  all_subset <- reactive({
    if (input$construct == "wm") {
      data %>% select(all_of(cols_wm))
    } else {
      data %>% select(all_of(cols_mindset))
    }
  })
  selected_subset <- reactive({
    df <- selected_rows()
    
    if(input$construct == "wm") {
      df %>% select(all_of(cols_wm))
    } else {
      df %>% select(all_of(cols_mindset))
    }
  })
  output$table <- renderTable({
    selected_subset()
  })
  output$plots_ui <- renderUI({
    df <- all_subset()
    
    plot_output_list <- lapply(names(df), function(varname) {
      plotname <- paste0("plot_", varname)
      plotOutput(plotname, height = 300)
    })
    
    tagList(plot_output_list)
  })
  
  # 5) Generiere die Plots serverseitig
  observe({
    df <- all_subset()
    
    lapply(names(df), function(varname) {
      plotname <- paste0("plot_", varname)
      
      output[[plotname]] <- renderPlot({
        x <- df[[varname]]
        person_data <- selected_rows()
        if (nrow(person_data) > 0 && varname %in% names(person_data)) {
          person_value <- person_data[[varname]]
        } else {
          person_value <- NA
        }

        hist(x,
             main = paste("Histogramm:", varname, "In rot siehst Du Deinen Wert im Vergleich zur Gruppe"),
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