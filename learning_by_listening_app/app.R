
# Load R packages
library(shiny)
library(shinythemes)
library(tidyverse)

data <- read.csv("data_app/results.csv", stringsAsFactors = FALSE)


# on_server <- grepl("learning_by_listening_app", getwd())

# if(on_server){
#   result_dir <- "../learning_by_listening_app/results"
# } else {
#   result_dir <- "data/from_server"
# }
# 
# 
# setup_workspace <- function(results = "data/from_server"){
#   master <- read_data(results)
#   assign("master", master, globalenv())
#   invisible(master)
# }
# 
# # read_data <- function(result_dir = "data/from_server"){
# #   res_files <- list.files(result_dir, pattern = "*rds", full.names = T)
# #   #browser()
# #   
# #   map_dfr(res_files, function(fname){
# #     #browser()
# #     tmp <- readRDS(fname) %>% as.list()
# #     nms <- tmp %>% names()
# #     if(!("session" %in% names(tmp))){
# #       return(NULL)
# #     }
# #     session <- tmp$session[c("p_id", "time_started", "complete", "current_time", "ability", "score")] %>% as.data.frame()
# #     #bind_cols(session, deg, gms, edt, ehi, mhe, class)
# #   }) %>%
# #     distinct() %>%
# #     as_tibble()
# # }
# read_data <- function(result_dir = "data/from_server") {
#   res_files <- list.files(result_dir, pattern = "*rds", full.names = TRUE)
#   
#   map_dfr(res_files, function(fname) {
#     tmp <- readRDS(fname)
#     
#     # Wenn session fehlt oder NULL → überspringen
#     if (!("session" %in% names(tmp)) || is.null(tmp$session)) {
#       return(NULL)
#     }
#     
#     # Prüfe, ob session die erwarteten Spalten enthält
#     needed <- c("p_id", "time_started", "complete", "current_time", "jaj.ability", "bds.score")
#     have <- intersect(names(tmp$session), needed)
#     
#     # Wenn keine dieser Spalten vorhanden sind → überspringen
#     if (length(have) == 0) {
#       return(NULL)
#     }
#     
#     # Erzeuge sicheres DataFrame: fehlende Spalten als NA ergänzen
#     session <- tmp$session[have]
#     for (missing in setdiff(needed, have)) {
#       session[[missing]] <- NA
#     }
#     
#     session <- as.data.frame(session)
#     
#     # Nur, wenn tatsächlich Zeilen vorhanden
#     if (nrow(session) == 0) {
#       return(NULL)
#     }
#     
#     return(session)
#   }) %>%
#     distinct() %>%
#     as_tibble()
# }
# 
# setup_workspace(result_dir)




# Define UI
ui <- fluidPage(theme = shinytheme("yeti"),
                navbarPage(
                  "Working Memory Monitor",
                  tabPanel("WM Result",
                           sidebarPanel(
                             tags$h3("Matrikelnr."),
                             textInput("txt1", "Deine Matrikelnummer", ""),
                             selectInput(
                               label = "Wähle einen Test aus:",
                               choices = c("WM"),
                               selected = "WM",
                               inputId = "test"
                             ),
                             
                           ), # sidebarPanel
                           mainPanel(
                             h1("Scores"),
                             
                             tableOutput("table"),
                             plotOutput(outputId = "distPlot_jaj"),
                             plotOutput(outputId = "distPlot_bds")
                             
                           ) # mainPanel
                           
                  )
)) # fluidPage


# Define server function  
server <- function(input, output, session) {
  
  # Reaktive Auswahl — filtert die Daten nach der eingegebenen ID
  selected_rows <- reactive({
    req(input$txt1)              # stellt sicher, dass etwas eingegeben wurde
    # Robustheit: Vergleiche als character und entferne führende/folgende Leerzeichen
    id_input <- trimws(as.character(input$txt1))
    data %>%
      mutate(session.p_id = trimws(as.character(session.p_id))) %>%
      filter(session.p_id == id_input)
  })

  output$table <- renderTable({
    rows <- selected_rows()
    scores <- rows %>% select(session.p_id, JAJ.ability, BDS.score)
        if (length(scores) == 1) {
          scores

          } else {
          cat("Mehrere Einträge gefunden. JAJ.ability Werte:\n")
          print(scores)
  }})
  output$distPlot_jaj <- renderPlot({
    rows <- selected_rows()
    scores <- rows %>% select(session.p_id, JAJ.ability, BDS.score)
    y    <- na.omit(data[, 317])
    hist(y, main = "Histogramm des JAJ Tests mit Deinem Ergebnis in rot", breaks = 20)
  wert_person_jaj <- data[data$session.p_id == input$txt1, 316]
  abline(v = wert_person_jaj, col = "red", lwd = 2)
  })
  output$distPlot_bds <- renderPlot({
    rows <- selected_rows()
    scores <- rows %>% select(session.p_id, JAJ.ability, BDS.score)
    x    <- data[, 45]
    hist(x, main = "Histogramm des BDS Tests mit Deinem Ergebnis in rot", breaks = 20)
    wert_person_bds <- data[data$session.p_id == input$txt1, 45]
    abline(v = wert_person_bds, col = "red", lwd = 2)
    })
}

 # server


# Create Shiny object
shinyApp(ui = ui, server = server)