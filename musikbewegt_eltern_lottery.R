library(psychTestR)
library(shiny)
source("./lottery_page.R")

online_finish_page <- function(){
  prompt <- shiny::div(
    shiny::h3("Ein großes Dankeschön!"),
    shiny::p(
      "Unter allen Teilnehmenden werden acht Geldpreise ", 
      shiny::tags$b("in Höhe von jeweils 25 € verlost."), 
      "Bitte geben Sie hier Ihre E-Mail-Adresse ein, wenn Sie an der Verlosung teilnehmen möchten. Die E-Mail-Adressen werden getrennt von den Umfragedaten gespeichert.",
      "Wenn Sie nicht daran teilnehmen wollen, schließen Sie einfach den Browser-Tab.",
      "Eine Zuordnung Ihrer Daten ist somit ausgeschlossen. Viel Glück!", 
    style = "text-align:justify;display:block;width:60%;min-width:300px;")
  ) 
  labels <- list(
    shiny::p("Ich will am Gewinnspiel teilnehmen.", style = "text-align:center;width:100%")
  )
  
  #stupid comment
  lottery_page(label = "lottery", 
               prompt = prompt,
               choices = c("i_want_to_take_part"),
               labels = labels,
               mail_label = "E-Mail-Adresse:", 
               #name_label = "Ihr Name",
               with_name = F,
               mail_width = "400px",
               trigger_button_text = "Absenden", 
               force_answer = TRUE,
               failed_validation_message = "Bitte E-Mail-Addresse eingeben.")
}
  
problems_info = list(
  de = shiny::tags$span("")
)


run_lottery <- function(){
  elts <- psychTestR::join(
    online_finish_page(),
    psychTestR::elt_save_results_to_disk(complete = TRUE),
    psychTestR::dropdown_page(label = "contact",
                              prompt = "Dürfen wir Ihre E-Mail Adresse verwenden, um Sie über weitere Studien zum Thema
                              Musikalität im Kindesalter zu kontaktieren? Diese Einwilligung können Sie jederzeit und ohne
                              Angabe von Gründen widerrufen.",
                              choices = c("Ja", "Nein"),
                              next_button_text = "Weiter"),
    psychTestR::dropdown_page(label = "further_information",
                              prompt = "Möchten Sie per E-Mail über die Ergebnisse dieser Studie informiert werden?",
                              choices = c("Ja", "Nein"),
                              next_button_text = "Weiter"),
    psychTestR::final_page(p("Vielen Dank."))
  )
  psychTestR::make_test(
    elts,
    opt = psychTestR::test_options(title = "Musik bewegt – Dich auch?",
                                   admin_password = "conifer",
                                   researcher_email = "kindermusik.studien.prj@ae.mpg.de",
                                   problems_info = "",
                                   logo = "https://s3.eu-west-1.amazonaws.com/media.dots.org/img/mpiae_logo_de_green.png",
                                   logo_width = "107px",
                                   logo_height = "auto"
                                   ))
  
}
#run_lottery()