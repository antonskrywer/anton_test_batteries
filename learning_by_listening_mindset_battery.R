library(psyquest)
library(psychTestR)
library(shiny)
library(tidyverse)
#source("./utils.R")
source("./data_raw/general_dict.R")
  
debug <- T

if(debug){
  num_items <- c(BAT = 1, EDT = 1,  MDT = 1, MPT = 1, RAT = 1, JAJ = 1, BDS = 1, PIT = 1, EHI = 2, MUS = 1)  
  take_training <- F
}else{
  #num_items <- c(BAT  = 20, EDT = 18,  MDT = 16, MPT = 20, RAT = 14)
  num_items <- c(BAT  = 18, EDT = 18,  MDT = 15, MPT = 18, JAJ = 7, BDS = 8, PIT = 20, EHI = 24, MUS = 25)
  take_training <- T
} 

problems_info = list(
  de = shiny::tags$span(""),
  de_f = shiny::tags$span(""),
  en = shiny::tags$span("")
)
messagef <- function(...) message(sprintf(...)) 
messagef("Debug is %s, take_training is %s", debug, take_training)

consent_page <- function(dict = general_dict, variant = NULL){
  psychTestR::new_timeline(
    psychTestR::checkbox_page(
      label = "consent",
      prompt = shiny::includeHTML("consent_dgm.html"),
      labels = list(psychTestR::i18n("MPIAE_CONSENT_PROMPT")),
      choices = c("choice1"),
      javascript = "checkboxes = $('input:checkbox'); checkboxes.slice(checkboxes.length - 1, checkboxes.length).click(function() { checkboxes.slice(0, checkboxes.length - 1).prop('checked', '') }); checkboxes.slice(0, checkboxes.length - 1).click(function() { checkboxes.slice(checkboxes.length - 1, checkboxes.length).prop('checked', '') });",
      force_answer = TRUE,
      save_answer = FALSE,
      trigger_button_text = psychTestR::i18n("CONTINUE"),
      failed_validation_message = psychTestR::i18n("PLEASE_CONFIRM_CONSENT")),
    dict = dict)
}


id_page <- function(language= "de"){
  if(language == "de"){
    psychTestR::join(
      psychTestR::text_input_page(
        label = "id",
        prompt = "Bitte gib hier Deine Matrikelnummer als ID ein",
        placeholder = "z.B. 7846378"
      ))
  }
  else{
    psychTestR::join(
      psychTestR::one_button_page(
        shiny::h4("Welcome to the this experiment"),
        button_text = "Continue"
      ))
  }
}





final_page <- function(language = "de"){
  if(language == "de"){
    final_prompt <- shiny::div(shiny::h4("Danke für Deine Teilnahme! Deine Angaben wurden gespeichert. Du kannst das Fenster jetzt schließen"))
    psychTestR::final_page(final_prompt)
  }
  else{
    final_prompt <- shiny::div(shiny::h4("Thank you for participation!"))
    psychTestR::final_page(final_prompt)
    
  }
}



musical_hearing_battery  <- function(title = "Learning By Listening. Mindset Test",
                               documentation = "mindset",
                               admin_password = "mindset",
                               researcher_email = "anton.schreiber@uni-hamburg.de",
                               languages = c("de"),
                               dict = psyquest::psyquest_dict,
                               ...) {
  if (languages[1] %in% c("de", "de_f")) {
    volume_head <- "Lautstärkeeinstellung"
    volume_button <- "Abspielen"
    volume_prompt = "Du solltest nun etwas hören. Bitte stelle eine angenehme Lautstärke ein, bevor Du fortfährst."
  }
  else if (languages[1] == "en") {
    volume_head <- "Volume Calibration"
    volume_prompt = "You should hear some audio playing. Please adjust the volume to a comfortable level before continuing."
    volume_button <- "Click here to play."
  }
  
  elts <- psychTestR::join(
    id_page(),
    psyquest::TOM(),
    psychTestR::elt_save_results_to_disk(complete = T),
    final_page(languages[[1]]))
  psychTestR::make_test(
    elts,
    opt = psychTestR::test_options(title = title,
                                   admin_password = admin_password,
                                   researcher_email = researcher_email,
                                   demo = FALSE,
                                   problems_info = problems_info,
                                   force_p_id_from_url = FALSE,
                                   allow_any_p_id_url  = TRUE,
                                   logo = "https://s3.eu-west-1.amazonaws.com/media.dots.org/img/up-uhh-logo-u-2010-u-png.png",
                                   logo_width = "107px",
                                   logo_height = "auto",
                                   languages = languages,
                                   theme = shinythemes::shinytheme("yeti")))
}

# test_battery <- psychTestR::make_test(
#   join(
#     DEG(),
#     MUS(),
#     TPI(),
#     final_page()
#   )
# )
