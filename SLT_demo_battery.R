library(psyquest)
library(psychTestR)
library(shiny)
library(tidyverse)
library(SLT)
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
get_id <- psychTestR::text_input_page(
  label = "id",
  prompt = "Gib bitte Deine ID ein"
)

psychTestR::join(
    id,
    SLT()
  )

id_page <- function(language= "de"){
  if(language == "de"){
    psychTestR::join(
      psychTestR::get_p_id(
        "Bitte geben Sie hier Ihre ID ein folgendem Schema ein: 
        die ersten drei Buchstaben des Mädchennamens der Mutter, den Geburtstag von Ihnen persönlich (nur den Tag) und die letzten zwei Buchstaben Ihres Nachnamens",
        placeholder = "z.B. KLA03ER"
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

landing_page1 <- function(language= "de"){
  if(language == "de"){
    psychTestR::join(
      psychTestR::one_button_page(
        shiny::includeHTML("welcome_page.html"),
        button_text = "Weiter"
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



SLT_demo_battery  <- function(title = "SLT_demo_battery",
                               documentation = "SLT_demo",
                               admin_password = "SLT",
                               researcher_email = "anton.schreiber@uni-hamburg.de",
                               languages = c("de", "en"),
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
    get_id(),
    SLT::SLT(num_items = 20, num_blocks = 3, with_welcome = F))
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
