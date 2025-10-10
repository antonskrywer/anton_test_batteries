library(psyquest)
library(psychTestR)
library(shiny)
library(EHI)
library(tidyverse)
#source("./utils.R")
source("./data_raw/general_dict.R")
  
debug <- T

if(debug){
  num_items <- c(BAT = 1, EDT = 1,  MDT = 1, MPT = 1, RAT = 1, JAJ = 1, BDS = 1, PIT = 1, EHI = 2)  
  take_training <- F
}else{
  #num_items <- c(BAT  = 20, EDT = 18,  MDT = 16, MPT = 20, RAT = 14)
  num_items <- c(BAT  = 18, EDT = 18,  MDT = 15, MPT = 18, JAJ = 7, BDS = 8, PIT = 20, EHI = 24)
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
      prompt = shiny::includeHTML("consent_schreiber.html"),
      labels = list(psychTestR::i18n("MPIAE_CONSENT_PROMPT")),
      choices = c("choice1"),
      javascript = "checkboxes = $('input:checkbox'); checkboxes.slice(checkboxes.length - 1, checkboxes.length).click(function() { checkboxes.slice(0, checkboxes.length - 1).prop('checked', '') }); checkboxes.slice(0, checkboxes.length - 1).click(function() { checkboxes.slice(checkboxes.length - 1, checkboxes.length).prop('checked', '') });",
      force_answer = TRUE,
      save_answer = FALSE,
      trigger_button_text = psychTestR::i18n("CONTINUE"),
      failed_validation_message = psychTestR::i18n("PLEASE_CONFIRM_CONSENT")),
    dict = dict)
  
}


break_page_gms <- function(language= "de"){
  if(language == "de"){
    psychTestR::join(
      psychTestR::one_button_page(
        shiny::includeHTML("break_page_gms_schreiber.html"),
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

break_page_ehi <- function(language= "de"){
  if(language == "de"){
    psychTestR::join(
      psychTestR::one_button_page(
        shiny::includeHTML("break_page_ehi_schreiber.html"),
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

break_page_mhe <- function(language= "de"){
  if(language == "de"){
    psychTestR::join(
      psychTestR::one_button_page(
        shiny::includeHTML("break_page_mhe_schreiber.html"),
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

landing_page1 <- function(language= "de"){
  if(language == "de"){
    psychTestR::join(
      psychTestR::one_button_page(
        shiny::includeHTML("welcome1_page_schreiber.html"),
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

landing_page2 <- function(language= "de"){
  if(language == "de"){
    psychTestR::join(
      psychTestR::one_button_page(
        shiny::includeHTML("welcome2_page_schreiber.html"),
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


final_page <- function(language = "de"){
  if(language == "de"){
    final_prompt <- shiny::div(shiny::h4("Danke für Deine Teilnahme!"))
    psychTestR::final_page(final_prompt)
  }
  else{
    final_prompt <- shiny::div(shiny::h4("Thank you for participation!"))
    psychTestR::final_page(final_prompt)
    
  }
}

final_page2 <- function(language = "de"){
  if(language == "de"){
    final_prompt <- shiny::includeHTML("final_page_schreiber.html")
    psychTestR::final_page(final_prompt)
  }
  else{
    final_prompt <- shiny::div(shiny::h4("Thank you for participation!"))
    psychTestR::final_page(final_prompt)
    
  }
}
  

schreiber_battery  <- function(title = "Emotion in Musik und Sprache",
                               documentation = "EHI_EDT",
                               admin_password = "EmoHI",
                               researcher_email = "schreibera@stud.hmtm-hannover.de",
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
    psychTestR::new_timeline(
      landing_page1(languages[1]),
      dict = dict
    ),
    landing_page2(),
    consent_page(),
    psychTestR::dropdown_page(
      label = "Klasse",
      prompt = "Wähle deine Klasse aus",
      choices = c("5a", "5b", "5c", "5d", "6a", "6b", "6c", "6d", "6e"),
      save_answer = T,
      next_button_text = "Weiter"
    ),
    psychTestR::volume_calibration_page(
      prompt = shiny::div(
        shiny::h4(volume_head), 
        shiny::p(volume_prompt, style = "margin-left:20%;margin-right:20%;text-align:center")),
      url = "https://s3-eu-west-1.amazonaws.com/media.gold-msi.org/misc/audio/terminator.mp3",
      btn_play_prompt = volume_button, button_text = "Weiter"),
    psyquest::DEG(
      subscales = c("Gender",
                    "Country Formative Years",
                    "Hearing Impairment",
                    "Type of Hearing Impairment",
                    "Age",
                    "First Language",
                    "Country of Residence",
                    "Nationality",
                    "Second Language",
                    "Best Shot"),
      language = "de",
      year_range = c(1950, 2016)),
    EDT::EDT(num_items = num_items[["EDT"]],
             with_welcome = T,
             with_finish = T,
             take_training = take_training[1],
             feedback = NULL),
    break_page_gms(),
    psyquest::GMS(subscales = c(
                  "Abilities",
                  "Absolute Pitch",
                  "Emotions",
                  "General",
                  "Instrument",
                  "Perceptual Abilities",
                  "Singing Abilities",
                  "Start Age")),
    break_page_ehi(),
    #break_page(),
    EHI::EHI(num_items = num_items[["EHI"]],
              with_welcome = T,
              take_training = take_training[1],
              with_finish = T,
              feedback = NULL),
    break_page_mhe(),
    psyquest::MHE(),
    psychTestR::elt_save_results_to_disk(complete = T),
    final_page2(languages[[1]]))
  psychTestR::make_test(
    elts,
    opt = psychTestR::test_options(title = title,
                                   admin_password = admin_password,
                                   researcher_email = researcher_email,
                                   demo = FALSE,
                                   problems_info = problems_info,
                                   force_p_id_from_url = FALSE,
                                   allow_any_p_id_url  = TRUE,
                                   logo = "https://s3.eu-west-1.amazonaws.com/media.dots.org/img/Logo_hmtmh_4c.jpg",
                                   logo_width = "107px",
                                   logo_height = "auto",
                                   languages = languages,
                                   theme = shinythemes::shinytheme("yeti")))
}
