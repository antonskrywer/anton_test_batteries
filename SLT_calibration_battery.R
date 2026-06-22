library(psyquest)
library(psychTestR)
library(shiny)
library(tidyverse)
library(SLT)
#source("./utils.R")
source("./data_raw/general_dict.R")
  
debug <- T

if(debug){
  num_items <- c(BAT = 1, EDT = 1,  MDT = 1, MPT = 1, RAT = 1, JAJ = 1, BDS = 1, PIT = 1, EHI = 2, MUS = 1, SLT = 2)  
  take_training <- F
}else{
  #num_items <- c(BAT  = 20, EDT = 18,  MDT = 16, MPT = 20, RAT = 14)
  num_items <- c(BAT  = 18, EDT = 18,  MDT = 15, MPT = 18, JAJ = 7, BDS = 8, PIT = 20, EHI = 24, MUS = 25, SLT = 24)
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
      prompt = psychTestR::i18n("MPIAE_PROLIFIC_CONSENT_BODY"),
      labels = list(psychTestR::i18n("MPIAE_CONSENT_PROMPT")),
      choices = c("choice1"),
      javascript = "checkboxes = $('input:checkbox'); checkboxes.slice(checkboxes.length - 1, checkboxes.length).click(function() { checkboxes.slice(0, checkboxes.length - 1).prop('checked', '') }); checkboxes.slice(0, checkboxes.length - 1).click(function() { checkboxes.slice(checkboxes.length - 1, checkboxes.length).prop('checked', '') });",
      force_answer = TRUE,
      save_answer = FALSE,
      trigger_button_text = psychTestR::i18n("CONTINUE"),
      failed_validation_message = psychTestR::i18n("PLEASE_CONFIRM_CONSENT")),
    dict = dict)
}


welcome_page <- function(language= "en"){
  if(language == "en"){
    psychTestR::join(
      psychTestR::one_button_page(
        shiny::includeHTML("SLT_calibration_welcome_page.html"),
        button_text = "Continue"
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



final_page <- function(language = "en"){
  if(language == "en"){
    final_prompt <- shiny::div(shiny::h4("Thank you for participating in this study!"))
    psychTestR::final_page(final_prompt)
  }
  else{
    final_prompt <- shiny::div(shiny::h4("Thank you for participation!"))
    psychTestR::final_page(final_prompt)
    
  }
}

### GEÄNDERT START — Prolific-IDs aus URL-Parametern erfassen ###
get_param <- function(params, key) {
  val <- params[[key]]
  if (is.null(val)) NA_character_ else val
}

record_prolific_ids <- function() {
  psychTestR::code_block(function(state, ...) {
    url_params <- psychTestR::get_url_params(state)
    print(url_params)
    psychTestR::save_result(place = state, label = "prolific_pid",
                            value = get_param(url_params, "PROLIFIC_PID"))
    psychTestR::save_result(place = state, label = "prolific_study_id",
                            value = get_param(url_params, "STUDY_ID"))
    psychTestR::save_result(place = state, label = "prolific_session_id",
                            value = get_param(url_params, "SESSION_ID"))
  })
}
### GEÄNDERT ENDE ###


SLT_calibration_battery  <- function(title = "Statistical Learning Test: Melody",
                               documentation = "SLT_calibration",
                               admin_password = "SLT-M",
                               researcher_email = "anton.schreiber@uni-hamburg.de",
                               languages = c("en", "de"),
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
      welcome_page(languages[1]),
      dict = dict
    ),
    record_prolific_ids(),
    consent_page(),
    psychTestR::volume_calibration_page(
      prompt = shiny::div(
        shiny::h4(volume_head),
        shiny::p(volume_prompt, style = "margin-left:20%;margin-right:20%;text-align:center")),
      url = "https://s3-eu-west-1.amazonaws.com/media.gold-msi.org/misc/audio/terminator.mp3",
      btn_play_prompt = volume_button, button_text = "Continue"),
    psyquest::DEG(
      subscales = c("Gender",
                    "Age",
                    "Nationality",
                    "Country Formative Years",
                    "First Language",
                    "Second Language",
                    "Country of Residence",
                    "School Degree"),
      language = languages[1]),
    SLT::SLT(version = 2, with_welcome = T, with_feedback = T, take_training = F, num_items = 2),
    psyquest::GMS(configuration_filepath = "SLT_calibration_GMSI_items.csv"),
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

print(SLT_calibration_battery())