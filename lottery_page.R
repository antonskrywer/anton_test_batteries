library(psychTestR)

#' New lottery page
#'
#' Creates a lottery page, which is a checkbox page with an added e-mail input
#'
#' @param label (Character scalar) Label for the current page.
#'
#' @param prompt Prompt to be displayed above the response choices.
#' Can be either a character scalar (e.g. "What is 2 + 2?") or an object of
#' class "shiny.tag", e.g. \code{shiny::tags$p("What is 2 + 2?")}.
#'
#' @param choices (Character vector) Choices for the participant.
#' If unnamed, then these values will be used both for checkbox IDs and for
#' checkbox labels.
#' If named, then values will be used for checkbox IDs and names will be used
#' for checkbox labels.
#'
#' @param subprompt (Character scalar) Optional additional text in bold letters
#' below the prompt.
#'
#' @param labels Optional vector of labels for the checkbox choices.
#' If not \code{NULL}, will overwrite the names of \code{choices}.
#' This vector of labels can either be a character vector
#' or a list of Shiny tag objects, e.g. as created by \code{shiny::HTML()}.
#'
#' @param trigger_button_text (Character scalar) Text for the trigger button.
#'
#' @param failed_validation_message (Character scalar) Text to be displayed
#' when validation fails.
#'
#' @param force_answer (Boolean scalar) Require at least one checkbox to be
#' ticked.
#'
#' @param javascript (Character scalar) JavaScript code to be added for
#' controlling checkbox behaviour.
#'
#' @param save_answer (Boolean scalar) Whether or not to save the answer.
#'
#' @param hide_response_ui (Boolean scalar) Whether to begin with the response
#' interface hidden (it can be subsequently made visible through Javascript,
#' using the element ID as set in \code{response_ui_id}.
#' See \code{audio_NAFC_page} for an example.).
#'
#' @param response_ui_id (Character scalar) HTML ID for the response user
#' interface.
#'
#' @inheritParams page
#' @inheritParams make_ui_checkbox
#'
#' @export
lottery_page <-
  function(label,
           prompt,
           choices,
           subprompt = "",
           labels = NULL,
           trigger_button_text = "Continue",
           failed_validation_message = "Choose at least one answer!",
           with_name = F,
           mail_label = "E-mail address:",
           name_label = "Your name:",
           mail_width = "300px",
           force_answer = FALSE,
           javascript = "",
           save_answer = TRUE,
           hide_response_ui = FALSE,
           response_ui_id = "response_ui",
           #on_complete = NULL,
           admin_ui = NULL) {
    stopifnot(
      psychTestR:::is.scalar.character(label),
      is.character(choices) && length(choices) > 0L,
      psychTestR:::is.scalar.character(trigger_button_text),
      psychTestR:::is.scalar.character(failed_validation_message),
      psychTestR:::is.scalar.logical(force_answer),
      psychTestR:::is.scalar.character(javascript)
    )
    ui <- shiny::div(
      psychTestR:::tagify(prompt),
      make_ui_checkbox_mail(
        label,
        choices,
        subprompt = subprompt,
        labels = labels,
        mail_label = mail_label,
        name_label = name_label,
        mail_width = mail_width,
        with_name = with_name,
        trigger_button_text = trigger_button_text,
        javascript = javascript,
        hide = hide_response_ui,
        id = response_ui_id
      )
      
    )
    get_answer <- function(input, ...){
      #browser()
      answer <- character(0)
      if (!is.null(input[[label]])) {
        answer <- input[[label]]
      }
      c(answer, input$mail_address)
    }
    validate <- function(answer, ...){
      #browser()
      missing_email <- length(answer) > 1 && answer[length(answer)] == ""
      if (missing_email  && force_answer) {
        failed_validation_message
      } else {
        TRUE
      }
    }
    on_complete <- function(state, answer, ...){
      #browser()
      answer <- paste(answer, collapse = ";")
      psychTestR::save_result(place = state,
                             label = "lottery",
                             value = answer)
      }
    
    page(
      ui = ui,
      label = label,
      get_answer = get_answer,
      save_answer = F,
      validate = validate,
      on_complete = on_complete,
      final = FALSE,
      admin_ui = admin_ui
    )
  }

#' Make checkboxes
#'
#' Creates HTML code for checkbox response options with an extra text input.
#'
#' @param label (Character scalar) Label for the current page.
#'
#' @param choices (Character vector) Choices for the participant.
#' If unnamed, then these values will be used both for checkbox IDs and for
#' checkbox labels.
#' If named, then values will be used for checkbox IDs and names
#' will be used for checkbox labels.
#'
#' @param subprompt (Character scalar) Optional additional text in bold letters
#' below the prompt.
#'
#' @param labels Optional vector of labels for the NOMC choices.
#' If not \code{NULL}, will overwrite the names of \code{choices}.
#' This vector of labels can either be a character vector
#' or a list of Shiny tag objects, e.g. as created by \code{shiny::HTML()}.
#'
#' @param mail_label Label for the text input.
#'
#' @param mail_width width of the text input
#'
#' @param trigger_button_text (Character scalar) Text for the trigger button.
#'
#' @param javascript (Character scalar) JavaScript code to be added for
#' controlling checkbox behaviour.
#'
#' @param hide (Boolean scalar) Whether the checkboxes should be hidden
#' (possibly to be shown later).
#'
#' @param id (Character scalar) HTML ID for the div containing the checkboxes.
#'
#' @export
make_ui_checkbox_mail <-
  function(label,
           choices,
           subprompt = "",
           labels = NULL,
           mail_label = "E-mail address",
           mail_width = "300px",
           with_name = F,
           name_label = "Your name",
           trigger_button_text = "Continue",
           javascript = "",
           hide = FALSE,
           id = "response_ui") {
    stopifnot(
      is.character(choices) && length(choices) > 0L,
      psychTestR:::is.scalar.logical(hide),
      is.null(labels) ||
        ((is.character(labels) || is.list(labels)) &&
           length(labels) == length(choices))
    )
    if (is.null(labels)) {
      labels <- if (is.null(names(choices))) {
        choices
      } else {
        names(choices)
      }
    }
    labels <-
      purrr::map(labels, function(label)
        shiny::tags$span(style = "font-size: 15px; line-height: 15px;", label))
    
    subprompt_div <- NULL
    if (subprompt != "") {
      subprompt_div <-
        shiny::tags$div(id = id,
                        style = "text-align: center;",
                        shiny::tags$strong(subprompt))
    }
    
    checkboxes_div <-
      shiny::tags$div(
        style = "text-align: left;width:80%",
        if (javascript != "")
          shiny::tags$script(shiny::HTML(javascript)),
        subprompt_div,
        shiny::checkboxGroupInput(label, "",
                                  choiceNames = labels, choiceValues = choices)
      )
    
    shiny::tags$div(
      id = id,
      style = "display: inline-block",
      checkboxes_div,
      if(with_name)       shiny::textInput("name", 
                                           label = name_label,
                                           placeholder = "",
                                           width = mail_width),
      
      shiny::textInput("mail_address", 
                       label = mail_label,
                       placeholder = "",
                       width = mail_width),
      psychTestR::trigger_button("next", trigger_button_text)
    )
  }