library(psychTestR)
library(htmltools)
library(shiny)

dict_raw <- read.csv2("learning_protocol_dict_utf8.csv")
dict <- i18n_dict$new(dict_raw)




protocol_timeline <- new_timeline(join(
  one_button_page(
    i18n("MLP_welcome_prompt"),
    button_text = i18n("CONTINUE")),
  one_button_page(
    i18n("INFORMED_CONSENT"),
    button_text = i18n("CONTINUE")),
  NAFC_page("filter_id",
            i18n("MLP_0001_prompt"), 
            c(i18n("MLP_YES"), i18n("MLP_NO"))),
            conditional(test = function(state, ...) {
              ans <- answer(state)
              ans == i18n("MLPYES")
            },
            logic = join(text_input_page("get_id",
                  i18n("GET_ID"),
                  placeholder = i18n("ID_PLACEHOLDER")),
            NAFC_page("holiday",
                i18n("MLP_0002_prompt"), 
                c(i18n("MLP_YES"), i18n("MLP_NO"))),
            one_button_page(
              i18n("MLP_0003_prompt"),
              button_text = i18n("CONTINUE")),
            text_input_page("instrument",
                              i18n("MLP_0004_prompt")),
            text_input_page("age",
                            i18n("MLP_0005_prompt")),
            text_input_page("learning_time",
                            i18n("MLP_0006_prompt"),
                            placeholder = i18n("MLP_0006_placeholder")))),
            join(
            one_button_page(
                        body = i18n("MLP_0007_prompt"),
                        button_text = i18n("CONTINUE")),
  final_page("final")
  )),
  dict = dict
)
make_test(protocol_timeline, opt = test_options(title = "Learning Log", admin_password = "test", languages = "de"))

welcome_page <- one_button_page(
  div(
    h1("Lernprotokoll für Instrumentalgruppe"),
    p("Als Mitglied der Instrumentalgruppe sollst Du ein Stück Deiner Wahl auf einem Instrument Deiner Wahl lernen.
Zum Prä-, Mid- oder Post-Test kannst Du die Performance insgesamt bewerten."),
    p(tags$strong("Deine Aufgabe"), "ist, jede Woche ca. 15 Minuten zu üben und danach dieses kurze Lernprotokoll auszufüllen."),
    p(tags$em("Hinweis:"), "Wenn möglich, mache regelmäßig Audio- oder Videoaufnahmen, um später den Übeprozess zu analysieren.")
  ),
  button_text = "Weiter"
)

ethics_page <- one_button_page(
  div(
    h3("Teilnahmevoraussetzungen"),
    p("Deine Angaben werden vollständig", tags$strong("anonym"), "verarbeitet und können nicht auf Deine Person bezogen werden. Die Teilnahme ist komplett", tags$stron("freiwillig"), "und kann jederzeit abgebrochen werden, ohne dass für Dich dadurch Nachteile entstehen."),
    p("Durch das Klicken auf den", tags$em("weiter"), "-Button bestätigst Du, dass Du einverstanden mit den Teilnahmebedingungen bist")
  ),
  button_text = "Weiter"
)

filter_id <- NAFC_page("filter_id",
                       "Füllst Du dieses Protokoll zum ersten Mal aus?",
                       c("Ja", "Nein"))


holiday <- NAFC_page("holiday",
                     "Führst Du gerade einen Zwischentest durch? (Pre-/Mid-/Posttest)",
                     c("Ja", "Nein"))

get_id <- text_input_page(
  label = "id",
  prompt = "Bitte gib Deine Matrikelnummer als ID ein",
  placeholder = "z.B.1045738",
  button_text = "Weiter",
  validate = function(state, ...) {
    x <- answer(state)
    if (is.na(suppressWarnings(as.numeric(x))) ||
        x < 0)
      "Bitte geben Sie die ID korrekt als Ziffern ein"
    else
      TRUE}
)

open_question <- text_input_page(
  label = "OQ",
  prompt = "Gibt es weitere Dinge, die Du kommentieren/protokollieren möchtest (z.B. besondere Lernfort-/Rückschritte; Schwierigkeiten der Performance etc.)?",
  button_text = "Weiter"
)
inst_demographics <- one_button_page(
  div(
    p("Da Du diesen Fragebogen zum ersten Mal ausfüllst, möchten wir einige generelle Informationen über Dich abfragen"),
    p("Diese Angaben musst Du ", tags$strong("nur einmal"), "machen. Wenn Du wieder das Protokoll ausfüllst, wird der Fragebogen kürzer sein.")
  )
)

demographics <- module(
  label = "demographics",
  join(
    text_input_page("instrument",
                    prompt = "Welches Instrument lernst Du?",
                    button_text = "Weiter",
                    validate = function(answer, ...) {
                      if (answer == "")
                        "Bitte fülle diese Frage aus."
                      else 
                        TRUE
                    }),
    text_input_page("piece",
                    prompt = "Welches Stück lernst Du? Gib Titel des Stücks und Name des/der Komponist/in an",
                    button_text = "Weiter",
                    validate = function(answer, ...) {
                      if (answer == "")
                        "Bitte fülle diese Frage aus."
                      else 
                        TRUE
                    })
    # text_input_page("age",
    #                 prompt = "Wie alt ist der/die Schüler/in?",
    #                 button_text = "Weiter",
    #                 validate = function(state, ...) {
    #                   x <- answer(state)
    #                   if (is.na(suppressWarnings(as.numeric(x))) ||
    #                       x < 0)
    #                     "Bitte geben Sie das Alter in Ziffern an"
    #                   else
    #                     TRUE
    #                 }),
    # text_input_page("learning_time",
    #                 prompt = "Seit wann lernt der/die Schüler/in das Instrument?",
    #                 placeholder = "z.B. seit 6 Monaten",
    #                 button_text = "Weiter",
    #                 validate = function(answer, ...) {
    #                   if (answer == "")
    #                     "Bitte füllen Sie diese Frage aus."
    #                   else 
    #                     TRUE
    #                 })
  )
)

break_page_absolute <- one_button_page(
  div(p("Wie bewertest Du folgende Aspekte Deines Instrumentalspiels?"),
      p("Bewerte  die folgenden Fragen in", tags$strong("absoluten"), "Angaben auf einer Skala von 1 bis 10."),
      p("1 = Ich kann kaum einen Ton auf dem Instrument produzieren"),
      p("10 = Ich spiele annähernd wie ein/e Profimusiker/in")),
  button_text = "Weiter"
)

absolute_assessment_general <- NAFC_page(
  label = "absolute_assessment_general",
  prompt = "Gesamteindruck des Instrumentalspiels",
  choices = as.character(c(1:10)),
  arrange_vertically = F
) 

absolute_assessment_melody <- NAFC_page(
  label = "absolute_assessment_melody",
  prompt = "Genauigkeit von Melodie und Harmonik bzw. Intonation",
  choices = as.character(c(1:10)),
  arrange_vertically = F
)

absolute_assessment_rhythm <- NAFC_page(
  label = "absolute_assessment_rhythm",
  prompt = "Genauigkeit von Rhythmus und Tempo",
  choices = as.character(c(1:10)),
  arrange_vertically = F
) 

absolute_assessment_art <- NAFC_page(
  label = "absolute_assessment_art",
  prompt = "Genauigkeit von Artikulation/Tonerzeugung",
  choices = as.character(c(1:10)),
  arrange_vertically = F
) 

absolute_assessment_int <- NAFC_page(
  label = "absolute_assessment_int",
  prompt = "Qualität der Interpretation und des musikalischen Ausdrucks",
  choices = as.character(c(1:10)),
  arrange_vertically = F
) 
absolute_assessment <- module(
  label = "absolute_assessment",
  join(
    break_page_absolute,
    absolute_assessment_general,
    absolute_assessment_melody,
    absolute_assessment_rhythm,
    absolute_assessment_art,
    absolute_assessment_int
  ))

break_page_relative <- one_button_page(
  div(p("Wie bewertest Du Dein Instrumentalspiel ", tags$strong("im Vergleich zur letzten Unterrichtseinheit?")),
      button_text = "Weiter")
)

relative_assessment_general <- NAFC_page(
  label = "relative_assessment_general",
  prompt = "Gesamteindruck des Instrumentalspiels",
  choices = c("Deutlich schlechter", "Etwas schlechter", "Etwa gleich", "Etwas besser", "Deutlich besser"),
  arrange_vertically = F
) 

relative_assessment_melody <- NAFC_page(
  label = "relative_assessment_melody",
  prompt = "Genauigkeit von Melodie und Harmonik bzw. Intonation ",
  choices = c("Deutlich schlechter", "Etwas schlechter", "Etwa gleich", "Etwas besser", "Deutlich besser"),
  arrange_vertically = F
) 

relative_assessment_rhythm <- NAFC_page(
  label = "relative_assessment_rhythm",
  prompt = "Genauigkeit von Rhythmus und Tempo",
  choices = c("Deutlich schlechter", "Etwas schlechter", "Etwa gleich", "Etwas besser", "Deutlich besser"),
  arrange_vertically = F
) 

relative_assessment_art <- NAFC_page(
  label = "relative_assessment_art",
  prompt = "Qualität der Tonerzeugung/Artikulation ",
  choices = c("Deutlich schlechter", "Etwas schlechter", "Etwa gleich", "Etwas besser", "Deutlich besser"),
  arrange_vertically = F
) 

relative_assessment_int <- NAFC_page(
  label = "relative_assessment_int",
  prompt = "Qualität der Interpretation und des musikalischen Ausdrucks",
  choices = c("Deutlich schlechter", "Etwas schlechter", "Etwa gleich", "Etwas besser", "Deutlich besser"),
  arrange_vertically = F
) 


relative_assessment <- module(
  label = "relative_assessment",
  join(
    break_page_relative,
    relative_assessment_general,
    relative_assessment_melody,
    relative_assessment_rhythm,
    relative_assessment_art,
    relative_assessment_int
  ))

break_page_processing_type <- one_button_page(
  "Welche der folgenden Aussagen treffen auf Dein Instrumentalspiel zu?",
  button_text = "Weiter"
)

speed <- NAFC_page(
  label = "speed",
  prompt = "Ich spiele deutlich langsamer als durch das Tempo des Stücks vorgegeben.",
  choices = c("Trifft überhaupt nicht zu", "Trifft eher nicht zu", "Weder noch", "Trifft eher zu", "Trifft voll und ganz zu"))
sight_reading <- NAFC_page(
  label = "sight_reading",
  prompt = "Ich spiele das Stück auswendig.",
  choices = c("Trifft überhaupt nicht zu", "Trifft eher nicht zu", "Weder noch", "Trifft eher zu", "Trifft voll und ganz zu"))

distraction <-NAFC_page(
  label = "distraction",
  prompt = "Beim Spielen führen leichte Ablenkungen zu Fehlern.",
  choices = c("Trifft überhaupt nicht zu", "Trifft eher nicht zu", "Weder noch", "Trifft eher zu", "Trifft voll und ganz zu"))

interpretation <-NAFC_page(
  label = "interpretation",
  prompt = "Ich fokussiere mich beim Spielen mehr auf die musikalische Gestaltung, als darauf, die richtigen Töne zu treffen.",
  choices = c("Trifft überhaupt nicht zu", "Trifft eher nicht zu", "Weder noch", "Trifft eher zu", "Trifft voll und ganz zu"))

processing_type <- module(
  label = "processing_type",
  break_page_processing_type, speed, sight_reading, distraction, interpretation
)

final_page <- final_page(body = "Vielen Dank für das Ausfüllen des Protokolls. Deine Angaben wurden gespeichert. Du kannst das Fenster nun schließen")

protocol <- join(
  welcome_page,
  ethics_page,
  get_id,
  filter_id,
  conditional(test = function(state, ...) {
    ans <- answer(state)
    ans == "Nein"
  },
  logic = join(holiday, 
               conditional(test = function(state, ...) {
                 ans <- answer(state)
                 ans == "Ja"
               },
               logic = join(absolute_assessment, relative_assessment, processing_type, open_question, elt_save_results_to_disk(T), final_page)),
               join(absolute_assessment, relative_assessment, processing_type, open_question, elt_save_results_to_disk(T), final_page))),
  inst_demographics,
  demographics,
  absolute_assessment,
  #relative_assessment,
  processing_type,
  open_question,
  elt_save_results_to_disk(T),
  final_page
)

t1 <- make_test(protocol,
                opt = test_options(
                  title = "Lernprotokoll Learning By Listening",
                  admin_password = "learning",
                  researcher_email = "anton.schreiber@uni-hamburg.de"
                ))
#runApp(t1)
