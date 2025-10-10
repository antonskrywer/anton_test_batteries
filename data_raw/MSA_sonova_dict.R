library(tidyverse)


# set WD
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# # general_dict_raw <- readxl::read_xlsx("MSA_long_only_dict.xlsx", trim_ws = T)

general_dict_raw <- readxl::read_xlsx("data_raw/MSA_sonova.xlsx", trim_ws = T)
general_dict <- general_dict_raw %>% psychTestR::i18n_dict$new()
