library(tidyverse)

general_dict_raw <- readxl::read_xlsx("data_raw/MSA_short_dict.xlsx", trim_ws = T)
general_dict <- general_dict_raw %>% psychTestR::i18n_dict$new()
