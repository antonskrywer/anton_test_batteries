library(tidyverse)

general_dict_raw <- readxl::read_xlsx("data_raw/general_dict.xlsx", trim_ws = T) %>%  mutate(de_f = de)
general_dict <- general_dict_raw %>% psychTestR::i18n_dict$new()
