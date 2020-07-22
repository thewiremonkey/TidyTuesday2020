library(tidyverse)
library(tidytuesdayR)
library(emojifont)
library(ggridges)

emojifont::

tt<-tidytuesdayR::tt_load(2020, 30)

ao<-tt$animal_outcomes
ac <- tt$animal_complaints %>%
  janitor::clean_names() %>%
  mutate(month = map(str_split(date_received, " "),`[`,1) %>% unlist(),
         year = map(str_split(date_received, " ") , tail, 1) %>% unlist(),
         month = factor(month, levels = c("January", "February", "March" ,"April", "May", "June", "July", "August", "September", "October", "November","December")))

