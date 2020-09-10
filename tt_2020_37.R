library(tidytuesdayR)
library(tidyverse)

tt <- tidytuesdayR::tt_load(2020, 37)

friends <- tt$friends

main_chars = friends %>%
  count(speaker) %>%
  arrange(desc(n)) %>%
  slice(1:6) %>%
  pull(speaker)

friends_pairs <- friends %>%
  mutate(instance = paste0(season, episode, scene)) %>%
  filter(speaker %in% main_chars) %>%
  spread(key=speaker)

