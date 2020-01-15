library(tidyverse)
library(tidytext)

# Get the Data

passwords <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-14/passwords.csv') %>%
  filter(!is.na(nchar(password)))#the next function kept throwing errors until I filtered out the NAs!!

#break the passwords into pairs of characters - make sure you remove all NAs or this will cause you hours of pain
letter_bi<-passwords %>%
  mutate(pairs = map(password, ~nchar(.x)%%2)) %>%
  mutate(bi_chars = case_when(
    pairs == 0 ~map(password, ~substring(.x, seq(1, nchar(.x, type="c"), 2), seq(2, nchar(.x, type="c"), 2))),
    TRUE ~ map(password, ~substring(.x, seq(1, nchar(.x,type = "c")-1, 2), seq(2, nchar(.x, type = "c"), 2)))
  )) %>%
  group_by(password) %>%
  mutate(bi_chars = paste0(unlist(bi_chars), collapse =  " ")) %>%
  unnest_tokens("word", bi_chars) %>%
  ungroup()

#calculate the tf_idf, get the top 10.  Some will return more than 10 items because of identical tf_idf scores.
#arrange by category and tf_idf and add a row number
bi_count<-letter_bi %>%
  group_by(category, word) %>%
  summarise(avg_crack = mean(offline_crack_sec, na.rm = TRUE),
            count=n(),
            avg_strength = mean(strength)) %>%
  ungroup() %>%
  bind_tf_idf(word, category, count) %>%
  arrange(desc(tf_idf)) %>%
  group_by(category) %>%
  top_n(10, tf_idf) %>%
  arrange(category,tf_idf) %>%
  mutate(index=row_number())

##use the index as the x (flipped to y) and set the scales to free in order to ensure each facet being ordered properly
##remove the axis line and labels, add in the word as the label
ggplot(bi_count, aes(x=index, y= tf_idf))+
  geom_col(aes(fill = log(avg_crack, 10)))+
  geom_text(aes(label = word), nudge_y = 0.01)+
  facet_wrap(~category, scales="free_y")+
  coord_flip()+
  theme(plot.background = element_blank(),
        panel.background = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())+
  labs(x="", y="Term Frequency - Inverse Document Frequency", title="Which Letter Pairs Most Characterize Each Category?", subtitle = "top 10 by tf_idf score", caption = "data: Information Is Beautiful\nviz: @WireMonkey\n#TidyTuesday 2020 Week 3", fill = "log 10 of average offline\ncrack in seconds")


