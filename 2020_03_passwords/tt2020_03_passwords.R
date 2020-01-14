library(tidyverse)
library(tidytext)

# Get the Data

passwords <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-14/passwords.csv')


letter_count<-passwords %>% tidytext::unnest_tokens(.,letter, password, "characters") %>%
  group_by(category, letter) %>%
  summarise(cnt = n(),
            value=median(offline_crack_sec, na.rm=TRUE),
            rank = mean(rank, na.rm = TRUE),
            strength = mean(strength, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(!is.na(category)) %>%
  arrange(category, cnt) %>% #this is where you set up the overall order - you have to know what plot you're going to use first. In this case
  mutate(index = row_number())

ggplot(letter_count , aes(x=index, y=cnt))+
  geom_col(aes(fill=strength))+
  facet_wrap(~category, scales="free")+
  scale_x_continuous(breaks=letter_count$index, labels=letter_count$letter)+
  coord_flip()+
  labs(x="", y="Count of Characters", fill = "average strength")

letter_time<-passwords %>% tidytext::unnest_tokens(.,letter, password, "characters") %>%
  group_by(time_unit, letter) %>%
  summarise(cnt = n(),
            value=median(offline_crack_sec, na.rm=TRUE),
            rank = mean(rank, na.rm = TRUE),
            strength = mean(strength, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(!is.na(time_unit)) %>%
  arrange(time_unit, cnt) %>% #this is where you set up the overall order - you have to know what plot you're going to use first. In this case
  mutate(index = row_number(),
         time_unit = fct_relevel(time_unit, c("seconds", "minutes", "hours", "days", "weeks", "months", "years")))

ggplot(letter_time , aes(x=index, y=cnt))+
  geom_col(aes(fill=strength))+
  facet_wrap(~time_unit, scales="free")+
  scale_x_continuous(breaks=letter_time$index, labels=letter_time$letter)+
  coord_flip()+
  labs(x="", y="Count of Characters", fill = "average strength")+
  theme(axis.text = element_text(size=8))

full_chars<-unique(letter_count$letter)

full_chars[which(!full_chars %in% unique(letter_time$letter[which(letter_time$time_unit !="year")]))]

