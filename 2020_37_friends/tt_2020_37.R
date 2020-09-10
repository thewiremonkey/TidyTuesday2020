library(tidytuesdayR)
library(tidyverse)
library(magick)

# setwd("./2020_37_friends")
tt <- tidytuesdayR::tt_load(2020, 37)

#pull the friends data
friends <- tt$friends%>%
  mutate(instance = paste(season, episode, scene, sep = "_"))

#get the main 6 characters
main_chars = friends %>%
  count(speaker) %>%
  arrange(desc(n)) %>%
  slice(1:6) %>%
  pull(speaker)

#filter to include only rows including text uttered by one of the main characters
main_friends = friends %>%
  filter(speaker %in% main_chars)

#not technically a matrix
#determine which characters are in a given scene if they have spoken an utterance. Their appearance is logical, we don't care how much they've spoken
main_matrix = main_friends %>%
  mutate(in_scene = 1) %>%
  spread(key=speaker, value = in_scene) %>%
  group_by(season,episode,scene) %>%
  summarise_at(.vars = vars(all_of(main_chars)), sum, na.rm = TRUE) %>%
  mutate_at(.vars = vars(all_of(main_chars)),.funs = function(x){ifelse(x == 0, 0, 1)})

#generate corrplots
for(season in unique(main_matrix$season)){
  str_season = str_pad(season,2,"left",0)
  fname = paste0(getwd(),"/",str_season, ".png")
  print(fname)
  png(units = "in", height=8, width=6,res = 300, file=fname)
  corrplot::corrplot(cor(main_matrix[which(main_matrix$season == season),4:9]),diag = FALSE,type = "lower",title = paste("season",season),tl.cex = 1,mar=c(0,0,4,0))
  dev.off()
  }

## assist for combining multiple non-ggplot images into a gif: https://stackoverflow.com/questions/56389470/convert-multiple-png-to-gif-as-an-animation-in-r
list.files(path=getwd(), pattern = '*.png', full.names = TRUE) %>%
  image_read() %>% # reads each path file
  image_join() %>% # joins image
  image_animate(fps=1) %>% # animates, can opt for number of loops
  image_write("Friends.gif") # write to current dir

#function to create pairs of speakers - didn't end up using this for #tidytuesday
get_pairs<-function(friend){
  instances <- main_friends %>%
    filter(speaker == friend) %>%
    select(instance) %>%
    distinct() %>%
    pull(instance)

  df <- main_friends %>%
    filter(instance %in% instances) %>%
    select(instance,season, episode, scene, speaker) %>%
    distinct() %>%
    count(season, speaker) %>%
    mutate(friend1 = friend) %>%
    rename(friend2 = speaker) %>%
    select(friend1, friend2, n, season) %>%
    filter(friend1 != friend2)

  return(df)
}

#didn't end up doing anything with this
friend_pairs = map_df(main_chars, ~get_pairs(.x)) %>%
  mutate(grp = paste0(season,pmin(friend1, friend2), pmax(friend1, friend2))) %>%
  group_by(grp) %>%
  slice(1) %>%
  ungroup()


