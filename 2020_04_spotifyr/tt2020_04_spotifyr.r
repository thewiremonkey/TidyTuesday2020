library(spotifyr)
library(lubridate)
library(tidyverse)

Sys.setenv(SPOTIFY_CLIENT_ID = 'e4ec23b2bada4263b05944323785c8ca')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '4dbb404760984bf2b98ff3c2ae1e6328')

seeds<-data.frame(name = c('Johnny Cash',
                           'David Bowie',
                           'Monks of the Abbey of Notre Dame',
                           'Missy Elliot',
                           'Edith Piaf'),
                  artist_id = c('6kACVPfCOnqzgfEF5ryl0x',
                                '0oSGxfWSnnOXhD2fKuz2Gy',
                                '3ePHoPWDrxEt93wK19jxi0',
                                '2wIVse2owClT7go1WT98tk',
                                '1WPcVNert9hn7mHsPKDn7j'))



scopes<-c("user-library-read","user-library-modify","playlist-read-private","playlist-modify-public","playlist-modify-private","playlist-read-collaborative","user-read-recently-played","user-top-read","user-read-private","user-read-email","user-read-birthdate","streaming","user-modify-playback-state","user-read-currently-playing","user-read-playback-state","user-follow-modify","user-follow-read")

get_spotify_authorization_code(client_id = Sys.getenv("SPOTIFY_CLIENT_ID"),
                               client_secret = Sys.getenv("SPOTIFY_CLIENT_SECRET"),
                               scope = c("user-library-read" ,
                                         "playlist-read-private",
                                         "user-top-read",
                                         "user-read-email" ))

access_token <- get_spotify_access_token()


my_recs<-get_recommendations(limit=100, market = "US", seed_artists = seeds$artist_id) %>%
  unnest_wider(.,artists, names_repair = "universal") %>%
  janitor::clean_names()

rec_features<-get_track_audio_features( my_recs$id_11) %>%
  left_join(my_recs %>% select(id_11, name_3, name_14), by=c("id" = "id_11"))

pca<-prcomp(rec_features %>% select_if(is.numeric), scale=TRUE, center=TRUE)

summary(pca)

