library(spotifyr)
library(lubridate)
library(tidyverse)
library(ggfortify)
library('caret')

Sys.setenv(SPOTIFY_CLIENT_ID = 'e4ec23b2bada4263b05944323785c8ca')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '4dbb404760984bf2b98ff3c2ae1e6328')

playlists<-data.frame(name = c('sleep',
                               'floating through space',
                               'birds in the forest'),
                      playlist_id = c('37i9dQZF1DWZd79rJ6a7lp',
                                      '37i9dQZF1DX1n9whBbBKoL',
                                      '37i9dQZF1DWVEt8B7a1H1M')
)
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

#assist for plotting pca https://cran.r-project.org/web/packages/ggfortify/vignettes/plot_pca.html
#
pca<-prcomp(rec_features %>% select_if(is.numeric), scale=TRUE, center=TRUE)

zzzz<-get_playlist_audio_features(username = 'spotify',
                            playlist_uris = playlists$playlist_id) %>%
  unnest_wider(track.artists) %>%
  janitor::clean_names()

zz_df<-zzzz %>% select_if(is.numeric) %>%
  bind_cols(name=zzzz$playlist_name )

zz_index<-createDataPartition(y = zz_df$name,times = 1,p = .8,list = FALSE)
zz_train<-zz_df[zz_index,]
zz_test<-zz_df[-zz_index,]
zz_control<-trainControl(method="rf", number=10)



pca_list<-prcomp(zz_df %>% select(-name))

autoplot(pca_list$rotation)


zz_rf<-train(name~., data=zz_train,method="rf")
zz_pred<-predict(zz_rf,zz_test)
zz_rf$confusion
zz_rf$importance

