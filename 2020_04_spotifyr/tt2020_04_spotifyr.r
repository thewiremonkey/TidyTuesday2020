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

my_recs<-get_recommendations(limit=100, market = "US", seed_artists = seeds$artist_id) %>%
  unnest_wider(.,artists, names_repair = "universal")


scopes<-c("user-library-read","user-library-modify","playlist-read-private","playlist-modify-public","playlist-modify-private","playlist-read-collaborative","user-read-recently-played","user-top-read","user-read-private","user-read-email","user-read-birthdate","streaming","user-modify-playback-state","user-read-currently-playing","user-read-playback-state","user-follow-modify","user-follow-read")

get_spotify_authorization_code(client_id = Sys.getenv("SPOTIFY_CLIENT_ID"),
                               client_secret = Sys.getenv("SPOTIFY_CLIENT_SECRET"),
                               scope = c("user-library-read" ,
                                         "playlist-read-private",
                                         "user-top-read",
                                         "user-read-email" ))

access_token <- get_spotify_access_token()





my_lists<-get_my_playlists(limit = 50,authorization =get_spotify_authorization_code(
  client_id = Sys.getenv("SPOTIFY_CLIENT_ID"),
  client_secret = Sys.getenv("SPOTIFY_CLIENT_SECRET"),
  scope = c("user-library-read",
            "playlist-read-private",
            "user-top-read",
            "user-read-email" )))

#need to do this in order to get artist id
bowie<-get_artist_audio_features('David Bowie')

cash<-get_artist_audio_features('Johnny Cash')

bowie_id<-bowie$artist_id %>% unique()

#create a little function to get all of the albums
get_all_albums<-function(x){
  offset=x*50
 res<-get_artist_albums(bowie_id,market = "US", authorization = access_token,limit = 50, offset = offset)

return(res)
}

#use the function
bowie_albums<-map_df(0:5, ~get_all_albums(.x))%>% filter(!str_detect(name, "E.P."),
                                        album_group !="appears_on",
                                        album_group !="compilation",
                                        total_tracks>4) %>%
  mutate(album_id = id)

get_tracks<-function(x){
  get_album_tracks(x,limit=50, authorization = access_token) %>%
    mutate(album_id = x,
           track_id = id)
}

bowie_tracks<-map_df(bowie_albums$id, ~get_tracks(.x))

get_track_features<-function(x, track_list){
  cnt_start = (x*100)+1
  cnt_end = (x*100)+100

  get_track_audio_features(ids=track_list[cnt_start:cnt_end])
}

bowie_features<-map_df(0:10, ~get_track_features(.x, bowie_tracks$id)) %>%
  left_join(bowie_tracks %>% select(track_id, album_id), by=c("id" = "track_id")) %>%
  left_join(bowie_albums %>% select(album_id, release_date)) %>%
  mutate(release_year = as.numeric(str_extract(release_date,"^[0-9]{4}"))) %>%
  filter(!is.na(release_year))

bowie_numbers<-bowie_features %>% select_if(is.numeric)

ggplot(bowie_features, aes(x=speechiness,y=danceability))+
  geom_point(aes(color=as.factor(mode)))
