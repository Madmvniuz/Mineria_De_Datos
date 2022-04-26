#Importamos librerias
pacman::p_load(tidyverse, Rtsne, ggdendro ,magick ,  mclust, spotifyr, knitr, ggjoy)

#conectamos a la api de spotify

#probando si funciona conectarse directamente a la API

Sys.setenv(SPOTIFY_CLIENT_ID = '0401714112b14f72ae4e0fc355244ece')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '159cf2594c864acdb0424b819263cda5')
acces_token <- get_spotify_access_token()

brujeria <- get_artist_audio_features('brujeria')

brujeria %>%
  arrange(valence) %>%
  select(track_name,valence)%>%
  head(5)%>%
  kable()

ggplot(brujeria, aes(x= valence, y= album_name))+
  geom_joy()
  theme_joy()
  





#-------------------------------------------------------------------------------------------------------
#setiamos la semilla Negativa
set.seed(-1)

#Cargamos la base de datos de Prueba
beats <- readRDS("C:/Users/maxho/OneDrive - Universidad Adolfo Ibanez/GITHUB/Mineria_De_Datos/Data/beats.rds")

#Limpiamos las columnas que nos serviran para la clusterizacion 

beats$album_type <- NULL
beats$album_release_date <- NULL
beats$album_release_year <- NULL
beats$album_release_date_precision <- NULL
beats$key <- NULL
beats$key_mode <- NULL
beats$key_name <- NULL
beats$type <- NULL
beats$external_urls.spotify <- NULL
beats$mode_name <- NULL
beats$mode <- NULL
beats$explicit <- NULL
beats$analysis_url<- NULL
beats$time_signature <- NULL
beats$track_href<- NULL
beats$is_local<- NULL
beats$track_name<- NULL
beats$track_number <- NULL
beats$album_id<- NULL
beats$album_name<- NULL
beats$artist_id <- NULL
beats$artist_name<- NULL
beats$track_preview_url<- NULL
beats$disc_number<- NULL

#pasamos de milisegundos a minutos para un trabajo mas agradable XD
beats$duration_min <- beats$duration_ms/60000

#borramos la columna de milisegundos
beats$duration_ms<- NULL


#Una vez listos los datos aplicamos el modelo 1 (dejamso track id y track Uri para ingresarlas a la futura playlist)

