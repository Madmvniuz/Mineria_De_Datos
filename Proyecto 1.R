pacman::p_load(tidyverse, Rtsne, ggdendro ,magick ,purrr ,mclust, spotifyr, knitr, ggjoy,data.table, fpc, dplyr, dendextend )#Importamos librerias
#Accedemos a spotify
Sys.setenv(SPOTIFY_CLIENT_ID = '0401714112b14f72ae4e0fc355244ece')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '159cf2594c864acdb0424b819263cda5')
access_token <- get_spotify_access_token()
auth_token <- get_spotify_authorization_code(scope = scope)
#user_id <- readline(prompt = "Ingrese su user id: ")
#nombre_play = readline(prompt="Ingrese nombre la playlist a crear: ")
Cancion_inicial = readline(prompt="Ingrese la cancion para buscar: ") #pedimos la cancion para buscarla en la API
Busqueda = search_spotify(Cancion_inicial, type = c("track"),market = NULL, limit = 20, offset = 0, include_external = NULL,authorization = get_spotify_access_token(),include_meta_info = FALSE) #busqueda de spotifyr
Filtrada = subset(Busqueda, select= c("name", "album.name", "uri", "id"))#busqueda filtrada
print(Filtrada)
selected_search <- readline(prompt = "Escoja la cancion utilizando su numero asociado:  ") #escojemos la cancion de la lista 
Cancion_inicial = Filtrada[selected_search,3:4] #filtramos para que solo queden uri e id
id_cancion = pull(Cancion_inicial, var=2) #selecciona id
uri_cancion= pull(Cancion_inicial, var=1) #selecciona uri

#Tenemos nuestra cancion lista (uri e id)

#-------------------------------------------------------------------------------------------------------

beats <- readRDS("C:/Users/maxho/OneDrive - Universidad Adolfo Ibanez/GITHUB/Mineria_De_Datos/Data/beats.rds")#Cargamos la base de datos de Prueba

#Limpiamos las columnas que nos serviran para la clusterizacion 
#---------------------------------------------------
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
beats$track_id<- NULL
beats$duration_ms<- NULL

#-------------------------------------------

beats <- unique(beats,by="track_uri") #hacemos unicos cada dato para que no hayan redundancias
features = get_track_audio_features(id_cancion, authorization = get_spotify_access_token()) #con esto extraemos los datos importantes de la cancion directamente desde la api
#filtramos los datos para ingresarlos a beats
features_filtrado = features
features_filtrado$type <- NULL
features_filtrado$id <- NULL
features_filtrado$key <- NULL
features_filtrado$duration_ms <- NULL
features_filtrado$analysis_url <- NULL
features_filtrado$mode <- NULL
features_filtrado$track_href <- NULL
features_filtrado$time_signature <- NULL
names(features_filtrado)[12]<- 'track_uri'
#--------------------------------------------
beats_con_cancion <-rbind(beats,features_filtrado) #aca anadimos la cancion de la API a la base de datos
rownames(beats_con_cancion)<- beats_con_cancion$track_uri #ponemos los uri como el identificador 
