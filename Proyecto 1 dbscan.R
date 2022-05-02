pacman::p_load(tidyverse, Rtsne, ggdendro ,magick ,purrr ,mclust, spotifyr, knitr, ggjoy,data.table, fpc, dplyr, dendextend,gridExtra,cluster,factoextra ,httr)#Importamos librerias
#Accedemos a spotify
Sys.setenv(SPOTIFY_CLIENT_ID = '0302518daf824b66bc815b6261adc923')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '343c6e05374745e89301c448061d3de3')
access_token <- get_spotify_access_token()
auth_token <- get_spotify_authorization_code(scope = scope)
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
beats_default <- readRDS("C:/Users/maxho/OneDrive - Universidad Adolfo Ibanez/GITHUB/Mineria_De_Datos/Data/beats.rds")#Cargamos la base de datos de Prueba

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
names(features_filtrado)[10]<- 'track_uri'
#--------------------------------------------
beats_con_cancion <-rbind(beats,features_filtrado) #aca anadimos la cancion de la API a la base de datos
beats_con_cancion <- unique(beats_con_cancion,by="track_uri") #hacemos unicos cada dato para que no hayan redundancias denuevo
rownames(beats_con_cancion)<- beats_con_cancion$track_uri #ponemos los uri como el identificador 
beats_escalados <-scale(beats_con_cancion[,1:9])#escalamos 
#iteramos dbscan pero sampleamos
muestramia<- sample(1:nrow(beats_escalados),size=1500,replace=FALSE)
Dbscan_cl <- dbscan(muestramia, eps = 0.45, MinPts = 10)
canciones_por_cluster = order(Dbscan_cl$cluster)#ordenamos las canciones por clusters
data_cluster = data.frame(beats_con_cancion$track_uri[canciones_por_cluster],Dbscan_cl$cluster[canciones_por_cluster])
cluster_seleccionado = data_cluster[data_cluster$beats_con_cancion.track_uri.canciones_por_cluster.== uri_cancion,2]
playlist = data_cluster[data_cluster$kmeans4.cluster.canciones_por_cluster.==cluster_seleccionado, ] 
#Creamos playlist entre 30 y 50 canciones para que sea aproximadamente unas 3 horas
playlist_final_df = playlist[sample(nrow(playlist),40), ]
playlist_final_df[nrow(playlist_final_df),]= c(uri_cancion,cluster_seleccionado)
#--------------------------------------
#escogimos 4 clusters ya que con 2 quedaban muy separados y con 3 igual se superponian, por ende decidimos utilizar 4 ya que asi teniamos separados en 4 grupos las canciones y aunque dependan de muchisimas variables podriamos obtener literal una radio de la cancion al tener canciones muy parecidas con canciones quzias similares, dandole una sensacion de "Radio" que es una funcion integrada de spotify
#volvemos con spotifyr para crea la playlist
#usamos nuestro nombre de usuario
#-------------------------------------------------------------------------------------------------------------------------

names(playlist_final_df)[1]<- 'track_uri'
playlist_final_df[2]<- NULL
Playlist_exportada = merge(playlist_final_df,beats_default)
##esta es la playlist generada 
View(Playlist_exportada)