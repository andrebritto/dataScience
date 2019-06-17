library(dplyr)
library(plyr)
library(readr)
setwd('/home/andre/git/dataScience/dataset/spotify/')

# 1 - Qual são os 10 generos musical mais popular (numero de stream) por continente (separadamente)

df_features <- read_csv('SpotifyFeatures.csv',locale = locale(encoding = "UTF-8"))

df_features[df_features$track_name == 'Andas En Mi Cabeza',]

musica_genero<-df_features %>% select(track_name,genre,popularity,danceability,energy, valence)

musica_genero <-musica_genero %>% group_by(track_name) %>% top_n(1,popularity)

musica_genero

musica_genero <- musica_genero %>% top_n(1,genre)

colnames(musica_genero)<-c('faixa','genero','popularidade','danceability','energy','valence')

musica_genero <- musica_genero[,c('faixa','genero','popularidade','danceability','energy','valence')]

musica_genero[,"genero"] %>% distinct()

rm(df_features)

df_stream_17_18 <- read_csv('data-spotifyStreamming-2017.csv',locale = locale(encoding = "UTF-8"))
df_stream_17_18$Region <-toupper(df_stream_17_18$Region)
colnames(df_stream_17_18) <-  c('posicao','faixa','artista','execucao','url','data_execucao','region')
summary(df_stream_17_18)

df_tocadas <- df_stream_17_18 %>% filter( !is.na(artista) ) 

rm(df_stream_17_18)


tocadas_musica_genero<-inner_join(df_tocadas,musica_genero)

tocadas_musica_genero

tocadas_musica_genero <- tocadas_musica_genero[,c("faixa","artista","data_execucao","region","genero",'popularidade','danceability','energy','valence')]

df_paises <- read_csv('iso3166.csv',locale = locale(encoding = "UTF-8"))[,c("name","alpha-2","region","sub-region")]
colnames(df_paises) <-  c('pais','sigla','continente','sub-continente')

tocadas_musica_genero_pais_continente <- inner_join(tocadas_musica_genero,df_paises, by=c("region" = "sigla"))

rm(tocadas_musica_genero,df_paises,df_tocadas, df_tocadas_regiao, musica_genero)

# relatorios ###################################################

tocadas_musica_genero_pais_continente



tocadas_musica_genero_pais_continente %>% group_by(region,genero, faixa) %>% tally() %>% top_n(5)

tocadas_musica_genero_pais_continente %>% filter(faixa == 'Andas En Mi Cabeza') %>% group_by(region,faixa) %>% tally()



tocadas_musica_genero_pais<-tocadas_musica_genero_pais_continente %>% group_by(region,genero) %>% tally()

tocadas_musica_genero_continente<-tocadas_musica_genero_pais_continente %>% group_by(continente,genero) %>% tally()


musicas_mais_tocadas_pais <- tocadas_musica_genero_pais_continente %>% group_by(region,faixa) %>% tally() %>% top_n(5)

lm(popularidade ~ valence ,data = tocadas_musica_genero_pais_continente_br)

tocadas_musica_genero_pais_continente_br<-tocadas_musica_genero_pais_continente[tocadas_musica_genero_pais_continente$region == 'BR',]

scatter.smooth(x=tocadas_musica_genero_pais_continente_br$danceability, y=tocadas_musica_genero_pais_continente_br$popularidade, main="popularidade ~ danceability")

scatter.smooth(x=tocadas_musica_genero_pais_continente_br$energy, y=tocadas_musica_genero_pais_continente_br$popularidade, main="popularidade ~ energy")

scatter.smooth(x=tocadas_musica_genero_pais_continente_br$valence, y=tocadas_musica_genero_pais_continente_br$popularidade, main="popularidade ~ valence")
