library(readr)
library('ggplot2')
require(GGally)
require(plyr)
require(tidyr)
require(readxl)

require(dplyr)

require(magrittr)
setwd('/home/andre/git/dataScience/dataset/spotify/')

# Musicas
# Metadados das Musicas
df <- read_csv('SpotifyFeatures.csv')

# Tocadas entre Jan de 2017 e Jan de 2018
df_stream_17_18 <- read_csv('data-spotifyStreamming-2017.csv')
summary(df_stream_17_18)

# Paises e continentes

# informações de apoio. Pais e seu rspectivo continente
df_continentes <- read_csv('countryContinent.csv', locale = readr::locale(encoding = "latin1"))
df_continentes <- df_continentes[,c("country","code_2", "continent")]

df_continentes <- df_continentes %>% rename(Region = code_2)

df_continentes

# df        - 228159
# df_stream - 3441197

summary(df)
summary(df_stream_17_18)


# 1 - Qual são os 10 generos musical mais popular (numero de stream) por continente (separadamente)
# 2 - Quem é o artista mais popular em cada genero
# 3 - montar uma playlist com 10 musicas por genero com base nos artistas mais populares
# 4 - montar uma playlist com 10 musicas por genero com base nos artistas similares aos populares
# 5 - Quais as escalas musicais mais utilizadas nos 10 generos mais populares

# 1.
# Agrupar as musicas mais tocadas
# Limpeza dos dados das musicas

sample(df_stream_17_18,100, replace = T)
df_stream_17_18 <- df_stream_17_18 %>% rename(track_name = `Track Name`)

df_stream_17_18 <- df_stream_17_18 %>% 
  filter(!is.na(track_name))


# Quais são os generos 
generos <- df %>% select (genre) %>% distinct()

df[,c("genre","track_name")]

df_musica_regiao_genero <- inner_join(df_stream_17_18,df[,c("genre","track_name")],by='track_name') %>% distinct()

df_musica_regiao_genero$Region <-toupper(df_musica_regiao_genero$Region)

# Contagem de reprodução da musica em cada pais classificada por genero


# Adicionando a informação de continente 
df_musica_regiao_genero <- inner_join(df_musica_regiao_genero, df_continentes)
df_stream_17_18$Region <- toupper(df_stream_17_18$Region)
df_stream_17_18 <- inner_join(df_continentes,df_stream_17_18)


tmp<-df_musica_regiao_genero[,c("genre","continent")]

genero_continent_contagem <- tmp %>% group_by(genre, continent) %>% summarise(qtd = n()) %>% arrange(-qtd)
rm(tmp)

genero_continent_contagem %>% group_by(continent) %>% summarise(n = sum(qtd)) %>% arrange(-n)
genero_continent_contagem %>% group_by(genre) %>% summarise(n=sum(qtd)) %>% arrange(-n)

ggplot(genero_continent_contagem, aes(genero_continent_contagem$continent, genero_continent_contagem$genre)) +
  geom_raster(aes(fill = genero_continent_contagem$qtd), interpolate = TRUE)

df_top_10_continent <- genero_continent_contagem %>% group_by(continent) %>% top_n(n=10)

df_stream_17_18 <- inner_join(df_stream_17_18,df_top_10_continent)



df_top_10_continent

df_stream_17_18 %>% group_by(country) %>% distinct()

genero_tracs <- df[df$track_name %in% df_top10_tracks$`Track Name`,c("track_name","genre")] %>% distinct()

df_top10_tracks[df_top10_tracks$`Track Name` %in% genero_tracs$track_name,]

genero_tracs <- genero_tracs %>% rename('Track Name' =  track_name)



str(genero_tracs)
str(df_top10_tracks)

df_01$Region <- toupper(df_01$Region)

# 
df_01 <- inner_join(genero_tracs ,df_top10_tracks)

df_continentes

df_01

df_01_final <- inner_join(df_continentes, df_01)

df_01_final

df_top10_tracks[df_top10_tracks$`Track Name` == 'Alone',]

df_generos <- df %>% group_by(genre) %>% summarise(qtd = n())

df_generos %>% arrange(-qtd)

df_genre_top3 <- head(df_generos %>% arrange(-qtd),3)$genre
# Não usar popularity e sim a quantidade de registros de stream


df_popular_genero <- df %>% filter(genre %in% df_genre_top3) %>% group_by(artist_name, genre) %>% summarise(mais_popular = sum(popularity))

df_popular_genero %>% arrange(-mais_popular)

df %>% group_by(artist_name, genre) %>% summarise(x = sum(popularity)) %>% arrange(-x) %>% arrange(artist_name)


df %>% select(artist_name, genre, popularity) %>% filter(genre %in% df_genre_top3)  #%>% group_by(artist_name, genre) %>% summarise(mais_popular = sum(popularity))

xx <- df %>% select(artist_name, genre, popularity) %>% 
  group_by(artist_name, genre) %>% 
  summarise(mais_popular = sum(popularity)) %>%
  arrange(mais_popular)

gen <- unique(xx$genre)

tmp <- data.frame(artist_name = character(), genre = character(), mais_popular = numeric(),stringsAsFactors = FALSE)

xx[xx$artist_name == 'mAsis',]
str(tmp)

for(i in (unique(xx$genre))){
  print(xx[xx$mais_popular == max(xx[xx$genre == i ,"mais_popular"]),])
  #tmp <- rbind(tmp,xx[xx$mais_popular == max(xx[xx$genre == i ,"mais_popular"]),])
  bind_rows(tmp,xx[xx$mais_popular == max(xx[xx$genre == i ,"mais_popular"]),])
}

