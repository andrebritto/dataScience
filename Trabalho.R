library(dplyr)
library(readr)
library(ggplot2)
library(GGally)
library(gridExtra)
library(grid)
library(treemap)
library(DT)

setwd('dataset/')

# 1 - Qual são os 10 generos musical mais popular (numero de stream) por continente (separadamente)

df_features <- read_csv('SpotifyFeatures.csv',locale = locale(encoding = "UTF-8"))

musica_genero<-df_features %>% select(track_name,genre,popularity,danceability,energy, valence)

musica_genero <-musica_genero %>% group_by(track_name) %>% top_n(1,popularity)

musica_genero <- musica_genero %>% top_n(1,genre)

colnames(musica_genero)<-c('faixa','genero','popularidade','danceability','energy','valence')

musica_genero <- musica_genero[,c('faixa','genero','popularidade','danceability','energy','valence')]

musica_genero[,"genero"] %>% distinct()


df_stream_17_18 <- read_csv('data-spotifyStreamming.csv',locale = locale(encoding = "UTF-8"))
df_stream_17_18$Region <-toupper(df_stream_17_18$Region)
colnames(df_stream_17_18) <-  c('posicao','faixa','artista','execucao','url','data_execucao','region')

head(df_stream_17_18,50)

df_tocadas <- df_stream_17_18 %>% filter( !is.na(artista) ) 

rm(df_stream_17_18)

tocadas_musica_genero<-inner_join(df_tocadas,musica_genero)

tocadas_musica_genero <- tocadas_musica_genero[,c("faixa","artista","data_execucao","region","genero",'popularidade','danceability','energy','valence')]

df_paises <- read_csv('iso3166.csv',locale = locale(encoding = "UTF-8"))[,c("name","alpha-2","region","sub-region")]
colnames(df_paises) <-  c('pais','sigla','continente','sub-continente')

tocadas_musica_genero_pais_continente <- inner_join(tocadas_musica_genero,df_paises, by=c("region" = "sigla"))

rm(tocadas_musica_genero,df_paises,df_tocadas, df_tocadas_regiao, musica_genero)

# relatorios ###################################################
# quest01 - 1 - Qual são os 5 generos musical mais popular por continente


quest01 <-  tocadas_musica_genero_pais_continente %>% 
  group_by(continente, genero) %>% summarise(qtd = n())

resp01 <- quest01 %>% arrange(-qtd,continente) %>% top_n(5)
resp01 <- resp01 %>% group_by(genero) %>% mutate(percent=round(100*(qtd/sum(resp01$qtd)),2))
resp01  %>% arrange(continente)
DT::datatable(resp01, class = 'cell-border stripe')


total_qtd = sum(resp01$qtd)
total_qtd
# Grafico da resposta 01
ggplot(data = resp01, aes(x=continente, y=genero, label= paste(round(percent,2),'%') , fill= percent)) + 
  geom_tile() +
  geom_text()+
  labs(title = "Total de Execução de Generos em Cada Continente", x='',y='') +
  guides(fill=guide_legend(title = paste("% Sobre o \n Total de Execuções:", total_qtd )))

rm(quest01)

# Quest02 - Quem é o artista mais popular em cada genero

# Os generos
generos<-resp01 %>% select (genero) %>% distinct() %>% unlist


quest02 <-tocadas_musica_genero_pais_continente %>%
  select (genero, artista) %>% 
  filter(genero %in% generos) %>%
  group_by(genero,artista) %>%
  summarise(qtd_toca = n() ) 

resp02 <- quest02 %>%  arrange(-qtd_toca) %>% top_n(1)

resp02

DT::datatable(resp02, class = 'cell-border stripe')

rm(quest02)

# Grafico da resposta 02
ggplot(resp02, aes(x=resp02$qtd_toca, y=resp02$artista, color=resp02$genero)) +
  geom_point(size=3) +
  geom_text(label=resp02$qtd_toca, nudge_x = 0.25, nudge_y = 0.25, check_overlap = T) +
  geom_segment(aes(x=0 , y=resp02$artista, xend=resp02$qtd_toca , yend=resp02$artista )) + 
  labs(title = "Total de Execução de Generos em Cada Continente", x='',y='') +
  guides(color=guide_legend(title ="Genero"))



# quest03 - montar uma playlist com 10 musicas por artistas mais populares

artistas_10_mais <- resp02 %>% arrange(-qtd_toca)

artistas_10_mais <- artistas_10_mais['artista'] %>% distinct() %>% unlist

tocadas_musica_genero_pais_continente

resp03<-tocadas_musica_genero_pais_continente %>% 
  select (artista, genero,faixa) %>%
    filter((artista %in% artistas_10_mais) & (genero %in% generos) ) %>%
  group_by(artista, faixa) %>%
  summarise(qtd = n()) %>%
 arrange(-qtd) %>% top_n(10)
  


#resp03
 DT::datatable(resp03, class = 'cell-border stripe')

 # artista mais famoso - que toca em mais paises
 
 #artis_pais_continente <-tocadas_musica_genero_pais_continente [sample(nrow(tocadas_musica_genero_pais_continente),100),c('artista', 'pais', "continente")]
 
 artis_pais_continente <-tocadas_musica_genero_pais_continente [,c('artista', 'pais', "continente")]
 
 resp_aux <-artis_pais_continente %>% group_by(artista) %>% count(continente) %>% arrange(-n)
 resp_aux <- head(resp_aux,100)
 
 resp04 <- resp_aux %>% group_by(artista,continente) %>% summarise(rnk = max(n))
 
 resp04 <-resp04 %>% group_by(continente) %>% top_n(3)
 
 png(filename="tree.png",width=800, height=800)
 treemap(resp04,
         # definindo os niveis hierarquicos
         index = c("continente", "artista"),
         # variavel numerica que define o tamanho dos blocos
         vSize = "rnk",
         # titulo
         title = "Os Reis dos Quatro Cantos",
         # definir estilo da fonte e ajuste vertical para cada um dos niveis hierarquicos
         fontface.labels = c("oblique", "bold", "italic"),
         fontsize.labels = c(12,10),
         align.labels = list(c("centre","centre"),c("left","top")),
         ymod.labels = c(0.6, 0, 0)
 ) 

 
rm(resp01, resp02, resp03, resp04)
rm(tocadas_musica_genero, tocadas_musica_genero_pais_continente)
rm(artis_pais_continente, artistas_10_mais)
## Validar popularidade
# Verificação de correlação entre a indicador de popularidade 
modelo.dado <- df_features[c("popularity" ,"danceability","energy","valence")]
rm(df_features)
# Pegando amostra
#lista_index <- sample(1:150000, 150000, replace=TRUE)
#modelo.dado <- modelo.dado[lista_index,]

# Analisar a correlação entre as variáveis popularidade, danceability, energy e valence"

library("ggcorrplot")
# Matriz de Coeficiente de Correlação Linear de Pearson entre as variaveis
corr <- round(cor(modelo.dado), 3)
corr

ggcorrplot(corr, p.mat = cor_pmat(modelo.dado),
           hc.order = TRUE, type = "lower",
           color = c("#FC4E07", "white", "#00AFBB"),
           outline.col = "white", lab = TRUE)

ajuste01 <- lm(modelo.dado$valence ~ modelo.dado$danceability)

ajuste02 <-lm(modelo.dado$valence ~ modelo.dado$energy)

summary(ajuste01)

summary(ajuste02)


# Os coeficientes próximos de zero mostram uma correlação fraca e os proximos de 1 uma correlação forte


# As correlações mais fortes são: valence com danceability e valence com energy 
# A variavel popularidade não é explicada por nenhuma das relações
