require(dplyr)
require(readr)
View(cars)

cars
# Maior distancia por velocidade
cars %>% group_by(speed) %>% summarise(max(dist))

# as duas maiores distancias por velocidade
 cars %>% group_by(speed) %>% top_n(2,dist)

View(x) 


setwd('/home/andre/git/dataScience/dataset/spotify/')

# DF de paises
df_paises <- read_csv('iso3166.csv',locale = locale(encoding = "UTF-8"))[,c("name","alpha-2","region","sub-region")]
colnames(df_paises) <-  c('pais','sigla','continente','sub-continente')

df_paises


