#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(readr)
library(ggplot2)


# Define UI for application that draws a histogram
ui <- fluidPage(
    navbarPage('>>',    
               # Application title
               tabPanel("Questão 01",
               
               # Sidebar with a slider input for number of bins 
               sidebarLayout(
                   sidebarPanel(
                       selectInput('nome', 'Continente', c("Todos", df_paises[,c("region")]))
                   ),
                   
                   # Show a plot of the generated distribution
                   mainPanel(
                       h4("Observations"),
                       verbatimTextOutput("summary"),
                       DT::dataTableOutput("view"),
                       plotOutput("plot")
                   )
               )
        ) ,
        tabPanel("Questão 02",
                 mainPanel(
                     h4("Questão 02"),
                     verbatimTextOutput("summaryQ2"),
                     DT::dataTableOutput("viewQ2"),
                     plotOutput("plotQ2")
                 )
                          
        )
        ,
        tabPanel("Questão 03",
                 mainPanel(
                     h4("Questão 03"),
                     verbatimTextOutput("summaryQ3"),
                     DT::dataTableOutput("viewQ3"),
                     plotOutput("plotQ3")
                 )
                 
        )
        ,
        tabPanel("Questão 04",
                 mainPanel(
                     h4("Questão 04"),
                     verbatimTextOutput("summaryQ4"),
                     DT::dataTableOutput("viewQ4"),
                     plotOutput("plotQ4")
                 )
                 
        )
    )
)
# Define server logic required to draw a histogram
server <- function(input, output) {
    # datasets
    df_features <- read_csv('../dataset/SpotifyFeatures.csv',locale = locale(encoding = "UTF-8"))
    
    musica_genero<-df_features %>% select(track_name,genre,popularity,danceability,energy, valence)
    
    musica_genero <-musica_genero %>% group_by(track_name) %>% top_n(1,popularity)
    
    musica_genero <- musica_genero %>% top_n(1,genre)
    
    colnames(musica_genero)<-c('faixa','genero','popularidade','danceability','energy','valence')
    
    musica_genero <- musica_genero[,c('faixa','genero','popularidade','danceability','energy','valence')]
    
    musica_genero[,"genero"] %>% distinct()
    
    
    df_stream_17_18 <- read_csv('../dataset/data-spotifyStreamming.csv',locale = locale(encoding = "UTF-8"))
    df_stream_17_18$Region <-toupper(df_stream_17_18$Region)
    colnames(df_stream_17_18) <-  c('posicao','faixa','artista','execucao','url','data_execucao','region')
    
    head(df_stream_17_18,50)
    
    df_tocadas <- df_stream_17_18 %>% filter( !is.na(artista) ) 
    
    rm(df_stream_17_18)
    
    tocadas_musica_genero<-inner_join(df_tocadas,musica_genero)
    
    tocadas_musica_genero <- tocadas_musica_genero[,c("faixa","artista","data_execucao","region","genero",'popularidade','danceability','energy','valence')]
    
    df_paises <- read_csv('../dataset/iso3166.csv',locale = locale(encoding = "UTF-8"))[,c("name","alpha-2","region","sub-region")]
    colnames(df_paises) <-  c('pais','sigla','continente','sub-continente')
    
    tocadas_musica_genero_pais_continente <- inner_join(tocadas_musica_genero,df_paises, by=c("region" = "sigla"))
    
    quest01 <-  tocadas_musica_genero_pais_continente %>% 
        group_by(continente, genero) %>% summarise(qtd = n())
    
    resp01 <- quest01 %>% arrange(-qtd,continente) %>% top_n(5)
    resp01 <- resp01 %>% group_by(genero) %>% mutate(percent=round(100*(qtd/sum(resp01$qtd)),2))
    
    
    selectedData <- reactive({
        df_paises[, c('continente')]
    })
    
    
    output$view <- DT::renderDataTable(DT::datatable({
        if(input$nome == 'Todos'){
            resp01
        } else{
            resp01 %>% filter(continente == input$nome)
        }
    }))
    
    
    total_qtd = sum(resp01$qtd)
    total_qtd
    # Grafico da resposta 01
    
    rm(quest01)
    
    output$plot <- renderPlot({
        ggplot(data = resp01, aes(x=continente, y=genero, label= paste(round(percent,2),'%') , fill= percent)) + 
            geom_tile() +
            geom_text()+
            labs(title = "Total de Execução de Generos em Cada Continente", x='',y='') +
            guides(fill=guide_legend(title = paste("% Sobre o \n Total de Execuções:", total_qtd )))
        
    })
    
    
    output$summary <- renderPrint({
        input$nome
    })
    
    
    
    
    # Questão 02
    quest02 <-tocadas_musica_genero_pais_continente %>%
        select (genero, artista) %>% 
        filter(genero %in% resp01$genero) %>%
        group_by(genero,artista) %>%
        summarise(qtd_toca = n() ) 
    
    resp02 <- quest02 %>%  arrange(-qtd_toca) %>% top_n(1)
     rm(quest02)
   
    output$plotQ2 <- renderPlot({
        # Grafico da resposta 02
        ggplot(resp02, aes(x=resp02$qtd_toca, y=resp02$artista, color=resp02$genero)) +
            geom_point(size=3) +
            geom_text(label=resp02$qtd_toca, nudge_x = 0.25, nudge_y = 0.25, check_overlap = T) +
            geom_segment(aes(x=0 , y=resp02$artista, xend=resp02$qtd_toca , yend=resp02$artista )) + 
            labs(title = "Total de Execução de Generos em Cada Continente", x='',y='') +
            guides(color=guide_legend(title ="Genero"))
        
        
    })
    
    
    # Questao 03
    
    # quest03 - montar uma playlist com 10 musicas por genero com base nos artistas mais populares
    
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
    output$viewQ3 <- DT::renderDataTable(DT::datatable({
        resp03
    }))
    
    
    # Questão 04
    # artista mais famoso - que toca em mais paises
    
    #artis_pais_continente <-tocadas_musica_genero_pais_continente [sample(nrow(tocadas_musica_genero_pais_continente),100),c('artista', 'pais', "continente")]
    
    artis_pais_continente <-tocadas_musica_genero_pais_continente [,c('artista', 'pais', "continente")]
    
    artis_pais_continente %>% arrange(artista)
    
    resp_aux <-artis_pais_continente %>% group_by(artista) %>% count(continente) %>% arrange(-n)
    resp_aux <- head(resp_aux,100)
    
    resp04 <- resp_aux %>% group_by(artista,continente) %>% summarise(rnk = max(n))
    
    resp04 <-resp04 %>% group_by(continente) %>% top_n(3)
    
    
    
    output$plotQ4 <- renderPlot({
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
    })
    
    
    rm(musica_genero, df_paises,tocadas_musica_genero, artistas_10_mais)
    
}

# Run the application 
shinyApp(ui = ui, server = server)
