

######################################################

#ATTENZIONE!
#TRAMITE QUESTA SOLA RIGA E' POSSIBILE APRIRE L'APP:

# shiny::runGitHub( "AsteEnergiaElettrica", "MMazzei3")

######################################################

library(shiny)
library(shinyWidgets)
library(lubridate)
library(ggplot2)
library(plotly)
library(tidyr)
library(dplyr)
library(shinythemes)

load("data/DATI_PULITI2.RData")
#RIS.HELLO <- read.csv("data/RIS_HELLO.csv")
RIS.HELLO <- read.csv("data/RIS_ENERGY_HELLO.csv")
RIS.ENERGY <- read.csv("data/RIS_ENERGY3.csv")
dt2 %>% filter(OPERATORE != "ACEA ENERGIA SPA") -> dt3
td %>%
    filter(OPERATORE != "ACEA ENERGIA SPA") %>%
    group_by(BID_OFFER_DATE_DT, ANNO, GIORNOWEEK) %>%
    summarise(ENERGY_PRICE_NO = sum(ENERGY_PRICE_NO), 
              QUANTITY_NO = sum(QUANTITY_NO), .groups = 'drop') -> dt4
source("Funzioni2.R")





ui <-  navbarPage(title = "Asta Energia Elettrica",
                  theme = shinytheme("journal"),
                  tabPanel(title="Contesto",
                           h6(em("Manuel Generoso & Marco Mazzei - Università degli Studi di Milano Bicocca"), align = "right"), 
                           br(),
                           br(), 
                           "Il", 
                           strong("Gestore dei Mercati Energetici"), 
                           a("(GME)", href = "https://www.mercatoelettrico.org/it/"),
                           "è la società responsabile in Italia dell'organizzazione e della gestione del mercato elettrico, oltre che di assicurare la gestione economica di un'adeguata disponibilità della riserva di potenza.",
                           br(),
                           br(),
                           
                           "Il ", 
                           strong("Mercato del Giorno Prima"),
                           a("(MGP)", href = "https://www.mercatoelettrico.org/it/Mercati/MercatoElettrico/MPE.aspx"),
                           "ospita la maggior parte delle transazioni di compravendita di energia elettrica.",
                           "Caratteristiche principali:",
                           
                           br(),
                           
                           p("- Sul MGP si scambiano blocchi orari di energia per il giorno successivo;"),
                           p("- Gli operatori partecipano presentando offerte nelle quali indicano la quantità ed il prezzo massimo/minimo al quale sono disposti ad acquistare/vendere;"),
                           p("- La seduta del MGP si apre alle ore 8.00 del nono giorno antecedente il giorno di consegna e si chiude alle ore 12.00 del giorno precedente il giorno di consegna. Gli esiti del MGP vengono comunicati entro le ore 12.55 del giorno precedente il giorno di consegna;"),
                           p("- Le offerte sono accettate dopo la chiusura della seduta di mercato, sulla base del merito economico e nel rispetto dei limiti di transito tra le zone. Il MGP è quindi un mercato d'asta e non un mercato a contrattazione continua;"),
                           p("- Tutte le offerte di vendita e di acquisto accettate sul MGP vengono valorizzate al prezzo marginale di equilibrio della zona a cui appartengono. Tale prezzo è determinato, per ogni ora, dall'intersezione della curva di domanda e di offerta, e si differenzia da zona a zona in presenza di limiti di transito saturati;"),
                           p("- Le offerte di acquisto accettate e riferite alle unità di consumo appartenenti alle zone geografiche italiane sono valorizzate al prezzo unico nazionale (PUN), pari alla media dei prezzi delle zone geografiche ponderata per le quantità acquistate in tali zone."),
                           br(), 
                           h4(strong("L'obiettivo: ")),
                           p("Supponendo di lavorare per l'azienda", 
                             a("ACEA", href = "https://www.acea.it/"), 
                             "si vuole prevedere l'andamento della curva di offerta della 
                  concorrenza a livello nazionale. I dati a disposizione fanno riferimento agli anni
                  2015-2020 e riguardano i prezzi e quantità offerte alle ore 11:00."),
                           p("In questa analisi si prende in considerazione l'intero territorio nazionale, 
                  pertanto i dati per zona geografica sono stati aggregati per azienda."),
                           br()
                           ),
              navbarMenu(title="Esplorative",
              tabPanel(title="Annuali",
                        pageWithSidebar(
                            
                            h1(" ", align = "center"),
                            
                            sidebarPanel(
                                checkboxGroupInput("e1",
                                                   "Scegli gli anni da visualizzare:",
                                                   choices = list("2015" = 1,
                                                                  "2016" = 2,
                                                                  "2017" = 3,
                                                                  "2018" = 4,
                                                                  "2019" = 5,
                                                                  "2020" = 6),
                                                   selected = 1),
                                radioButtons("e2",
                                             "Scegli una opzione",
                                             c("Prezzo" = 1, "Quantità" = 2)
                                
                            )),

                            
                            mainPanel(
                                plotlyOutput("e3"))
                        )),
              tabPanel(title="Settimanali",
                       pageWithSidebar(
                           h1("", align = "center"),
                           sidebarPanel(
                               radioButtons("e4",
                                                  "Scegli gli anni da visualizzare:",
                                                  c("2015" = 1,
                                                                 "2016" = 2,
                                                                 "2017" = 3,
                                                                 "2018" = 4,
                                                                 "2019" = 5,
                                                                 "2020" = 6),
                                                  selected = 1),
                               radioButtons("e5",
                                            "Scegli una opzione",
                                            c("Prezzo" = 1, "Quantità " = 2)
                                            
                               )),
                           
                           
                           mainPanel(
                               plotlyOutput("e6"))
                       ))),
              
              
              
              
              navbarMenu(title="Modelli",
                         tabPanel(title="Modello1",
                                  strong("L'IDEA: "), 
                                  br(), 
                                  "Per fare previsione sul giorno successivo si è pensato di
                  applicare un algoritmo in base al giorno della settimana
                  da prevedere. Inoltre alcuni giorni particolari sono stati trattati in modo differente.",
                                  br(), 
                                  "La previsione sarà fornita dalla mediana delle curve di offerta
                rispettivamente dei giorni:",
                                  br(),
                                  br(),
                                  strong("- lunedì:"), p("Poichè è il primo giorno lavorativo, esso dipenderà 
                  dall'ultimo giorno lavorativo della settimana precedente (venerdì) e
                  dal lunedì precedente"), 
                                  strong("- martedì:"),p("Poichè è il secondo giorno lavorativo, esso dipenderà
                  dai primi due giorni lavorativi precedenti (lunedì e venerdì)"), 
                                  strong("- mercoledì, giovedì,venerdì:"), p("Questi giorni dipendono dai due giorni
                  lavorativi precedenti"), 
                                  strong("- sabato:"),p("Saranno dipendenti dal venerdì e dal sabato precedente"), 
                                  strong("- domenica:"),p("Dipenderanno dal sabato e dalla domenica precedenti"),
                                  br(), 
                                  strong("Giorni speciali"),
                                  br(),
                                  p("- Capodanno: dipendenderà dal 31 Dicembre "),
                                  p("- 2 Gennaio: dipenderà dal 30-29 Dicembre (utlimi 2 giorni lavorativi)"),
                                  p("- 7 Gennaio: dipenderà dal 03-02 Gennaio (utlimi 2 giorni lavorativi)"),
                                  p("- Pasquetta(13 Aprile) : dipenderà dalla Pasqua precedente, nonchè il giorno prima"),
                                  
                         ),
                         tabPanel(title="Modello2",
                                  strong("L'IDEA: "), 
                                  br(), 
                                  "Per fare previsione sul giorno successivo si è pensato di
                  interpolare la curva di Offerta tramite un'accurata approssimazione di tale funzione, in modo da
                poter costruire un numero prefissato k di serie storiche univariate della domanda per ogni
                possibile valore del prezzo.",
                                  br(),
                                  "Nello specifico abbiamo deciso di considerare una variazione del prezzo pari a 2 unità nell'intervallo
                0-500, costruendo cosi 251 serie storiche della quantità per ogni prezzo. ",
                                  br(), 
                                  "Un primo approccio è stato quello di considerare i primi 10 giorni precedenti al giorno da prevedere, e costruire
                le nostre previsioni per ogni prezzo del giorno dopo, tramite un modello STLF(Seasonal and Trend decomposition using Loess Forecasting model)"
                         ),
                         tabPanel(title="Modello3",
                                  strong("L'IDEA: "), 
                                  br(), 
                                  "Il terzo metodo riprende le stesse caratteristiche del secondo metodo, ma utilizza i 3 anni
                precedenti come input per fare previsoni sul giorno successivo."
                         )
                         
              ),
              
              tabPanel(title="Risultati",
                       pageWithSidebar(
                           
                           h1(" ", align = "center"),
                           
                           sidebarPanel(
                               dateInput("AnnoMeseGiorno", 
                                         h3("Giorno da prevedere"), 
                                         value = "2020-01-01", 
                                         min = "2020-01-01", 
                                         max = "2020-04-30"), 
                               
                               radioButtons("modeltype", 
                                            "Scegli un modello", 
                                            c("Personalizzato (Modello 1)" = 1, 
                                              "10 giorni prima (Modello 2)" = 2, 
                                              "Serie storica (Modello 3)" = 3, 
                                              "Confronto" = 4)),
                               tableOutput("map")  
                               
                                
                           ),
                           mainPanel(
                                plotlyOutput("r")
                                
                               ))
                       )
)

    
    


server <- function(input, output) {
  
  output$e3 <- renderPlotly({
    
    anni <- sapply(input$e1,
                   function(x){
                     switch (x,
                             "1" = 2015,
                             "2" = 2016,
                             "3" = 2017,
                             "4" = 2018,
                             "5" = 2019,
                             "6" = 2020
                     )
                   })
    
    if(input$e2 == 1){
      dt4 %>% 
        filter(ANNO %in% anni) %>%
        ggplot(aes(BID_OFFER_DATE_DT,ENERGY_PRICE_NO)) +
        geom_line() + geom_smooth(method = "loess", span = .15) +
        xlab("") + ylab("Prezzo in euro") +
        ggtitle("Consumo di energia elettrica negli anni 2015-2020") +
        facet_wrap(~ ANNO, scales = "free",nrow = 3, ncol = 2) + 
        theme_light()+
        theme(plot.title = element_text(hjust = 0.5))
      
      ggplotly()
    }else if(input$e2 == 2){
      dt4 %>%
        filter(ANNO %in% anni) %>%
        ggplot(aes(BID_OFFER_DATE_DT,QUANTITY_NO)) +
        geom_line() + geom_smooth(method = "loess", span = .15) +
        xlab("") + ylab("Quantià in MWh") +
        ggtitle("Consumo di energia elettrica negli anni 2015-2020") +
        facet_wrap(~ANNO, scales = "free",nrow = 3, ncol = 2) + 
        theme_light()+
        theme(plot.title = element_text(hjust = 0.5))
      
      ggplotly()
    }
    
  })
  
  output$e6 <- renderPlotly({
    
    anni <- sapply(input$e4,
                   function(x){
                     switch (x,
                             "1" = 2015,
                             "2" = 2016,
                             "3" = 2017,
                             "4" = 2018,
                             "5" = 2019,
                             "6" = 2020
                     )
                   })
    
    if(input$e5 == 1){
      dt4 %>%
        filter(ANNO %in% anni) %>%
        ggplot(aes(BID_OFFER_DATE_DT,ENERGY_PRICE_NO)) +
        geom_line() +
        xlab(" ") + ylab("Prezzo in euro") +
        ggtitle("Consumo di energia elettrica negli anni 2015-2020") +
        facet_wrap(~ GIORNOWEEK, scales = "free", nrow = 4, ncol = 2) + 
        theme_light() + 
        theme(plot.title = element_text(hjust = 0.5))
      
      ggplotly()
      
    }else if(input$e5 == 2){
      
      dt4 %>%
        filter(ANNO %in% anni) %>%
        ggplot(aes(BID_OFFER_DATE_DT,QUANTITY_NO)) +
        geom_line() +
        xlab(" ") + ylab("Quantità in MWh") +
        ggtitle("Consumo di energia elettrica negli anni 2015-2020") +
        facet_wrap(~ GIORNOWEEK, scales = "free", nrow = 4, ncol = 2) + 
        theme_light() + 
        theme(plot.title = element_text(hjust = 0.5))
      
      ggplotly()
    }
  })
  
  output$r <- renderPlotly({
    
    if (input$modeltype==1){
      
      fit <- ENERGETICO(dt3, input$AnnoMeseGiorno, .1)
      names(fit$Splines)[-3] <- c("Price", "Quantity")
      df <- rbind.data.frame(fit$RealCurve, fit$PredCurve, fit$Splines[, -3])
      df$Legenda <- c(rep("Attuale", NROW(fit$RealCurve)), 
                      rep("Prevista", NROW(fit$PredCurve)),
                      rep(unique(fit$Splines$Giorno), each = NROW(fit$PredCurve)))
      df$Legenda <- factor(df$Legenda, c("Attuale", "Prevista", unique(fit$Splines$Giorno)))
      
      df %>% 
        ggplot(aes(Price, Quantity,  colour = Legenda)) + 
        geom_line(aes(linetype = Legenda)) +
        scale_colour_manual(values = c("coral", "cornflowerblue", rep("gray80", NROW(unique(fit$Splines$Giorno))))) + 
        scale_linetype_manual(values = c("solid", "solid", rep("dotdash", NROW(unique(fit$Splines$Giorno))))) + 
        xlab("Prezzo in euro") + ylab("Quantità cumulata in MWh") + 
        ggtitle(paste0("Curva di offerta di ", weekdays(input$AnnoMeseGiorno), " ", input$AnnoMeseGiorno)) + 
        theme_light() + 
        theme(plot.title = element_text(hjust = 0.5))
      
      ggplotly()
      
    }else if (input$modeltype==2){
      
      RIS.ENERGY %>% filter(Giorno == input$AnnoMeseGiorno) %>% 
        ggplot(aes(Price, Quantity, col = Legenda)) + geom_line() + 
        scale_colour_manual(values = c("coral", "cornflowerblue")) +
        xlab("Prezzo in euro") + ylab("Quantità cumulata in MWh") + 
        ggtitle(paste0("Curva di offerta di ", weekdays(input$AnnoMeseGiorno), " ", input$AnnoMeseGiorno)) + 
        theme_light() + 
        theme(plot.title = element_text(hjust = 0.5))
      
    }else if (input$modeltype==3){
      
      RIS.HELLO %>% filter(Giorno == input$AnnoMeseGiorno) %>% 
        ggplot(aes(Price, Quantity, col = Legenda)) + geom_line() + 
        scale_colour_manual(values = c("coral", "cornflowerblue")) +
        xlab("Prezzo in euro") + ylab("Quantità cumulata in MWh") + 
        ggtitle(paste0("Curva di offerta di ", weekdays(input$AnnoMeseGiorno), " ", input$AnnoMeseGiorno)) + 
        theme_light() + 
        theme(plot.title = element_text(hjust = 0.5))
      
    }else if (input$modeltype==4){
      
      fit <- ENERGETICO(dt3, input$AnnoMeseGiorno, .1)
      names(fit$Splines)[-3] <- c("Price", "Quantity")
      df <- rbind.data.frame(fit$RealCurve, fit$PredCurve, fit$Splines[, -3])
      df$Legenda <- c(rep("Attuale", NROW(fit$RealCurve)),
                      rep("Prevista", NROW(fit$PredCurve)),
                      rep(unique(fit$Splines$Giorno), each = NROW(fit$PredCurve)))
      #df$Legenda <- factor(df$Legenda, c("Attuale", "Prevista", unique(fit$Splines$Giorno)))
      
      df %>% filter(Legenda %in% c("Attuale", "Prevista")) -> a
      a <- cbind.data.frame(a, fit$MAPE)
      RIS.ENERGY %>% filter(Giorno == input$AnnoMeseGiorno) -> b
      RIS.HELLO %>% filter(Giorno == input$AnnoMeseGiorno) -> c
      
      d <- rbind.data.frame(a, b[, names(b) != "Giorno"], c[, names(c) != "Giorno"])
      d$Modello <- factor(c(rep("Presonalizzato", NROW(a)), 
                            rep("10 Giorni", NROW(b)),
                            rep("Serie Storica", NROW(c))), 
                          c("Presonalizzato", "10 Giorni", "Serie Storica"))
      
      d %>%
        ggplot(aes(Price, Quantity, col = Legenda)) + geom_line() +
        scale_colour_manual(values = c("coral", "cornflowerblue")) +
        xlab("Prezzo in euro") + ylab("Quantità cumulata in MWh") +
        ggtitle(paste0("Curva di offerta di ", weekdays(input$AnnoMeseGiorno), " ", input$AnnoMeseGiorno)) +
        facet_wrap(~ Modello, scales = "free") + 
        theme_light() + 
        theme(plot.title = element_text(hjust = 0.5))
      
    }
    
    
    
  })
  
  output$map <- renderTable({
    
    if(input$modeltype==4){
      
      fit <- ENERGETICO(dt3, input$AnnoMeseGiorno, .1)
      names(fit$Splines)[-3] <- c("Price", "Quantity")
      df <- rbind.data.frame(fit$RealCurve, fit$PredCurve, fit$Splines[, -3])
      df$Legenda <- c(rep("Attuale", NROW(fit$RealCurve)),
                      rep("Prevista", NROW(fit$PredCurve)),
                      rep(unique(fit$Splines$Giorno), each = NROW(fit$PredCurve)))
      #df$Legenda <- factor(df$Legenda, c("Attuale", "Prevista", unique(fit$Splines$Giorno)))
      
      df %>% filter(Legenda %in% c("Attuale", "Prevista")) -> a
      a <- cbind.data.frame(a, fit$MAPE)
      RIS.ENERGY %>% filter(Giorno == input$AnnoMeseGiorno) -> b
      RIS.HELLO %>% filter(Giorno == input$AnnoMeseGiorno) -> c
      
      fr <- c(unique(a$MAPE),unique(a$MAPE200))
      fr2 <- c(unique(b$MAPE),unique(b$MAPE200))
      fr3 <- c(unique(c$MAPE),unique(c$MAPE200))
      fin <- rbind(fr,fr2, fr3)
      rownames(fin) <- paste0("Modello", 1:3)
      colnames(fin)<-c("MAPE","MAPE200")
      
      fin
      
    }
    
    
  },
  
  striped = TRUE, bordered = TRUE,
  hover = TRUE, spacing = 'xs',
  width = '100%', align = 'c',
  rownames = TRUE,
  digits = 3)
  
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
