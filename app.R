library(shiny)
library(zoo)
library(RSQLite)
library(DBI)
library(ggplot2)
library(forecast)
library(DT)
library(tidyverse)
library(plotly)
library(ISOweek)
library(xts)
library(googleVis)

Sys.setlocale(category = "LC_ALL", locale = "Polish")

dataDir    <- file.path(getwd(),"data")
if (!file.exists(dataDir)){dir.create(dataDir,mode="0777")}

dbName <- file.path(dataDir,"data.db")
con <- dbConnect(
    dbDriver("SQLite"),
    dbname = dbName
)

### UI
ui <- fluidPage(
    tabsetPanel(
        tabPanel("Import danych", 
                 fluidRow(
                     column(4,
                            actionButton("dataEURO", "pobierz i przetwórz dane EUROSTAT"),
                            actionButton("dataGUS", "pobierz i przetwórz dane GUS"),
                            actionButton("bazaEURO", "umieść dane EUROSTAT w bazie danych"),
                            actionButton("bazaGUS", "umieść dane GUS w bazie danych")
                            ),
                     column(7,
                            textInput("query",
                                      label = "Zapytanie SQL",
                                      value = "select null from zgony_wg_tygodni_eurostat"    
                            ),
                            submitButton(text="Wyślij zapytanie"),
                            offset = 1
                     ),
                 ),
                 fluidRow(
                     column(12,
                            verbatimTextOutput("plainSQLText"),
                            plotlyOutput("out")
                            ),
                 ),
                
                 
                 
        ),
        tabPanel("Przegląd danych",
                 downloadLink("download", "Pobierz plik csv"),
                 DT::dataTableOutput("tbTable")
        ),
        tabPanel("Mapa GUS",
                 fluidRow(
                   column(3,
                          textInput('periodFrom2g',label="data początku badanego okresu [yyyy-mm-dd]",value="2020-01-01"),
                          textInput('periodTo2g',label="data końca badanego okresu [yyyy-mm-dd] ",value="2021-12-01"),
                          textInput('compareFrom2g',label="data początku okresu porównania [yyyy-mm-dd]",value="2015-01-01"),
                          textInput('compareTo2g',label="data końca okresu porównania [yyyy-mm-dd]",value="2019-12-31"),
                          submitButton(text="wyślij zapytanie")
                          
                   ),
                   column(9,
                          htmlOutput("mapagus"),
                          htmlOutput("mapagus2")
                   )
                 )
                 ),
        tabPanel("Mapa EU",
                 fluidRow(
                   column(3,
                          textInput('periodFrom2',label="data początku badanego okresu [yyyy-mm-dd]",value="2020-01-01"),
                          textInput('periodTo2',label="data końca badanego okresu [yyyy-mm-dd] ",value="2021-12-01"),
                          textInput('compareFrom2',label="data początku okresu porównania [yyyy-mm-dd]",value="2015-01-01"),
                          textInput('compareTo2',label="data końca okresu porównania [yyyy-mm-dd]",value="2019-12-31"),
                          submitButton(text="wyślij zapytanie")
                   ),
                   column(9,
                          htmlOutput("mapaeu"),
                          htmlOutput("mapaeu2")
                          
                   )
                 )
                 ),
        tabPanel("Szeregi czasowe GUS",
                 fluidRow(
                   column(3,
                          selectizeInput('sexgus', 'wybierz płeć',
                                         choices = c("Mezczyzna","Kobieta","Ogolem"),
                                         multiple = FALSE,
                                         selected = "Ogolem",
                                         options = list()
                          ),
                          selectizeInput('wojew', 'wybierz województwo',
                                         choices =c("Polska","Małopolskie","Śląskie","Wielkopolskie","Zachodniopomorskie","Lubuskie","Dolnośląskie","Opolskie","Kujawsko-Pomorskie","Warmińsko-Mazurskie","Pomorskie","Łódzkie","Świętokrzyskie","Lubelskie","Podkarpackie","Podlaskie","Makroregion Województwo Mazowieckie"),
                                         multiple = FALSE,
                                         selected = "Polska",
                                         options = list()
                          ),
                          selectizeInput('wiek', 'wybierz grupę wiekową',
                                         choices =c("0 - Inf","5 - 9"),
                                         multiple = FALSE,
                                         selected = "0 - Inf",
                                         options = list()
                          ),
                          textInput('periodFromg',label="data początku badanego okresu [yyyy-mm-dd]",value="2020-01-01"),
                          textInput('periodTog',label="data końca badanego okresu [yyyy-mm-dd] ",value="2021-12-01"),
                          textInput('compareFromg',label="data początku okresu porównania [yyyy-mm-dd]",value="2015-01-01"),
                          textInput('compareTog',label="data końca okresu porównania [yyyy-mm-dd]",value="2019-12-31"),
                          submitButton(text="wyślij zapytanie")
                          
                   ),
                   column(9,
                          plotlyOutput("xts_gus"),
                          plotlyOutput("xts_gus_relative")   
                   )
                 )),
        tabPanel("Szeregi czasowe EU",
                 fluidRow(
                     column(3,
                            selectizeInput('sex1', 'wybierz płeć',
                                           choices = c("T","F","M"),
                                           multiple = FALSE,
                                           selected = "T",
                                           options = list()
                            ),
                            selectizeInput('country1', 'wybierz kraj',
                                           choices = c('Albania', 'Andorra', 'Armenia', 'Austria', 'Belgium', 'Bulgaria', 'Croatia', 'Cyprus', 'Czechia', 'Denmark', 'Estonia', 'Finland', 'France', 'Georgia', 'Germany', 'Greece', 'Hungary', 'Iceland', 'Ireland', 'Italy', 'Latvia', 'Liechtenstein', 'Lithuania', 'Luxembourg', 'Malta', 'Montenegro', 'Netherlands', 'Norway', 'Poland', 'Portugal', 'Romania', 'Serbia', 'Slovakia', 'Slovenia', 'Spain', 'Sweden', 'Switzerland', 'United Kingdom'),
                                           multiple = FALSE,
                                           selected = "Poland",
                                           options = list()
                            ),
                            textInput('periodFrom',label="data początku badanego okresu [yyyy-mm-dd]",value="2020-01-01"),
                            textInput('periodTo',label="data końca badanego okresu [yyyy-mm-dd] ",value="2021-12-01"),
                            textInput('compareFrom',label="data początku okresu porównania [yyyy-mm-dd]",value="2015-01-01"),
                            textInput('compareTo',label="data końca okresu porównania [yyyy-mm-dd]",value="2019-12-31"),
                            submitButton(text="wyślij zapytanie")
                            
                            ),
                     column(9,
                            plotlyOutput("xts_euro"),
                            plotlyOutput("xts_euro_relative")
                            )
                 ))
    )
    
        )



### SERVER
server <- function(input, output, session) {
    observeEvent(input$dataEURO, {
        message("running script.R")
        source("eurostat.R")
    }
    )
    observeEvent(input$dataGUS, {
        message("running script.R")
        source("gus.R")
    }
    )
    observeEvent(input$bazaEURO, {
        message("running script.R")
        source("sqlE.R")
    }
    )
    observeEvent(input$bazaGUS, {
        message("running script.R")
        source("sqlG.R")
    }
    )
    sqlVar <- reactiveValues(
        sqlText = NULL
    )
    
    observeEvent(input$query,{
        sqlVar$sqlText <- input$query
    })
    
    output$plainSQLText <- renderPrint({
        return(cat(paste(sqlVar$sqlText,"\n")))
    })
    
    myData <- reactive({
        query <- input$query
        res <- dbGetQuery(conn=con,query)
        res
    }
    )
    
    plotlogic <- reactive({
      columns <- colnames(myData())
      ordered <- sort(columns)
      if (length(ordered)==3)
      {
        if(ordered[3]=="Tydzien")
        {
          if(ordered[1]=="Liczba"){
            ###RENDER PLOT
            frame <- myData()
            castframe <- frame[,order(colnames(frame))]
            plotgg <- ggplot(castframe,aes(x=Tydzien,y=Liczba,col=castframe[,2])) + geom_line()
            return(plotgg)
            
          } else if (ordered[2]=="Liczba") {
            frame <- myData()
            castframe <- frame[,order(colnames(frame))]
            plotgg <- ggplot(castframe,aes(x=Tydzien,y=Liczba,col=castframe[,1])) + geom_line()
            plotgg
          }  
        } else if (ordered[3]=="Week" && ordered[2]=="value")
        {
          frame <- myData()
          castframe <- frame[,order(colnames(frame))]
          plotgg <- ggplot(castframe,aes(x=Week,y=value,colour=castframe[,1])) + geom_line()
          plotgg
          
        }
      }
    })
    
    output$out <- renderPlotly({ggplotly({plotlogic()})})
    
    EuroTS1 <- reactive({
        sex <- input$sex1
        country <- input$country1
        periodFrom <- as.Date(input$periodFrom)
        periodTo <- as.Date(input$periodTo)
        baseDateFrom <- paste0(ISOweek(periodFrom),"-4")
        baseDateTo <- paste0(ISOweek(periodTo),"-4")
        euroquery1 <- paste0("select * from zgony_wg_tygodni_eurostat where sex='",sex,"' and countries='",country,"' and Week between '",baseDateFrom,"' and '",baseDateTo,"'")
        result <- dbGetQuery(conn=con,euroquery1)
        baseres <- na.omit(result)
        series <- as.xts(x=baseres$value,order.by=ISOweek2date(baseres$Week))
        as.data.frame(series)
    }
    )
    
    EuroTS2 <- reactive({
        sex <- input$sex1
        country <- input$country1
        compareFrom <- as.Date(input$compareFrom)
        compareTo <- as.Date(input$compareTo)
        compareDateFrom <- paste0(ISOweek(compareFrom),"-4")
        compareDateTo <- paste0(ISOweek(compareTo),"-4")
        euroquery2 <- paste0("select * from zgony_wg_tygodni_eurostat where sex='",sex,"' and countries='",country,"' and Week between '",compareDateFrom,"' and '",compareDateTo,"'")
        result <- dbGetQuery(conn=con,euroquery2)
        mean_all <- mean(result$value)
        mean_all
    }
    )
    
    
    Euro_comparative <- reactive({
      compar <- EuroTS1()/EuroTS2()
      compar
    }
    )
    
    gusTS1 <- reactive({
      sex <- input$sexgus
      country <- input$wojew
      age <- input$wiek
      periodFrom <- as.Date(input$periodFromg)
      periodTo <- as.Date(input$periodTog)
      baseDateFrom <- paste0(ISOweek(periodFrom),"-4")
      baseDateTo <- paste0(ISOweek(periodTo),"-4")
      gusquery1 <- paste0("select * from zgony_wg_tygodni where Plec='",sex,"' and Grupa_wiekowa='",age,"' and Region='",country,"' and Tydzien between '",baseDateFrom,"' and '",baseDateTo,"'")
      result <- dbGetQuery(conn=con,gusquery1)
      baseres <- na.omit(result)
      series <- as.xts(x=baseres$Liczba,order.by=ISOweek2date(baseres$Tydzien))
      as.data.frame(series)
    }
    )
    
    gusTS2 <- reactive({
      sex <- input$sexgus
      country <- input$wojew
      age <- input$wiek
      compareFrom <- as.Date(input$compareFromg)
      compareTo <- as.Date(input$compareTog)
      compareDateFrom <- paste0(ISOweek(compareFrom),"-4")
      compareDateTo <- paste0(ISOweek(compareTo),"-4")
      gusquery2 <- paste0("select * from zgony_wg_tygodni where Plec='",sex,"' and Grupa_wiekowa='",age,"' and Region='",country,"' and Tydzien between '",compareDateFrom,"' and '",compareDateTo,"'")
      result <- dbGetQuery(conn=con,gusquery2)
      mean_all <- mean(result$Liczba)
      mean_all
    }
    )
    
    
    gus_comparative <- reactive({
      compar <- gusTS1()/gusTS2()
      compar
    }
    )
    
    EuroMap1 <- reactive({
        periodFrom <- as.Date(input$periodFrom2)
        periodTo <- as.Date(input$periodTo2)
        baseDateFrom <- paste0(ISOweek(periodFrom),"-4")
        baseDateTo <- paste0(ISOweek(periodTo),"-4")
        euroquery3 <- paste0("select value, Week, countries from zgony_wg_tygodni_eurostat where sex='T' and Week between '",baseDateFrom,"' and '",baseDateTo,"'")
        basemapres <- dbGetQuery(conn=con,euroquery3)
        basemapres
    }
    )
    EuroMap2 <- reactive({
        compareFrom <- as.Date(input$compareFrom2)
        compareTo <- as.Date(input$compareTo2)
        compareDateFrom <- paste0(ISOweek(compareFrom),"-4")
        compareDateTo <- paste0(ISOweek(compareTo),"-4")
        euroquery4 <- paste0("select value, Week, countries from zgony_wg_tygodni_eurostat where sex='T' and Week between '",compareDateFrom,"' and '",compareDateTo,"'")
        compmapres <- dbGetQuery(conn=con,euroquery4)
        meanmap <- mean(compmapres$value)
        meanmap
        
    }
    )
    Euro_map_comparative <- reactive({
      compar <- EuroMap1()/EuroMap2()
      compar
    }
    )
    
    gusMap1 <- reactive({
      periodFrom <- as.Date(input$periodFrom2g)
      periodTo <- as.Date(input$periodTo2g)
      baseDateFrom <- paste0(ISOweek(periodFrom),"-4")
      baseDateTo <- paste0(ISOweek(periodTo),"-4")
      gusquery3 <- paste0("select Liczba, Tydzien, Region from zgony_wg_tygodni where Plec='Ogolem' and Grupa_wiekowa='0 - Inf' and Tydzien between '",baseDateFrom,"' and '",baseDateTo,"'")
      basemapres <- dbGetQuery(conn=con,gusquery3)
      basemapres$Liczba <- as.numeric(basemapres$Liczba)
      basemapres$Region <- as.character(basemapres$Region)
      basemapres
    }
    )
    gusMap2 <- reactive({
      compareFrom <- as.Date(input$compareFrom2g)
      compareTo <- as.Date(input$compareTo2g)
      compareDateFrom <- paste0(ISOweek(compareFrom),"-4")
      compareDateTo <- paste0(ISOweek(compareTo),"-4")
      gusquery4 <- paste0("select Liczba, Tydzien, Region from zgony_wg_tygodni where Plec='Ogolem' and Grupa_wiekowa='0 - Inf' and Tydzien between '",compareDateFrom,"' and '",compareDateTo,"'")
      compmapres <- dbGetQuery(conn=con,gusquery4)
      meanmap <- mean(compmapres$Liczba)
      meanmap
      
    }
    )
    gus_map_comparative <- reactive({
      compar <- gusMap1()/gusMap2()
      na.omit(compar)
    }
    )
    
    output$mapaeu <- renderGvis({
      gvisGeoMap(EuroMap1(), locationvar='countries', numvar=as.numeric('value'))
    })
    ####output$mapaeu2 <- renderGvis({
      ####gvisGeoMap(Euro_map_comparative(), locationvar='countries', numvar=as.numeric('value'),
                 ####options=list())
    ###})
    
    output$mapagus <- renderGvis({
      gvisGeoMap(gusMap1(), locationvar='Region', numvar=as.numeric('Liczba'),
                 options=list())
    })
    ####output$mapagus2 <- renderGvis({
      ####gvisGeoMap(gus_map_comparative(), locationvar='Region', numvar=as.numeric('Liczba'),
                 ####options=list())
    ####})
    
    
    
    output$xts_euro <- renderPlotly({
      plot_ly(
        data=(EuroTS1()),
        x=row.names(data), y=~V1,
        type = 'scatter', mode = 'lines', fill = 'tozeroy'
      )})
    output$xts_euro_relative <- renderPlotly({
      plot_ly(
        data=as.data.frame(Euro_comparative()),
        x=row.names(data),y=~V1,
        type = 'scatter', mode = 'lines', fill = 'tozeroy'
        )})
    output$xts_gus <- renderPlotly({
      plot_ly(
        data=(gusTS1()),
        x=row.names(data), y=~V1,
        type = 'scatter', mode = 'lines', fill = 'tozeroy'
      )})
    output$xts_gus_relative <- renderPlotly({
      plot_ly(
        data=as.data.frame(gus_comparative()),
        x=row.names(data),y=~V1,
        type = 'scatter', mode = 'lines', fill = 'tozeroy'
      )})
    
    output$tbTable<- DT::renderDataTable({
        DT::datatable(myData(), options = list(lengthMenu = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100), pageLength = 10))
    })
    
    
    
    output$download <- downloadHandler(filename = function() {paste(Sys.Date(),"wynikzapytania",".csv")}, 
                                       content = function(file) {write.csv(myData(), file)})
    
    ###DISCONNECT!
    session$onSessionEnded(function(){dbDisconnect(con)})
}

# Run the application 
shinyApp (ui = ui, server = server)
