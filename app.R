library('shiny')
library('shinydashboard')
library('ggplot2')
library('dplyr')
library('DT')

title <- tags$p(icon('globe',class = 'fa-spin'),'Global LIfe exp and pop data')

file = read.csv('gapminder.csv')
countries = read.csv('countries.csv')
github <- tags$a(href = 'http://github.com/preciousNliwasa','My github profile')
linkedin <- tags$a(href = 'https://www.linkedin.com/in/precious-nliwasa-5b2257192/','My linkedin profile')
kaggle <- tags$a(href = 'https://www.kaggle.com/preciousnliwasa','My kaggle profile')

ui <- fluidPage(
  dashboardPage(skin = 'red',
    dashboardHeader(title = title,titleWidth = 400),
    dashboardSidebar(sidebarMenu(
      menuItem('Data',tabName = 'data',icon = icon('home')),
      menuItem('Visualisation',tabName = 'visualise',icon = icon('bar-chart')),
      menuItem('Kmeans',tabName = 'linear',icon = icon('pen')),
      menuItem('About',tabName = 'about',icon = icon('user'))
    )),
    dashboardBody(tags$head(tags$style(' h3 {text-align:center;}
                                         a {color:black;}')),tabItems(
      tabItem('data',fluidRow(
        column(6,box(title = 'Table',collapsible = T,height = 610,width = 500,solidHeader = T,status = ('danger'),DT::dataTableOutput('dt'))),
        column(3,
               box(title = 'Controls',collapsible = T,width = 200,height = 393,background = 'red',
                   selectInput('continent','continent',choices = c('All','Africa','Americas','Asia','Europe','Oceania')),
                   selectInput('country','country',choices = countries$countries),
                   selectInput('year','year',choices = c('All','1952','1957','1962','1967','1972','1977','1982','1987','1992','1997','2002','2007')),
                   selectInput('field2','field',choices = c('Life Expectacy','Population','GDP Per Cap'))),
               infoBoxOutput('maximum',width = 50),
               infoBoxOutput('minimum',width = 50)),
        column(3,box(title = 'About Data',width = 200,height = 393,background = 'black',collapsible = T,tags$h4('Info'),tags$p('Dataset source : plotly datasets',tags$br(),'Columns : 6 (including continent)',tags$br(), 'Countries used : 142'),tags$h4('Note'),tags$p('field control applicable on infobox only')))
        )),
      tabItem('visualise',tabsetPanel(
        tabPanel('Barplot',fluidRow(column(3,tags$br(),tags$br(),box(background = 'maroon',height = 400,width = 300,selectInput('countries','country',choices = countries$countries[-1]))),column(9,tabsetPanel(
          tabPanel('Life Expectacy',plotOutput('bar1')),
          tabPanel('Population',plotOutput('bar2')),
          tabPanel('GDP Per Cap',plotOutput('bar3')))
        ))),
        tabPanel('Histogram',
                 fluidRow(column(4,box(background = 'maroon',width = 200,selectInput('fields','field',choices = c('Life Expectacy','Population','GDP Per Cap')))),column(4,box(background = 'maroon',width = 200,selectInput('contii','continent',choices = c('Africa','Americas','Asia','Europe','Oceania')))),column(4,box(background = 'maroon',width = 200,selectInput('yea','year',choices = seq(from = 1952,to = 2007,by = 5)))),
                 fluidRow(plotOutput('lastplot'))
                 )
      ),
      tabPanel('Boxplot',fluidRow(column(3,box(background = 'maroon',height = 400,width = 200,selectInput('fiel','field',choices = c('Life Expectacy','Population','GDP Per Cap')),selectInput('yea2','year',choices = c('1952','1957','1962','1967','1972','1977','1982','1987','1992','1997','2002','2007')))),column(9,plotOutput('boxp')))))),
      tabItem('linear',fluidRow(
        column(3,tags$br(),tags$br(),box(height = 400,width = 200,
          selectInput('yearr','year',choices = seq(from = 1952,to = 2007,by = 5)),
          sliderInput('clusters','number of clusters',value = 3,min = 1,max = 6)
        )),
        column(9,tabsetPanel(
          tabPanel('Centers',verbatimTextOutput('prin')),
          tabPanel('Clustered Data',DT::dataTableOutput('dt3')),
          tabPanel('Plotted',plotOutput('plo6'))
        )))),
      tabItem('about',fluidRow(column(5,offset = 3,box(title = tags$h3('Precious Nliwasa'),background = 'black',width = 100,height = 500,
                                                       fluidRow(column(8,offset = 2,infoBox(title = 'Github',value = github,icon = icon('github',class = 'fa-spin'),color = 'teal',width = 50,fill = T))),
                                                       fluidRow(column(8,offset = 2,infoBox('Linkedin',linkedin,icon = icon('linkedin'),color = 'olive',width = 50,fill = T))),
                                                       fluidRow(column(8,offset = 2,infoBox('kaggle',kaggle,icon = icon('kaggle'),color = 'light-blue',width = 50,fill = T)))))))
    ))
    
  )
)
server <- shinyServer(function(input,output){
  modell <- reactive({
    kmeans(filter(file,year == input$yearr)[,c('lifeExp','gdpPercap')],input$clusters)
  })
  
  dt3 <- reactive({
    labels <- modell()$cluster
    df <- select(filter(file,year == input$yearr),country,lifeExp,gdpPercap)
    labels2 <- paste('category',labels)
    df[,'category'] = labels2
    df
  })
  
  output$plo6 <- renderPlot({
    ggplot(dt3(),aes(lifeExp,gdpPercap,color = category)) + geom_point(size = 3) + theme(panel.background = element_rect(fill = 'peachpuff'))
  })
  
  output$dt3 <- DT::renderDataTable(dt3())
  
  output$prin <- renderPrint({
    modell()$centers
  })
  
  output$boxp <- renderPlot({
    if (input$fiel == 'Life Expectacy'){
      ggplot(filter(file,year == input$yea2),aes(continent,lifeExp,fill = continent)) + geom_boxplot() + theme(panel.background = element_rect(fill = 'peachpuff')) 
    }else if(input$fiel == 'Population'){
      ggplot(filter(file,year == input$yea2),aes(continent,pop,fill = continent)) + geom_boxplot() + theme(panel.background = element_rect(fill = 'peachpuff'))
    }else{
      ggplot(filter(file,year == input$yea2),aes(continent,gdpPercap,fill = continent)) + geom_boxplot() + theme(panel.background = element_rect(fill = 'peachpuff'))
    }
  })
  
  output$lastplot <- renderPlot({
    if (input$fields == 'Life Expectacy'){
      ggplot(filter(file,continent == input$contii & year ==input$yea),aes(lifeExp)) + geom_histogram(bins = 10,color = 'white',fill = 'maroon') + theme(panel.background = element_rect(fill = 'peachpuff'))
    }else if(input$fields == 'Population'){
      ggplot(filter(file,continent == input$contii & year == input$yea),aes(pop)) + geom_histogram(bins = 10,color = 'white',fill = 'maroon') + theme(panel.background = element_rect(fill = 'peachpuff'))
    }else{
      ggplot(filter(file,continent == input$contii & year ==input$yea),aes(gdpPercap)) + geom_histogram(bins = 10,color = 'white',fill = 'maroon') + theme(panel.background = element_rect(fill = 'peachpuff'))
    }
    
  })
  
  output$bar3 <- renderPlot({
    
    ggplot(filter(file,country == input$countries),aes(as.factor(year),gdpPercap))  + geom_col(fill = 'maroon') + labs(x = 'Years',y = 'GDP Per Cap') + theme(axis.text.x = element_text(angle = 90)) + theme(panel.background = element_rect(fill = 'peachpuff'))
    
  })
  output$bar2 <- renderPlot({
    
    ggplot(filter(file,country == input$countries),aes(as.factor(year),pop))  + geom_col(fill = 'maroon') + labs(x = 'Years',y = 'Population') + theme(axis.text.x = element_text(angle = 90)) + theme(panel.background = element_rect(fill = 'peachpuff'))
    
  })
  output$bar1 <- renderPlot({
    
      ggplot(filter(file,country == input$countries),aes(as.factor(year),lifeExp))  + geom_col(fill = 'maroon') + labs(x = 'Years',y = 'Life Expectacy') + theme(axis.text.x = element_text(angle = 90)) + theme(panel.background = element_rect(fill = 'peachpuff'))
    
  })

  
  output$minimum <- renderInfoBox({
    if (input$continent == 'All' & input$country == 'All' & input$year == 'All'){
      foinf = select(file,country,year,lifeExp,pop,gdpPercap)
      if (input$field2 == 'Life Expectacy'){
        min = min(foinf$lifeExp)
        sub = select(filter(foinf,lifeExp == min),country)
        yea = select(filter(foinf,lifeExp == min),year)
        gg = infoBox(title = 'Minimum',value = min,subtitle = paste(sub[1,1],yea[1,1]),icon = icon('line-chart'),color = 'teal',width = 100,fill = T)
      }else if(input$field2 == 'Population'){
        min = min(foinf$pop)
        sub = select(filter(foinf,pop == min),country)
        yea = select(filter(foinf,pop == min),year)
        gg = infoBox(title = 'Minimum',value = min,subtitle = paste(sub[1,1],yea[1,1]),icon = icon('line-chart'),color = 'teal',width = 500,fill =T)
      }else{
        min = min(foinf$gdpPercap)
        sub = select(filter(foinf,gdpPercap == min),country)
        yea = select(filter(foinf,gdpPercap == min),year)
        gg = infoBox(title = 'Minimum',value = min,subtitle = paste(sub[1,1],yea[1,1]),icon = icon('line-chart'),color = 'teal',width = 50,fill = T)
      }
      gg
      
    }else if (input$continent == 'All' & input$country == 'All' & input$year != 'All') {
      foinf = select(filter(file,year == input$year),country,year,lifeExp,pop,gdpPercap)
      if (input$field2 == 'Life Expectacy'){
        min = min(foinf$lifeExp)
        sub = select(filter(foinf,lifeExp == min),country)
        gg = infoBox(title = 'Minimum',value = min,subtitle = sub[1,1],icon = icon('line-chart'),color = 'teal',fill = T)
      }else if(input$field2 == 'Population'){
        min = min(foinf$pop)
        sub = select(filter(foinf,pop == min),country)
        gg = infoBox(title = 'Minimum',value = min,subtitle = sub[1,1],icon = icon('line-chart'),color = 'teal',fill =T)
      }else{
        min = min(foinf$gdpPercap)
        sub = select(filter(foinf,gdpPercap == min),country)
        gg =infoBox(title = 'Minimum',value = min,subtitle = sub[1,1],icon = icon('line-chart'),color = 'teal',fill = T)
      }
      gg
    }else if (input$continent == 'All' & input$country != 'All' & input$year == 'All'){
      foinf = select(filter(file,country == input$country),country,year,lifeExp,pop,gdpPercap)
      if (input$field2 == 'Life Expectacy'){
        min = min(foinf$lifeExp)
        yea = select(filter(foinf,lifeExp == min),year)
        gg = infoBox(title = 'Minimum',value = min,subtitle = yea[1,1],icon = icon('line-chart'),color = 'teal',fill = T)
      }else if(input$field2 == 'Population'){
        min = min(foinf$pop)
        yea = select(filter(foinf,pop == min),year)
        gg = infoBox(title = 'Minimum',value = min,subtitle = yea[1,1],icon = icon('line-chart'),color = 'teal',fill =T)
      }else{
        min = min(foinf$gdpPercap)
        yea = select(filter(foinf,gdpPercap == min),year)
        gg = infoBox(title = 'Minimum',value = min,subtitle = yea[1,1],icon = icon('line-chart'),color = 'teal',fill = T)
      }
      gg
    }else if (input$continent != 'All' & input$country == 'All' & input$year == 'All'){
      foinf = select(filter(file,continent == input$continent),country,year,lifeExp,pop,gdpPercap)
      if (input$field2 == 'Life Expectacy'){
        min = min(foinf$lifeExp)
        sub = select(filter(foinf,lifeExp == min),country)
        yea = select(filter(foinf,lifeExp == min),year)
        gg = infoBox(title = 'Minimum',value = min,subtitle = paste(sub[1,1],yea[1,1]),icon = icon('line-chart'),color = 'teal',fill = T)
      }else if(input$field2 == 'Population'){
        min = min(foinf$pop)
        sub = select(filter(foinf,pop == min),country)
        yea = select(filter(foinf,pop == min),year)
        gg = infoBox(title = 'Minimum',value = min,subtitle = paste(sub[1,1],yea[1,1]),icon = icon('line-chart'),color = 'teal',fill =T)
      }else{
        min = min(foinf$gdpPercap)
        sub = select(filter(foinf,gdpPercap == min),country)
        yea = select(filter(foinf,gdpPercap == min),year)
        gg =infoBox(title = 'Minimum',value = min,subtitle = paste(sub[1,1],yea[1,1]),icon = icon('line-chart'),color = 'teal',fill = T)
      }
      gg
    }else if (input$continent == 'All' & input$country != 'All' & input$year != 'All') {
      foinf = select(filter(file,year == input$year & country == input$country),country,year,lifeExp,pop,gdpPercap)
      if (input$field2 == 'Life Expectacy'){
        min = min(foinf$lifeExp)
        gg = infoBox(title = 'Minimum',value = min,icon = icon('line-chart'),color = 'teal',fill = T)
      }else if(input$field2 == 'Population'){
        min = min(foinf$pop)
        gg = infoBox(title = 'Minimum',value = min,icon = icon('line-chart'),color = 'teal',fill =T)
      }else{
        min = min(foinf$gdpPercap)
        gg =infoBox(title = 'Minimum',value = min,icon = icon('line-chart'),color = 'teal',fill = T)
      }
      gg
    }else if (input$continent != 'All' & input$country != 'All' & input$year == 'All'){
      foinf = select(filter(file,country == input$country & continent == input$continent),country,year,lifeExp,pop,gdpPercap)
      if (input$field2 == 'Life Expectacy'){
        min = min(foinf$lifeExp)
        yea = select(filter(foinf,lifeExp == min),year)
        gg = infoBox(title = 'Minimum',value = min,subtitle = yea[1,1],icon = icon('line-chart'),color = 'teal',fill = T)
      }else if(input$field2 == 'Population'){
        min = min(foinf$pop)
        yea = select(filter(foinf,pop == min),year)
        gg = infoBox(title = 'Minimum',value = min,subtitle = yea[1,1],icon = icon('line-chart'),color = 'teal',fill =T)
      }else{
        min = min(foinf$gdpPercap)
        yea = select(filter(foinf,gdpPercap == min),year)
        gg = infoBox(title = 'Minimum',value = min,subtitle = yea[1,1],icon = icon('line-chart'),color = 'teal',fill = T)
      }
        gg
    }else if (input$continent != 'All' & input$country == 'All' & input$year != 'All'){
      foinf = select(filter(file,continent == input$continent & year == input$year),country,year,lifeExp,pop,gdpPercap)
      if (input$field2 == 'Life Expectacy'){
        min = min(foinf$lifeExp)
        sub = select(filter(foinf,lifeExp == min),country)
        gg = infoBox(title = 'Minimum',value = min,subtitle = sub[1,1],icon = icon('line-chart'),color = 'teal',fill = T)
      }else if(input$field2 == 'Population'){
        min = min(foinf$pop)
        sub = select(filter(foinf,pop == min),country)
        gg = infoBox(title = 'Minimum',value = min,subtitle = sub[1,1],icon = icon('line-chart'),color = 'teal',fill =T)
      }else{
        min = min(foinf$gdpPercap)
        sub = select(filter(foinf,gdpPercap == min),country)
        gg =infoBox(title = 'Minimum',value = min,subtitle = sub[1,1],icon = icon('line-chart'),color = 'teal',fill = T)
      }
      gg
    }else{
      foinf = select(filter(file,continent == input$continent & year == input$year & country == input$country ),country,year,lifeExp,pop,gdpPercap)
      if (input$field2 == 'Life Expectacy'){
        min = min(foinf$lifeExp)
        gg = infoBox(title = 'Minimum',value = min,icon = icon('line-chart'),color = 'teal',fill = T)
      }else if(input$field2 == 'Population'){
        min = min(foinf$pop)
        gg = infoBox(title = 'Minimum',value = min,icon = icon('line-chart'),color = 'teal',fill =T)
      }else{
        min = min(foinf$gdpPercap)
        gg =infoBox(title = 'Minimum',value = min,icon = icon('line-chart'),color = 'teal',fill = T)
      }
      gg
    }
    
    
  })
  
  output$maximum <- renderInfoBox({
    if (input$continent == 'All' & input$country == 'All' & input$year == 'All'){
      foinf = select(file,country,year,lifeExp,pop,gdpPercap)
      if (input$field2 == 'Life Expectacy'){
        max = max(foinf$lifeExp)
        sub = select(filter(foinf,lifeExp == max),country)
        yea = select(filter(foinf,lifeExp == max),year)
        gg = infoBox(title = 'Maximum',value = max,subtitle = paste(sub[1,1],yea[1,1]),icon = icon('line-chart'),color = 'teal',width = 100,fill = T)
      }else if(input$field2 == 'Population'){
        max = max(foinf$pop)
        sub = select(filter(foinf,pop == max),country)
        yea = select(filter(foinf,pop == max),year)
        gg = infoBox(title = 'Maximum',value = max,subtitle = paste(sub[1,1],yea[1,1]),icon = icon('line-chart'),color = 'teal',width = 500,fill =T)
      }else{
        max = max(foinf$gdpPercap)
        sub = select(filter(foinf,gdpPercap == max),country)
        yea = select(filter(foinf,gdpPercap == max),year)
        gg =infoBox(title = 'Maximum',value = max,subtitle = paste(sub[1,1],yea[1,1]),icon = icon('line-chart'),color = 'teal',width = 50,fill = T)
      }
      gg
      
    }else if (input$continent == 'All' & input$country == 'All' & input$year != 'All') {
      foinf = select(filter(file,year == input$year),country,year,lifeExp,pop,gdpPercap)
      if (input$field2 == 'Life Expectacy'){
        max = max(foinf$lifeExp)
        sub = select(filter(foinf,lifeExp == max),country)
        gg = infoBox(title = 'Maximum',value = max,subtitle = sub[1,1],icon = icon('line-chart'),color = 'teal',fill = T)
      }else if(input$field2 == 'Population'){
        max = max(foinf$pop)
        sub = select(filter(foinf,pop == max),country)
        gg = infoBox(title = 'Maximum',value = max,subtitle = sub[1,1],icon = icon('line-chart'),color = 'teal',fill =T)
      }else{
        max = max(foinf$gdpPercap)
        sub = select(filter(foinf,gdpPercap == max),country)
        gg =infoBox(title = 'Maximum',value = max,subtitle = sub[1,1],icon = icon('line-chart'),color = 'teal',fill = T)
      }
      gg
    }else if (input$continent == 'All' & input$country != 'All' & input$year == 'All'){
      foinf = select(filter(file,country == input$country),country,year,lifeExp,pop,gdpPercap)
      if (input$field2 == 'Life Expectacy'){
        max = max(foinf$lifeExp)
        yea = select(filter(foinf,lifeExp == max),year)
        gg = infoBox(title = 'Maximum',value = max,subtitle = yea[1,1],icon = icon('line-chart'),color = 'teal',fill = T)
      }else if(input$field2 == 'Population'){
        max = max(foinf$pop)
        yea = select(filter(foinf,pop == max),year)
        gg = infoBox(title = 'Maximum',value = max,subtitle = yea[1,1],icon = icon('line-chart'),color = 'teal',fill =T)
      }else{
        max = max(foinf$gdpPercap)
        yea = select(filter(foinf,gdpPercap == max),year)
        gg =infoBox(title = 'Maximum',value = max,subtitle = yea[1,1],icon = icon('line-chart'),color = 'teal',fill = T)
      }
      gg
    }else if (input$continent != 'All' & input$country == 'All' & input$year == 'All'){
      foinf = select(filter(file,continent == input$continent),country,year,lifeExp,pop,gdpPercap)
      if (input$field2 == 'Life Expectacy'){
        max = max(foinf$lifeExp)
        sub = select(filter(foinf,lifeExp == max),country)
        yea = select(filter(foinf,lifeExp == max),year)
        gg = infoBox(title = 'Maximum',value = max,subtitle = paste(sub[1,1],yea[1,1]),icon = icon('line-chart'),color = 'teal',fill = T)
      }else if(input$field2 == 'Population'){
        max = max(foinf$pop)
        sub = select(filter(foinf,pop == max),country)
        yea = select(filter(foinf,pop == max),year)
        gg = infoBox(title = 'Maximum',value = max,subtitle = paste(sub[1,1],yea[1,1]),icon = icon('line-chart'),color = 'teal',fill =T)
      }else{
        max = max(foinf$gdpPercap)
        sub = select(filter(foinf,gdpPercap == max),country)
        yea = select(filter(foinf,gdpPercap == max),year)
        gg =infoBox(title = 'Maximum',value = max,subtitle = paste(sub[1,1],yea[1,1]),icon = icon('line-chart'),color = 'teal',fill = T)
      }
      gg
    }else if (input$continent == 'All' & input$country != 'All' & input$year != 'All') {
      foinf = select(filter(file,year == input$year & country == input$country),country,year,lifeExp,pop,gdpPercap)
      if (input$field2 == 'Life Expectacy'){
        max = max(foinf$lifeExp)
        gg = infoBox(title = 'Maximum',value = max,icon = icon('line-chart'),color = 'teal',fill = T)
      }else if(input$field2 == 'Population'){
        max = max(foinf$pop)
        gg = infoBox(title = 'Maximum',value = max,icon = icon('line-chart'),color = 'teal',fill =T)
      }else{
        max = max(foinf$gdpPercap)
        gg =infoBox(title = 'Maximum',value = max,icon = icon('line-chart'),color = 'teal',fill = T)
      }
      gg
    }else if (input$continent != 'All' & input$country != 'All' & input$year == 'All'){
      foinf = select(filter(file,country == input$country & continent == input$continent),country,year,lifeExp,pop,gdpPercap)
      if (input$field2 == 'Life Expectacy'){
        max = max(foinf$lifeExp)
        yea = select(filter(foinf,lifeExp == max),year)
        gg = infoBox(title = 'Maximum',value = max,subtitle = yea[1,1],icon = icon('line-chart'),color = 'teal',fill = T)
      }else if(input$field2 == 'Population'){
        max = max(foinf$pop)
        yea = select(filter(foinf,pop == max),year)
        gg = infoBox(title = 'Maximum',value = max,subtitle = yea[1,1],icon = icon('line-chart'),color = 'teal',fill =T)
      }else{
        max = max(foinf$gdpPercap)
        yea = select(filter(foinf,gdpPercap == max),year)
        gg =infoBox(title = 'Maximum',value = max,subtitle = yea[1,1],icon = icon('line-chart'),color = 'teal',fill = T)
      }
      gg
    }else if (input$continent != 'All' & input$country == 'All' & input$year != 'All'){
      foinf = select(filter(file,continent == input$continent & year == input$year),country,year,lifeExp,pop,gdpPercap)
      if (input$field2 == 'Life Expectacy'){
        max = max(foinf$lifeExp)
        sub = select(filter(foinf,lifeExp == max),country)
        gg = infoBox(title = 'Maximum',value = max,subtitle = sub[1,1],icon = icon('line-chart'),color = 'teal',fill = T)
      }else if(input$field2 == 'Population'){
        max = max(foinf$pop)
        sub = select(filter(foinf,pop == max),country)
        gg = infoBox(title = 'Maximum',value = max,subtitle = sub[1,1],icon = icon('line-chart'),color = 'teal',fill =T)
      }else{
        max = max(foinf$gdpPercap)
        sub = select(filter(foinf,gdpPercap == max),country)
        gg =infoBox(title = 'Maximum',value = max,subtitle = sub[1,1],icon = icon('line-chart'),color = 'teal',fill = T)
      }
      gg
    }else{
      foinf = select(filter(file,continent == input$continent & year == input$year & country == input$country ),country,year,lifeExp,pop,gdpPercap)
      if (input$field2 == 'Life Expectacy'){
        max = max(foinf$lifeExp)
        gg = infoBox(title = 'Maximum',value = max,icon = icon('line-chart'),color = 'teal',fill = T)
      }else if(input$field2 == 'Population'){
        max = max(foinf$pop)
        gg = infoBox(title = 'Maximum',value = max,icon = icon('line-chart'),color = 'teal',fill =T)
      }else{
        max = max(foinf$gdpPercap)
        gg =infoBox(title = 'Maximum',value = max,icon = icon('line-chart'),color = 'teal',fill = T)
      }
      gg
    }
  
    
  })
  
  output$dt <- DT::renderDataTable({
    
    if ((input$continent == 'All') & (input$country == 'All') & (input$year == 'All')){
      
      select(file,country,year,lifeExp,pop,gdpPercap)
      
    }else if (input$continent == 'All' & input$country == 'All' & input$year != 'All') {
      
      select(filter(file,year == input$year),country,year,lifeExp,pop,gdpPercap)
      
    }else if (input$continent == 'All' & input$country != 'All' & input$year == 'All'){
      
      select(filter(file,country == input$country),country,year,lifeExp,pop,gdpPercap)
      
    }else if (input$continent != 'All' & input$country == 'All' & input$year == 'All'){
      
      select(filter(file,continent == input$continent),country,year,lifeExp,pop,gdpPercap)
      
    }else if (input$continent == 'All' & input$country != 'All' & input$year != 'All') {
      
      select(filter(file,year == input$year & country == input$country),country,year,lifeExp,pop,gdpPercap)
      
    }else if (input$continent != 'All' & input$country != 'All' & input$year == 'All'){
      
      select(filter(file,country == input$country & continent == input$continent),country,year,lifeExp,pop,gdpPercap)
      
    }else if (input$continent != 'All' & input$country == 'All' & input$year != 'All'){
      
      select(filter(file,continent == input$continent & year == input$year),country,year,lifeExp,pop,gdpPercap)
      
    }else{
      
      select(filter(file,continent == input$continent & year == input$year & country == input$country ),country,year,lifeExp,pop,gdpPercap)
      
  }
   
  })
  
})

shinyApp(ui,server)