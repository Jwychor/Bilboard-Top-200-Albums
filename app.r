####Packages#####
library(shiny)
library(shinythemes)
library(plotly)
library(shinydashboard)
library(tidyverse)
library(reshape2)
library(rvest)
library(naniar)
library(magrittr)
library(DT)
library(colourpicker)

####Server####
###Server
server<- function(input,output,session){
  
  ##Data
  
  node_selector<- function(node){
    musicpage() %>%
      html_nodes(node) %>%
      html_text() %>%
      return(.)
  }
  
  mp100<- read_html("https://www.billboard.com/charts/hot-100")
  mp200<- read_html("https://www.billboard.com/charts/billboard-200")
  
  musicpage<- reactive({
    if(input$Source == "Hot 100"){
      return(mp100)
    } else {
      return(mp200)
    }
  })
  
  week_of<- reactive({
    week<- node_selector(".button--link") %>% as.character()
    return(week)
  })
  
  music_df<- reactive({
    
    song_rank<- node_selector('.chart-element__rank__number') %>% as.numeric()
    
    song_name<- node_selector('.color--primary') %>%
      .[c(3:length(.))]
    
    artist_name<- node_selector('.color--secondary') %>%
      .[!grepl("^[0-9]+$|\\n|^[-]$|^Steady$",.)]
    
    last_week_rank<- node_selector('.text--last') %>%
      .[c(F,T)] %>%
      str_replace("-",as.character(length(song_name) + 1)) %>%
      as.numeric()
    
    peak_rank<- node_selector('.text--peak') %>%
      .[c(F,T)] %>%
      as.numeric()
    
    rank_duration<- node_selector('.text--week') %>%
      .[c(F,T)] %>%
      as.numeric()
    
    last_week_delta<- as.numeric(last_week_rank - song_rank)
    
    music_dfe<- data.frame(song_name, artist_name, song_rank, 
                           last_week_rank, last_week_delta, peak_rank,
                           rank_duration
    )
    
    colnames(music_dfe)<-c("Song Name", "Artist Name", "Song Rank", 
                           "Last Week Rank", "Change from Last Week",
                           "Peak Rank", "Chart Duration" 
    )
    return(music_dfe)
  })
  
  
  ##RenderUIs
  NumColumns<- reactive({colnames(select_if(music_df(), is.numeric))})
  output$Xvars<- renderUI({
    list(
      selectInput("Xvar", "X-Variable",choices = NumColumns())
    )
  })
  
  output$Yvars<- renderUI({
    list(
      selectInput("Yvar", "Y-Variable",choices = NumColumns())
    )
  })
  
  ##Outputs
  output$week_of<- renderText({week_of()})
  
  output$MusicTable<- DT::renderDataTable({
    music_df2<- music_df()
    colnames(music_df2) <- paste0('<span style="color:',c(rep("white",ncol(music_df()))),'">',colnames(music_df()),'</span>')
    DT::datatable(music_df2, escape = F,
                  options = list(pageLength = 10,
                                 lengthMenu = c(10,20,50,100,200))) %>%
      formatStyle('<span style="color:white">Change from Last Week</span>', 
                  backgroundColor = styleInterval(c(-1,0), c("red","white","chartreuse")),
                  fontWeight = 'Bold'
      )
  })
  
  output$MusicPlot<- renderPlotly({
    p<- music_df() %>% plot_ly(type = 'scatter',
                               mode = 'markers',
                               x = ~get(input$Xvar),
                               y = ~get(input$Yvar)
    ) 
    p<- p %>% add_markers(
      marker = list(color = input$DotColor),
      text = ~paste(
        paste0("Rank: ", `Song Rank`), 
        `Song Name`, 
        paste0("By: ", `Artist Name`),
        sep = "<br />"),
      hoverinfo = 'text'
    ) %>%
      layout(title = input$Source,
             xaxis = list(title = input$Xvar),
             yaxis = list(title = input$Yvar))
  })
}

####UI#####
###UI

ui<- fluidPage(
  theme = shinytheme("superhero"),
  
  tags$head(
    tags$style(
      HTML('
           hr{
      	border: 1px solid white;
      }
      
      h1{
      	font-size: 56px;
      	font-weight: 700;
      	text-align: center;
      }
      
      table.table.dataTable tbody tr.active td {
      	color: unset !important;
      }
      
      label, th, thead, .paginate_button{
      	color: white !important;
      }
      
      select{
      	color: black;
      }
      
      .dataTables_wrapper{
        min-height: 600px !important;
      }
      
      .svg-container{
        min-height: 400px !important;
      }
      '
      )
    )
  ),
  
  headerPanel(
    wellPanel(
    list(HTML('<a href="https://github.com/jwychor"><img src="https://i.ibb.co/n3r8vLx/Logo.png" alt="Logo" border="0" style="height: 100px; width: 100px;" /></a>','Top Bilboard Data',
              HTML(paste('<br /><h2>for the week of', textOutput('week_of'),'</h3>')),
              HTML('<br /><h4><a href="https://github.com/Jwychor/Graphing-Top-Bilboard-Data">Source Code</a></h4>'))
         ),
    tags$hr()
    )
  ),
  wellPanel(
             fluidRow(
               column(2,
                      radioButtons("Source",
                                   HTML("<h3>Source</h3>"),
                                   choices = list(
                                     "Hot 100",
                                     "Bilboard Top 200"
                                   ))
                      ),
               column(10,
                      div(DT::DTOutput(outputId = 'MusicTable'),
                          style = "font-size: 100%; width: 100%")
               ),
             )
    ),
    wellPanel(tags$style('min-height: 800px;'),
             fluidRow(
               column(9,
                      titlePanel("Weekly Figures"),
                      plotlyOutput("MusicPlot")
               ),
               column(3,
                      h2("Interactive Chart Values"),
                      colourInput("DotColor","Dot Color","black"),
                      uiOutput("Xvars"),
                      uiOutput("Yvars")
               ))),
    wellPanel(
      headerPanel(
        wellPanel("Sources")
        ),
      HTML('
      <h3><a href="https://www.billboard.com/charts/hot-100">https://www.billboard.com/charts/hot-100</a>
            <br />
           <h3><a href="https://www.billboard.com/charts/billboard-200">https://www.billboard.com/charts/billboard-200</a>'
           )
    )
  )

shinyApp(ui, server)
