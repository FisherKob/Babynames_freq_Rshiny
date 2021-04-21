
library(shiny)
library(tidyverse)
library(babynames)
library(shinythemes)

ui<-fluidPage( titlePanel("Welcome! Let's see the frequency of common names of a person since 1880",windowTitle='Babynames'),br(),br(),themeSelector(),
               sidebarLayout(
                 sidebarPanel(
               textInput(inputId='name',
                         label='Name:',value='',placeholder = 'Name'),
               selectInput(inputId = 'sex', label='Sex:',choices = list(Male='M',Female='F'))
               ,sliderInput(inputId = 'year',label='Year Range:',min=min(babynames$year),max=max(babynames$year), value=c(min(babynames$year),max(babynames$year)), sep=''),
               submitButton(text='Create my plot!')
               ,width = 4,br(),br(),),
               
               mainPanel ( br(),br(),br(),
               plotOutput(outputId = 'nameplot',height = '600px',width='40%'),width = 50 )
               
               
               
,position = 'left' ))



server<-function(input,output){
  
  output$nameplot<-renderPlot(
    babynames%>%
      filter(sex==input$sex,name==input$name)%>%
      ggplot(aes(x=year,y=n))+geom_line()+scale_x_continuous(limits=input$year) +theme_minimal()+ylab('Frequency of names')+ggtitle('Nameplot across time period')+ theme_bw() +
      theme(plot.title = element_text(hjust = 0.5))
    
    
  )
  
  
  
}
shinyApp(ui=ui,server=server)

#Requirements-show the # of babies with given name over time 2) allow the user to type a single name 3)allow user to choose the range of years that would like to display 4)allow the user to filter by sex




