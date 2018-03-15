library(shiny)
library(shinythemes)
library(graphics)
library(ggplot2)
library(plotrix)
ui <- fluidPage(theme=shinytheme("superhero"),
   
   titlePanel("Color Picker"),
   plotOutput("SV",click="sv_click"),
   plotOutput("hue",click="h_click"),
   uiOutput("det")
)

server <- function(input, output) {
  output$SV<-renderPlot({
    h=input[["h_click"]]$x
    if(is.null(h))h=0
    color=c()
    blck=1/100
    lasty=0
    plot(1,1,type="n",xaxt="n",yaxt="n",xaxs="i",yaxs="i",xlim=c(0,1),ylim=c(0,1),xlab = "",ylab = "",frame.plot = F)
    for(i in 0:100)
    {
      i=as.double(i*blck)
      color=hsv(seq(h,h,length=100),seq(i,i,length=100),seq(0,1,length=100))
      gradient.rect(0,lasty,1,lasty+blck,col=color,border=NA)
      lasty=lasty+blck
    }

  })
  
  output$hue<-renderPlot({
    plot(c(0,1),c(0,1),type="n",xlim = c(0,1),ylim=c(0,1),xlab = "",ylab = "",xaxt="n",yaxt="n",xaxs="i",yaxs="i")
    color=hsv(seq(0,1,length=360),seq(1,1,length=360),seq(1,1,length=360))
    gradient.rect(0,0,1,1,col=color)
  },height=150)
  
  output$det<-renderUI({
    paste(input[["sv_click"]]$x," y = ",input[["sv_click"]]$y,sep="  ")
  })   
   
}

shinyApp(ui = ui, server = server)

