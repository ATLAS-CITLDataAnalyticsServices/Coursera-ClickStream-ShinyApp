library(haven)
library(stringr)
library(reshape)
library(dplyr)
library(plyr)
library(data.table)
library(splitstackshape)
library(doBy)
library(ggplot2)
library(foreign)

#setwd("T:/Practice ClickStream/NewApp")
#NewData <- read_csv("data/NewData.csv",col_types = cols(X1 = col_skip()))
NewData<- read_sav("data/Data.sav")
NewData$video_name=str_trim(NewData$video_name, side = "both")
NewData2 = data.frame(NewData)
NewData2 = subset(NewData2, !is.na(NewData2$timecode))

video_names <- as.data.frame(unique(NewData2$video_name))
group <- c("First Group","Second Group","Third Group")

attach(NewData2)
KeyForNewData2 = cbind(aggregate(key=="dowbkiad_subtitle", by=list(NewData2$illinois_user_id, NewData2$video_name), sum),
                       aggregate(key=="end", by=list(NewData2$illinois_user_id, NewData2$video_name), sum)[,3],
                       aggregate(key=="heartbeat", by=list(NewData2$illinois_user_id, NewData2$video_name), sum)[,3],
                       aggregate(key=="pause", by=list(NewData2$illinois_user_id, NewData2$video_name), sum)[,3],
                       aggregate(key=="play", by=list(NewData2$illinois_user_id, NewData2$video_name), sum)[,3],
                       aggregate(key=="playback_rate_change", by=list(NewData2$illinois_user_id, NewData2$video_name), sum)[,3],
                       aggregate(key=="seek", by=list(NewData2$illinois_user_id, NewData2$video_name), sum)[,3],
                       aggregate(key=="start", by=list(NewData2$illinois_user_id, NewData2$video_name), sum)[,3],
                       aggregate(key=="subtitle_change", by=list(NewData2$illinois_user_id, NewData2$video_name), sum)[,3],
                       aggregate(key=="volume_change", by=list(NewData2$illinois_user_id, NewData2$video_name), sum)[,3],
                       aggregate(key=="wait", by=list(NewData2$illinois_user_id, NewData2$video_name), sum)[,3])

colnames(KeyForNewData2) = c("UserID","Video", "Delete", "end", "heartbeat","pause","play","playback_rate_change","seek","start",
                             "subtitle_change","volume_change","wait")
detach(NewData2)

KeyForNewData2 = subset(KeyForNewData2, select = c("UserID","Video", "end", 
                                                   "heartbeat","pause","play",
                                                   "playback_rate_change","seek","start",
                                                   "subtitle_change","volume_change","wait"))

KeyForNewData2$Secs = KeyForNewData2$heartbeat * 5



library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Clickstream Data Analysis"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("video","Video:", choices=video_names),
      selectInput("group","User Group: (10 users per user group)", choices=c(1,2,3)),
      hr(),
      helpText("Data from Coursera")
    ),
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Histogram",plotOutput("histplot"),verbatimTextOutput("basic")),
                  tabPanel("Cumulative",plotOutput("cumulative")),
                  tabPanel("Density",plotOutput("ggplot")),
                  tabPanel("BoxPlot",plotOutput("BoxPlot"),plotOutput("BoxPlot2")),
#                  tabPanel("Density plot per user", plotOutput("ggplot2")),
                  tabPanel("Graph", plotOutput("graph"),plotOutput("graph2"),plotOutput("graph3"))
                  
                  
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$histplot <- renderPlot({
    DigiConP1 = subset(NewData2, NewData2$video_name == input$video)
    hist(DigiConP1$timecode, main = "Histogram of the time user spent by video",xlim = range(DigiConP1$timecode),
         ylim = c(0,max(DigiConP1$timecode)),xlab = "Seconds", breaks = 50)
  })
  
  output$basic <- renderPrint({
    DigiConP1 = subset(NewData2, NewData2$video_name == input$video)
    summary(DigiConP1$timecode)
  }) 
  output$cumulative <- renderPlot({
    DigiConP1 = subset(NewData2, NewData2$video_name == input$video)
    plot(ecdf(DigiConP1$timecode),main="Empirical Cumulative Distribution of the time by video", xlab="time by seconds")
    
  })
  output$ggplot <- renderPlot({
    DigiConP1 = subset(NewData2, NewData2$video_name == input$video)
    plot(density(NewData2$timecode),col="red", xlim = c(0,2500),ylim=c(0,0.0035),xlab="time in seconds",
         main = "Density plot of the time user spent, Red is for all videos, black is for the selected video")
    lines(density(DigiConP1$timecode), col = "black")
  })
  
  output$BoxPlot <- renderPlot({
    DigiConP1 = subset(NewData2, NewData2$video_name == input$video)
    boxplot(DigiConP1$timecode, data=DigiConP1, outline = TRUE, col="bisque")
    title("Boxplot for the duration of the video")
  })
  
  output$BoxPlot2 <- renderPlot({
    DigiConP1 = subset(NewData2, NewData2$video_name == input$video)
    boxplot(timecode~key, data=DigiConP1, outline = TRUE, col="bisque")
    title("Comparing boxplot()s by different keys")
  })
  
#  output$ggplot2 <- renderPlot({
#    if(input$group==1) temp <- FirstGroup
#    if(input$group==2) temp <- SecondGroup
#    if(input$group==3) temp <- ThirdGroup
#    DigiConP1 = subset(NewData2, NewData2$video_name == video)
#    ggplot(temp, aes(timecode, color = illinois_user_id)) + title("Density Plots by Group users")+
#      geom_density(alpha=.5) + 
#      geom_vline(data = MFG, aes(xintercept=rating.mean, colour = illinois_user_id),
#                 linetype="dashed", size=1) +
#      theme(legend.position="none")
#    
#  })
  
  output$graph <- renderPlot({
    #KeyForDigiConP4 <- KeyForDigiConP4[KeyForDigiConP4$Video==input$video,]
    #KeySub = subset(KeyData2, KeyData2$Video == input$video)
    KeySub = subset(KeyForNewData2, KeyForNewData2$Video == input$video)
    plot(KeySub$Secs, KeySub$pause,
         main = "Pause clicks per students per video",
         xlim = c(0,max(KeySub$Secs)+100),
         ylim = c(0,max(KeySub$pause)+10),
         xlab = "Seconds",
         ylab = "Number of Pause Clicks", 
         pch=18, col = "red")
    abline(v=mean(KeySub$Secs))
    abline(h=mean(KeySub$pause))
    text(KeySub$Secs, KeySub$pause, pos = 3, cex= 0.6)
  })
  
  output$graph2 <- renderPlot({
    #KeyForDigiConP4 <- KeyForDigiConP4[KeyForDigiConP4$Video==input$video,]
    #KeySub = subset(KeyData2, KeyData2$Video == input$video)
    KeySub = subset(KeyForNewData2, KeyForNewData2$Video == input$video)
    plot(KeySub$Secs, KeySub$wait,
         main = "wait clicks per students per video",
         xlim = c(0,max(KeySub$Secs)+100),
         ylim = c(0,max(KeySub$wait)+10),
         xlab = "Seconds",
         ylab = "Number of wait Clicks", 
         pch=18, col = "Green")
    abline(v=mean(KeySub$Secs))
    abline(h=mean(KeySub$wait))
    text(KeySub$Secs, KeySub$wait, pos = 3, cex= 0.6)
  })
  
  output$graph3 <- renderPlot({
    #KeySub = subset(KeyData2, KeyData2$Video == input$video)
    KeySub = subset(KeyForNewData2, KeyForNewData2$Video == input$video)
    plot(KeySub$Secs, KeySub$seek,
         main = "Seek clicks per students per video",
         xlim = c(0,max(KeySub$Secs)+100),
         ylim = c(0,max(KeySub$seek)+10),
         xlab = "Seconds",
         ylab = "Number of seek Clicks", 
         pch=18, col = "blue")
    abline(v=mean(KeySub$Secs))
    abline(h=mean(KeySub$seek))
    text(KeySub$Secs, KeySub$Seek, pos = 3, cex= 0.6)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

