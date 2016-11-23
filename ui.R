library(shinydashboard)
library(shiny)
library(dplyr)
library(ggplot2)
library(reshape2)
library(plotly)

## **All the time are in timezone Hongkong and the date is "2016-11-17"
finalized_POSIX <- read.csv("finalized_data.csv")
name_list <- c("RunnerNo","Name","Gender","Category","PlaceOverall","PlaceGender","PlaceCategory","5K","10K","15K","20K","21K","25K","30K","35K","40K","42K")
names(finalized_POSIX) <- name_list
finalized_POSIX[,2] <- as.character(finalized_POSIX[,2])
finalized_POSIX[,3] <- as.character(finalized_POSIX[,3])
finalized_POSIX[,4] <- as.character(finalized_POSIX[,4])
finalized_POSIX[,5] <- as.numeric(finalized_POSIX[,5])
finalized_POSIX[,6] <- as.numeric(finalized_POSIX[,6])
finalized_POSIX[,7] <- as.numeric(finalized_POSIX[,7])

for (i in 8:17) {
  finalized_POSIX[,i] <- as.POSIXct(finalized_POSIX[,i], format="%Y-%m-%d %H:%M:%S",tz="Hongkong")
}


ui <- dashboardPage(
  
  ######################  SIDEBAR TITLE  ###############################
  
  dashboardHeader(title = "London Marathon (2016 Fall)"),
  
  
  ####################  SIDEBAR DESIGN ###############################  
  
  dashboardSidebar(
    width=200,
    
    sidebarMenu(id="menu1",
                menuItem("Ready?", icon = icon("off",lib="glyphicon"), 
                         tabName = "ready",badgeColor = "green"),
                menuItem("Boxplot", icon = icon("users"), 
                         tabName = "box",badgeColor = "red"),
                
                menuItem("Ranking vs Time", icon = icon("glyphicon glyphicon-signal",lib="glyphicon"),
                         tabName = "ranking",badgeColor = "blue"),
                
                menuItem("Distance vs Time", icon = icon("random",lib="glyphicon"), 
                         tabName = "distance",badgeColor = "orange")
    )# glyphicon glyphicon-list
  ),
  
  ################## CONTENT DESIGN ##################################
  dashboardBody(
    
    
    tabItems(
      
      tabItem(
        tabName= "ready",r
        fluidPage(
          titlePanel(h1("Go to the Github Link Below to Play my Shiny App")),
          mainPanel(
            h3(a("https://github.com/riven314/London-Marathon.git")),
            code("ShinyApp(Marathon).R"),br(),
            code("finalized_data.csv")
          )
        )
      ),
      
      ## For Boxplot Display
      tabItem(
        tabName = "box",
        fluidPage(
          titlePanel(""),
          sidebarPanel(
            checkboxGroupInput("age", label = h4("Select Age Range"), 
                               choices=c("All" ,"18-39","40-44","45-49","50-54","55-59","60-64","60-64","65-69","70+"),
                               selected="All"),
            
            
            checkboxGroupInput("gender", label = h4("Select Gender"), 
                               choices=c("All","M","F"),
                               selected="All"),
            sliderInput("stage",h4("Time Spent for",textOutput("stage_no"),"Km"),
                        min=5,
                        max=42,step=5,
                        value=c(5),ticks=FALSE)),
          mainPanel(
            plotOutput("plot2")
          ))
        
      ),
      
      
      
      
      ## For Ranking Display
      tabItem(
        tabName = "ranking",
        fluidPage(
          titlePanel(""),
          sidebarPanel(
            sliderInput("stage2",h4("Time Spent for",textOutput("stage_no2"),"Km"),
                        min=5,
                        max=42,step=5,
                        value=c(5),ticks=FALSE,animate=animationOptions(interval=2300,loop=TRUE)),
            selectInput("ranking",h4("Ranking Type"),
                        choices=c("Overall Ranking","Gender Ranking"))
          ),
          mainPanel(
            plotOutput("plot3")
          )
        )),      
      
      
      
      ## For Distance Time Graph Display
      tabItem(
        tabName = "distance",
        fluidPage(
          titlePanel("Random Sampling"),
          sidebarPanel(
            sliderInput("n",h4("Adjust Time Range"),min=0,max=9,step=1,value=c(0,1)),
            numericInput("range1",label = h4("Range 1"), min=0, max=10000,step=1000,value = 10000),
            numericInput("q1_no",label = h4("Q1 No."), min=0, max=10,value = 1),
            numericInput("range2",label = h4("Range 2"), min=10000, max=20000,step=1000,value = 20000),
            numericInput("q2_no",label = h4("Q2 No."), min=0, max=10,value = 1),
            numericInput("range3",label = h4("Range 3"), min=20000, max=30000,step=1000,value = 30000),
            numericInput("q3_no",label = h4("Q3 No."), min=0, max=10,value = 1),
            numericInput("range4",label = h4("Range 4"), min=30000, max=39013,step=1000,value = 39013),
            numericInput("q4_no",label = h4("Q4 No."), min=0, max=10,value = 1),
            actionButton("go","Draw")
          ),
          mainPanel(
            plotlyOutput("runner_plot"),width=8
          )
        )
        
      )
    )
    
    
    
  )
)