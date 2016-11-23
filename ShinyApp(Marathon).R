#### SHINY APP WORK FLOW #####

# LAU WAN HONG 3035098070

#1. Dynamic Distance-Time Graph 
#2. Dynamic Boxplot/ Dynamic Histogram Histogram for different age
#3. Dynamic RANKING vs FINISH TIME 

#    Interface Framework
#    Beautify the Title, Xlab, Ylab, Legend
#    Reproducible Markdown
#    Template the script


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
      tabName= "ready",
        fluidPage(
          titlePanel(h1("")),
          mainPanel(
            h1("Download Files from Github Link"),
            h3(a("https://github.com/riven314/London-Marathon.git")),
            code("ShinyApp(Marathon).R"),br(),
            code("finalized_data.csv"),
            h1("Or Run the Code R (With Shiny Library)"),
            code('runGithub("London-Mararthon","riven314")')
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


#################### DATA MANIPULATINO HERE #########################



#################### BACK-END TERMINAL #############################
server <- function(input,output)  {
  #   output$time <- renderText({
  #   print(input$time[2])
  # })

    
    quartile_random <- function(data,Q1,S1,Q2,S2,Q3,S3,S4) {
      runner_4 <-rbind( sample_n(data[data$PlaceOverall>Q3, ],S4),
                        sample_n(data[(Q2<data$PlaceOverall&data$PlaceOverall<Q3), ],S3)) %>% 
        rbind(sample_n(data[(Q1<data$PlaceOverall&data$PlaceOverall<Q2), ],S2)) %>% 
        rbind(sample_n(data[data$PlaceOverall<Q1, ],S1))
      return(runner_4)
    }
    
    # melt_sample <- function(data) {
    #   melt_data <- melt(data, 
    #                     id.vars = c("RunnerNo","Name","Category","PlaceOverall","PlaceGender","PlaceCategory"), 
    #                     measure.vars = c("5K", "10K","15K","20K","21K","25K","30K","35K","40K","42K"))
    #   
    #   melt_data$variable <- sapply(as.character(melt_data$variable), switch, 
    #                                "5K" = 5, "10K" = 10, "15K" = 15, "20K" = 20,"21K"=21,
    #                                "25K"=25,"30K"=30,"35K"=35,"40K"=40,"42K"=42,USE.NAMES = F)
    #   
    #   colnames(melt_data)[7] <- "Distance"
    #   colnames(melt_data)[8] <- "Time"
    #   return(melt_data)
    # }
    
    # melt_sample <- function(data) {
    #   A0 <- as.POSIXct("2016-11-17 00:00:00","Hongkong")
    #   data$START <- sapply(vector(mode="character",length = nrow(data)), function(x) x=A0)
    #   data = data[,c(1,2,3,4,5,6,7,18,8,9,10,11,12,13,14,15,16,17)]
    #   melt_data <- melt(data, 
    #                     id.vars = c("RunnerNo","Name","Category","PlaceOverall","PlaceGender","PlaceCategory"), 
    #                     measure.vars = c("START","5K", "10K","15K","20K","21K","25K","30K","35K","40K","42K"))
    #   melt_data$variable <- sapply(as.character(melt_data$variable), switch, 
    #                                "START"=0,"5K" = 5, "10K" = 10, "15K" = 15, "20K" = 20,"21K"=21,
    #                                "25K"=25,"30K"=30,"35K"=35,"40K"=40,"42K"=42,USE.NAMES = F)
    #   colnames(melt_data)[7] <- "Distance"
    #   colnames(melt_data)[8] <- "Time"
    #   melt_data$Time <- as.POSIXct(melt_data$Time, tz="Hongkong",origin = "1970-01-01")
    #   return(melt_data)
    # }
    melt_sample <- function(data) {
    data$START <- sapply(vector(mode="character",length = nrow(data)), function(x) x=as.POSIXct("2016-11-17 00:00:00",format="%Y-%m-%d %H:%M:%S", tz="Hongkong"))
    data$START <- as.POSIXct(data$START,origin = "1970-01-01")
    data = data[,c(1,2,3,4,5,6,7,18,8,9,10,11,12,13,14,15,16,17)]
    melt_data <- melt(data, 
                     id.vars = c("RunnerNo","Name","Category","PlaceOverall","PlaceGender","PlaceCategory"), 
                     measure.vars = c("START","5K", "10K","15K","20K","21K","25K","30K","35K","40K","42K"))
    melt_data$variable <- sapply(as.character(melt_data$variable), switch, 
                                "START"=0,"5K" = 5, "10K" = 10, "15K" = 15, "20K" = 20,"21K"=21,
                                "25K"=25,"30K"=30,"35K"=35,"40K"=40,"42K"=42,USE.NAMES = F)
    colnames(melt_data)[7] <- "Distance"
    colnames(melt_data)[8] <- "Time"
    melt_data$Time <- as.POSIXct(melt_data$Time,origin = "1970-01-01")
    return(melt_data)
    }
    
    
    distance_time_plot <- function(data) {
      # melt_data <- melt(data, 
      #                   id.vars = c("RunnerNo","Name","Category","PlaceOverall","PlaceGender","PlaceCategory"), 
      #                   measure.vars = c("5K", "10K","15K","20K","21K","25K","30K","35K","40K","42K"))
      # 
      # melt_data$variable <- sapply(as.character(melt_data$variable), switch, 
      #                              "5K" = 5, "10K" = 10, "15K" = 15, "20K" = 20,"21K"=21,
      #                              "25K"=25,"30K"=30,"35K"=35,"40K"=40,"42K"=42,USE.NAMES = F)
      # 
      # colnames(melt_data)[7] <- "Distance"
      # colnames(melt_data)[8] <- "Time"
      
      runner_plot <- qplot(x=Time, y=Distance, geom = c("point","line"), 
                           data = data,color=as.factor(PlaceOverall))+theme(legend.position="none")
      return(ggplotly(runner_plot))
    }
    
    select_sample <- eventReactive(input$go,
                          {quartile_random(finalized_POSIX,
                          input$range1,input$q1_no,input$range2,input$q2_no,input$range3,input$q3_no,input$q4_no)})
    
    
    #filter(melt_data,Time <= c(as.POSIXct("2016-11-17 01:00:00",format= "%Y-%B-%d %H:%M:%S")))

    melt_select_sample <- reactive({melt_sample(select_sample())})
    
    A0 <- as.POSIXct("2016-11-17 00:00:00","Hongkong")
    filter_select_sample <- reactive({filter(melt_select_sample(), (
            difftime(Time,A0) >= as.difftime(input$n[1],units="hours"))&(difftime(Time,A0) <= as.difftime(input$n[2],units="hours")))
            })
    # filter(melt_data, difftime(Time,A0) >= as.difftime(2,units="hours"))

    output$runner_plot <- renderPlotly({
      distance_time_plot(filter_select_sample())
    })
    
    
    
    #######
    
    
    
    
    x <- reactive({
      if (input$age == "All"& input$gender == "All")    {na.omit(finalized_POSIX[,])}
      else if (input$age == "All") {na.omit(finalized_POSIX[finalized_POSIX$Gender == input$gender,])}
      else if (input$gender == "All") {na.omit(finalized_POSIX[finalized_POSIX$Category == input$age,])}
      else {na.omit(finalized_POSIX[finalized_POSIX$Category == input$age & finalized_POSIX$Gender == input$gender,])}
      })
    

    
    # output$data2 <- renderDataTable({
    #   if (input$age == "All"& input$gender == "All")    {na.omit(finalized_POSIX[,])}
    #   else if (input$age == "All") {na.omit(finalized_POSIX[finalized_POSIX$Gender == input$gender,])}
    #   else if (input$gender == "All") {na.omit(finalized_POSIX[finalized_POSIX$Category == input$age,])}
    #   else {na.omit(finalized_POSIX[finalized_POSIX$Category == input$age & finalized_POSIX$Gender == input$gender,])}
    # })
    
    stage <- reactive({
      if (input$stage == 5)    {8}
      else if (input$stage == 10) {9}
      else if (input$stage == 15) {10}
      else if (input$stage == 20) {11}
      else if (input$stage == 25) {13}
      else if (input$stage == 30) {14}
      else if (input$stage == 35) {15}
      else if (input$stage == 40) {16}
      else if (input$stage == 42) {17}
    })
    
    output$stage_no <- renderText({input$stage})
    
    output$plot2 <- renderPlot({
      ggplot(x(),aes(x=Category,y= x()[,stage()] ,color=Gender)) + 
        geom_boxplot(position = position_dodge(width = 0),alpha=0.3) +
            labs(x="Age Category",y="TIME SPENT") + theme(legend.position="bottom") + ggtitle("BoxPlot by Age and Gender")
      
      },width=750,height=600)
    
    
    #######
    output$stage_no2 <- renderText({input$stage2})
    
    stage2 <- reactive({
      if (input$stage2 == 5)    {8}
      else if (input$stage2 == 10) {9}
      else if (input$stage2 == 15) {10}
      else if (input$stage2 == 20) {11}
      else if (input$stage2 == 25) {13}
      else if (input$stage2 == 30) {14}
      else if (input$stage2 == 35) {15}
      else if (input$stage2 == 40) {16}
      else if (input$stage2 == 42) {17}
    })
    
    ranking <- reactive({
      if(input$ranking=="Overall Ranking") {5} 
      else if (input$ranking=="Gender Ranking") {6}
    })
    
    
    output$plot3 <- renderPlot({
      ggplot(na.omit(finalized_POSIX), aes(x=na.omit(finalized_POSIX)[,stage2()], y=na.omit(finalized_POSIX)[,ranking()],color=Gender)) + 
        geom_point() + ylab("Ranking") + xlab(paste("Time Spent for",input$stage2,"Km (Hrs)"))+ theme(legend.position="bottom")+
        ggtitle("Scatter Plot on Ranking vs Time")
    },width=850,height=500)
    
}

# updateSliderInput(session,"date_range",min = (min_max_time[2,]),
#                   max = (min_max_time[1,]), value=min_max_time[2,])
    
shinyApp(ui,server)
