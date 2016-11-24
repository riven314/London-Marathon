library(shinydashboard)
library(shiny)
library(dplyr)
library(ggplot2)
library(reshape2)
library(plotly)




#################### BACK-END TERMINAL #############################
shinyServer(function(input,output)  {
  #   output$time <- renderText({
  #   print(input$time[2])
  # })
  
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
  
  ###############
  
  mile_names <- c("RunnerNo","Name","Gender","Category","PlaceOverall","PlaceGender","PlaceCategory","K5","K10","K15","K20","K21","K25","K30","K35","K40","K42")
  finalized_POSIX2 <- finalized_POSIX
  names(finalized_POSIX2) <- mile_names
  
  
  finalized_POSIX2 = mutate(finalized_POSIX2, 
                            Speed42_40=1000*2/as.numeric(difftime(K42,K40,units="mins")),
                            Speed40_35=1000*5/as.numeric(difftime(K40,K35,units="mins")),
                            Speed35_30=1000*5/as.numeric(difftime(K35,K30,units="mins")),
                            Speed30_25=1000*5/as.numeric(difftime(K30,K25,units="mins")),
                            Speed25_21=1000*4/as.numeric(difftime(K25,K21,units="mins")),
                            Speed21_20=1000/as.numeric(difftime(K21,K20,units="mins")),
                            Speed20_15=1000*5/as.numeric(difftime(K20,K15,units="mins")),
                            Speed15_10=1000*5/as.numeric(difftime(K15,K10,units="mins")),
                            Speed10_5=1000*5/as.numeric(difftime(K10,K5,units="mins")),
                            Speed5_0=1000*5/as.numeric(difftime(K5,as.POSIXct("2016-11-17 00:00:00","Hongkong"),units="mins"))
  )
  finalized_POSIX2 <- na.omit(finalized_POSIX2)
  
  finalized_POSIX2$OverallRanking <- vector(mode="numeric",length = nrow(finalized_POSIX2))
  for (i in 1:nrow(finalized_POSIX2)) {
    if (0<= finalized_POSIX2$PlaceOverall[i]&finalized_POSIX2$PlaceOverall[i]<=10000) {finalized_POSIX2$OverallRanking[i]=1}
    else if (10000<finalized_POSIX2$PlaceOverall[i]&finalized_POSIX2$PlaceOverall[i]<=20000) {finalized_POSIX2$OverallRanking[i]=2}
    else if (20000<finalized_POSIX2$PlaceOverall[i]&finalized_POSIX2$PlaceOverall[i]<=30000) {finalized_POSIX2$OverallRanking[i]=3}
    else if (30000<finalized_POSIX2$PlaceOverall[i]&finalized_POSIX2$PlaceOverall[i]<=40000) {finalized_POSIX2$OverallRanking[i]=4}}
  finalized_POSIX2$OverallRanking <- as.factor(finalized_POSIX2$OverallRanking)
  
  
  Distance <- c(5,10,15,20,21,25,30,35,40,42)
  Ranking1 <- c(mean(filter(finalized_POSIX2,OverallRanking==1)[,27]),
                mean(filter(finalized_POSIX2,OverallRanking==1)[,26]),
                mean(filter(finalized_POSIX2,OverallRanking==1)[,25]),
                mean(filter(finalized_POSIX2,OverallRanking==1)[,24]),
                mean(filter(finalized_POSIX2,OverallRanking==1)[,23]),
                mean(filter(finalized_POSIX2,OverallRanking==1)[,22]),
                mean(filter(finalized_POSIX2,OverallRanking==1)[,21]),
                mean(filter(finalized_POSIX2,OverallRanking==1)[,20]),
                mean(filter(finalized_POSIX2,OverallRanking==1)[,19]),
                mean(filter(finalized_POSIX2,OverallRanking==1)[,18])
  )
  Ranking2 <- c(mean(filter(finalized_POSIX2,OverallRanking==2)[,27]),
                mean(filter(finalized_POSIX2,OverallRanking==2)[,26]),
                mean(filter(finalized_POSIX2,OverallRanking==2)[,25]),
                mean(filter(finalized_POSIX2,OverallRanking==2)[,24]),
                mean(filter(finalized_POSIX2,OverallRanking==2)[,23]),
                mean(filter(finalized_POSIX2,OverallRanking==2)[,22]),
                mean(filter(finalized_POSIX2,OverallRanking==2)[,21]),
                mean(filter(finalized_POSIX2,OverallRanking==2)[,20]),
                mean(filter(finalized_POSIX2,OverallRanking==2)[,19]),
                mean(filter(finalized_POSIX2,OverallRanking==2)[,18])
  )
  
  Ranking3 <- c(mean(filter(finalized_POSIX2,OverallRanking==3)[,27]),
                mean(filter(finalized_POSIX2,OverallRanking==3)[,26]),
                mean(filter(finalized_POSIX2,OverallRanking==3)[,25]),
                mean(filter(finalized_POSIX2,OverallRanking==3)[,24]),
                mean(filter(finalized_POSIX2,OverallRanking==3)[,23]),
                mean(filter(finalized_POSIX2,OverallRanking==3)[,22]),
                mean(filter(finalized_POSIX2,OverallRanking==3)[,21]),
                mean(filter(finalized_POSIX2,OverallRanking==3)[,20]),
                mean(filter(finalized_POSIX2,OverallRanking==3)[,19]),
                mean(filter(finalized_POSIX2,OverallRanking==3)[,18])
  )
  
  Ranking4 <- c(mean(filter(finalized_POSIX2,OverallRanking==4)[,27]),
                mean(filter(finalized_POSIX2,OverallRanking==4)[,26]),
                mean(filter(finalized_POSIX2,OverallRanking==4)[,25]),
                mean(filter(finalized_POSIX2,OverallRanking==4)[,24]),
                mean(filter(finalized_POSIX2,OverallRanking==4)[,23]),
                mean(filter(finalized_POSIX2,OverallRanking==4)[,22]),
                mean(filter(finalized_POSIX2,OverallRanking==4)[,21]),
                mean(filter(finalized_POSIX2,OverallRanking==4)[,20]),
                mean(filter(finalized_POSIX2,OverallRanking==4)[,19]),
                mean(filter(finalized_POSIX2,OverallRanking==4)[,18])
  )
  
  mean_time <- data.frame(Distance,Ranking1,Ranking2,Ranking3,Ranking4)
  
  mean_time <- melt(mean_time,
                    id.vars = c("Distance"))
  
  finalized_POSIX3 <- melt(finalized_POSIX2,
                           id.vars = c("RunnerNo","Name","Gender","Category","PlaceOverall","PlaceGender","PlaceCategory","K5", "K10","K15","K20","K21","K25","K30","K35","K40","K42","OverallRanking"), 
                           measure.vars = c("Speed42_40", "Speed40_35","Speed35_30","Speed30_25","Speed25_21","Speed21_20","Speed20_15","Speed15_10","Speed10_5","Speed5_0"))
  finalized_POSIX3$variable <- sapply(as.character(finalized_POSIX3$variable), switch, 
                                      "Speed42_40"=42,"Speed40_35"=40, "Speed35_30"=35, "Speed30_25"=30, "Speed25_21"=25,"Speed21_20"=21,
                                      "Speed20_15"=20,"Speed15_10"=15,"Speed10_5"=10,"Speed5_0"=5,USE.NAMES = F)
  finalized_POSIX3[,19] <- as.factor(finalized_POSIX3[,19])
  
  output$plot4 <- renderPlot({
    if (input$graph=="Boxplot") 
    {ggplot(finalized_POSIX3,aes(x=variable,y=value))+
        geom_boxplot(aes(color=OverallRanking)) + ggtitle("Speed Distribution in Different Phrases")+labs(y="Speed (Metres/Min)",x="From 0Km to 42Km")+
        theme(legend.position="bottom") + scale_y_continuous(limits = c(0, 300))
    }
    else if (input$graph=="Line")
    {ggplot(mean_time,aes(x=Distance,y=value)) + 
        geom_line(size=0.9,aes(color=mean_time$variable)) +
        geom_point(size=3,color="gray15")+labs(x="Distance (Km)", y="Average Speed (Metres/Min)") +
        ggtitle("Average Speed in Different Phrase") + theme(legend.position="none")}
  },width=1000)
  
  km <- reactive({
    if (input$km == 5)    {27}
    else if (input$km == 10) {26}
    else if (input$km == 15) {25}
    else if (input$km == 20) {24}
    else if (input$km == 25) {22}
    else if (input$km == 30) {21}
    else if (input$km == 35) {20}
    else if (input$km == 40) {19}
    else if (input$km == 42) {18}
  })
  
  output$plot5 <- renderPlot({
    ggplot(finalized_POSIX2,aes(x=PlaceOverall,y=finalized_POSIX2[,km()],color=Gender))+geom_point()+
      scale_y_continuous(limits = c(0, 400))+labs(x="Overall Ranking",y="Speed (Metres/Min)") + ggtitle(paste("Speed on",input$km,"Km",sep=" "))
  },width=1000,height=400)
})

