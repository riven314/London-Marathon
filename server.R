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
