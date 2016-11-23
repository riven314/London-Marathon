library(reshape2)
library(ggplot2)
library(plotly)

# randomly sample 10 rows of runners
# sample_runner <- complete_POSIX[sample(nrow(complete_POSIX),10),]


# define a function
# Pick one runner randomly from each quartile
quartile_random <- function(data,Q1,S1,Q2,S2,Q3,S3,S4) {
  runner_4 <-rbind( sample_n(data[data$PlaceOverall>Q3, ],S4),
                    sample_n(data[(Q2<data$PlaceOverall&data$PlaceOverall<Q3), ],S3)) %>% 
    rbind(sample_n(data[(Q1<data$PlaceOverall&data$PlaceOverall<Q2), ],S2)) %>% 
    rbind(sample_n(data[data$PlaceOverall<Q1, ],S1))
  return(runner_4)
}


# define a function to melt sample data frame
# and plot the distance time graph
# input sample dataframe, output a distance time graph
# ogive? 
distance_time_plot <- function(data) {
  # melt_data <- melt(data, 
  #                     id.vars = c("RunnerNo","Name","Category","PlaceOverall","PlaceGender","PlaceCategory"), 
  #                     measure.vars = c("5K", "10K","15K","20K","21K","25K","30K","35K","40K","42K"))
  # 
  # melt_data$variable <- sapply(as.character(melt_data$variable), switch, 
  #                                "5K" = 5, "10K" = 10, "15K" = 15, "20K" = 20,"21K"=21,
  #                                "25K"=25,"30K"=30,"35K"=35,"40K"=40,"42K"=42,USE.NAMES = F)
  # 
  # colnames(melt_data)[7] <- "Distance"
  # colnames(melt_data)[8] <- "Time"
  # 
  runner_plot <- qplot(x=Time, y=Distance, geom = c("point","line"), 
        data = data,color=as.factor(PlaceOverall))
  return(ggplotly(runner_plot))
}

melt_sample <- function(data) {
  melt_data <- melt(data, 
                    id.vars = c("RunnerNo","Name","Category","PlaceOverall","PlaceGender","PlaceCategory"), 
                    measure.vars = c("5K", "10K","15K","20K","21K","25K","30K","35K","40K","42K"))
  
  melt_data$variable <- sapply(as.character(melt_data$variable), switch, 
                               "5K" = 5, "10K" = 10, "15K" = 15, "20K" = 20,"21K"=21,
                               "25K"=25,"30K"=30,"35K"=35,"40K"=40,"42K"=42,USE.NAMES = F)
  
  colnames(melt_data)[7] <- "Distance"
  colnames(melt_data)[8] <- "Time"
  return(melt_data)
}


# Compare 4 sample from Ranking {20000-30000}
distance_time_plot(quartile_random(complete_POSIX,10000,0,20000,0,30000,4,0))

######################## Overall Data Exploration / Plot ###############################


# randomly generate some graph
distance_time_plot(complete_POSIX[sample(nrow(complete_POSIX),4),])
distance_time_plot(complete_POSIX[sample(nrow(complete_POSIX),4),])
distance_time_plot(complete_POSIX[sample(nrow(complete_POSIX),4),])

# plot FINISH histogram ??
# make that into an animated graph
hist(complete_POSIX[,16],breaks=100)
hist(complete_POSIX[,15],breaks=100)
hist(complete_POSIX[,14],breaks=100)
hist(complete_POSIX[,13],breaks=100)
hist(complete_POSIX[,12],breaks=100)
hist(complete_POSIX[,11],breaks=100)
hist(complete_POSIX[,10],breaks=100)
hist(complete_POSIX[,9],breaks=100)
hist(complete_POSIX[,8],breaks=100)
hist(complete_POSIX[,7],breaks=100)

# fit model into F distribution??


# filter age

# filter sex



#### SOME INTERESTING DISCOVERY: WE NEED TO MATCH THE GENDER TO FIND OUT MORE ######
#### RANKING VS FINISH TIME catergorized by Age
sample_runner3 <- na.omit(finalized_POSIX[sample(nrow(finalized_POSIX),1000),])

ggplot1 <- ggplot(sample_runner3, aes(x=sample_runner3[,16], y=sample_runner3[,7],color=Category,Gender)) + 
  geom_point() + ylab("Category Ranking") + xlab("Finish Time")
ggploty(ggplot1)

#### HISTOGRAM on FINISH TIME categorized by Gender
plot_ly(alpha = 0.6) %>%
  add_histogram(x = filter(finalized_POSIX,Gender=="M")[,17]) %>%
  add_histogram(x = filter(finalized_POSIX,Gender=="F")[,17]) %>%
  layout(barmode = "overlay")

#### BOXPLOTS on FINISH TIME categorized by Age Group
plot_ly(type="box") %>%
  add_trace(y = filter(finalized_POSIX,Category=="18-39")[,17]) %>%
  add_trace(y = filter(finalized_POSIX,Category=="40-44")[,17]) %>%
  add_trace(y = filter(finalized_POSIX,Category=="45-49")[,17]) %>% 
  add_trace(y = filter(finalized_POSIX,Category=="50-54")[,17]) %>% 
  add_trace(y = filter(finalized_POSIX,Category=="55-59")[,17]) %>% 
  add_trace(y = filter(finalized_POSIX,Category=="60-64")[,17]) %>% 
  add_trace(y = filter(finalized_POSIX,Category=="65-69")[,17]) %>% 
  add_trace(y = filter(finalized_POSIX,Category=="70+")[,17]) %>% 
  layout(barmode = "overlay")



#   xxx <- filter(sample_runner3,Category=="18-39")
# ggplot(xxx, aes(y=xxx[,7], x=xxx[,16],color=xxx$Gender)) + 
#   geom_point() + ylab("Category Ranking") + xlab("Finish Time")
#   guides(colour=guide_legend(title="Category"))
