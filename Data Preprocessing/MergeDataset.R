# create a numeric vector with each element being 0
id <- vector("numeric",39013)

# fill in the vector with unique id no.
for (i in id) {
  id[i] = i
}

# insert id column into the 2 tables
table_agg$id <- id
table_final <- id

# remove the 10th column which are all empty (execute only once)
# table_final <- table_final[,-10]

# merge the 2 tables by id tmp created
complete_table <- merge(table_agg,table_final)

# rename the full table
complete_table_names <- c("PlaceOverall","PlaceGender","PlaceCategory","Name","RunnerNo","Category","5K","10K","15K","20K","21K","25K","30K","35K","40K","42K")
names(complete_table) <- complete_table_names

# rearrange the column order so that it is better looking
complete_table <- complete_table[,c(5,4,6,1,2,3,7,8,9,10,11,12,13,14,15,16)]

complete_POSIX <- complete_table
complete_difftime <- complete_table

for (i in 7:16) {
  complete_POSIX[,i] <- as.POSIXct(complete_table[,i], format = "%H:%M:%S")
}

for (i in 7:16) {
  complete_difftime[,i] <- as.difftime(as.character(complete_table[,i]))
}


## Gender Insertion
list_female$Gender <- sapply(vector(mode="character",length = nrow(list_female)), function(x) x="F")
list_men$Gender <- sapply(vector(mode="character",length = nrow(list_men)), function(x) x="M")

list_female2 <- list_female[,-1]
list_men2 <- list_men[,-1]
list_gender <- rbind(list_female2,list_men2)

finalized_POSIX <- merge(x=complete_POSIX, y=list_gender, by.x = 1,by.y = 1, all.x = TRUE)
finalized_POSIX <- finalized_POSIX[,c(1,2,17,3,4,5,6,7,8,9,10,11,12,13,14,15,16)]
# write the a finalized csv file into the working directory
#write.csv(finalized_POSIX,file="finalized_data.csv",row.names=FALSE)

# copy it into a csv file, the full table
#write.csv(complete_table,file="complete_data.csv",row.names=FALSE)

#############################################################################################


## Remove data with NA GENDER
## Subset the data with NA
## Perform Linear Interpolation
## Replace the Original Data
data_NA <- finalized_POSIX[!(complete.cases(finalized_POSIX)),]



## outlier identification ??






