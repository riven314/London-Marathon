library(XML)

# initiater: create the first table
url <- "http://results-2016.virginmoneylondonmarathon.com/2016/?page=1&pid=list"
table_final <- readHTMLTable(url,which=1)

# copy another page and append it into the above table, repeat the proces
# set it to 10 first
for (page_no in 2:1561) {
  url_tmp <- paste("http://results-2016.virginmoneylondonmarathon.com/2016/?page=",
                   page_no,"&pid=list",sep="")
  table_tmp <- readHTMLTable(url_tmp,which=1)
  table_final <- rbind(table_final,table_tmp)
}

# Rename table_final before merging it with table_agg
# Unify the Name variable because it is a primary key
names <- c("PlaceOverall","PlaceGender","PlaceCategory","Name","Club","RunnerNo","Category","HALF","FINISH")
names(table_final) <- names
table_final$Name <- gsub("^. ","",as.character(table_final$Name))

# delete the last column i.e NA
table_final <- table_final[,c(-8,-9,-10)]

write.csv(table_final,file="full_list.csv",row.names=FALSE)


