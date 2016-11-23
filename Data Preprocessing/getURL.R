afflibrary(xml2)
library(rvest)
library(XML)
library(tidyr)

# initiate the first 25 URL of url_match
# input an URL output a list of URL for 25 runners
url_init <- "http://results-2016.virginmoneylondonmarathon.com/2016/?page=1&pid=list"
pg <- read_html(url_init)
url_init <- html_attr(html_nodes(pg, "a"), "href")
ind <- grep("?content=detail&fpid=list&pid=list&idp=",url_init)
vector_url_agg <- as.vector(url_init[ind])

for (page_no in 2:1561) {
  url_temp <- paste("http://results-2016.virginmoneylondonmarathon.com/2016/?page=",
                   page_no,"&pid=list",sep="")
  pg_temp <- read_html(url_temp)
  url_temp <- html_attr(html_nodes(pg_temp, "a"), "href")
  ind_temp <- grep("?content=detail&fpid=list&pid=list&idp=",url_temp)
  vector_url_temp <- as.vector(url_temp[ind_temp])
  vector_url_agg <- c(vector_url_agg,vector_url_temp)
}

# pass the table_url_agg into a for loop to transform each element into a complete url
for (x in vector_url_agg) {
  vector_url_agg[grep(x,vector_url_agg)] = paste("http://results-2016.virginmoneylondonmarathon.com/2016/",x,sep="")
}





# Initiate the first row of dataframe aggregate table
test_url <- vector_url_agg[1]
temp_table <- readHTMLTable(test_url,which=4)
temp_table <- temp_table[,c(1,3)]
temp_table$Split <- as.character(temp_table$Split)
temp_table$Time <- as.character(temp_table$Time)
temp_table <- spread(temp_table,Split,Time)

header <- readHTMLTable(test_url,which=1)

# check.names makes the variable names with number opening unchanged
# otherwise there will be an X in front of the names
table_agg <- data.frame(temp_table,Name=as.character(header[2,2]),check.names = FALSE)
table_agg <- table_agg[,c(11,8,1,2,3,10,4,5,6,7,9)]



# input an URL from url_match output a dataframe with a row with name as header
# note that Time is character type in this case 
for (x in vector_url_agg[-1]) {
  temp_table <- readHTMLTable(x,which=4)
  temp_table <- temp_table[,c(1,3)]
  temp_table$Split <- as.character(temp_table$Split)
  temp_table$Time <- as.character(temp_table$Time)
  temp_table <- spread(temp_table,Split,Time)
  
  header <- readHTMLTable(x,which=1)
  
  # check.names makes the variable names with number opening unchanged
  # otherwise there will be an X in front of the names
  table_row <- data.frame(temp_table,Name=as.character(header[2,2]),check.names = FALSE)
  table_row <- table_row[,c(11,8,1,2,3,10,4,5,6,7,9)]
  table_agg <- rbind(table_agg,table_row)
}

# Unify the primary key before merging with table_final
table_agg$Name <- as.character(table_agg$Name)

write.csv(table_agg,file="full_agg.csv",row.names=FALSE)

