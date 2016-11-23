library(XML)
# men list extract

url_men <- "http://results-2016.virginmoneylondonmarathon.com/2016/?page=1&event=MAS&pid=list&search%5Bage_class%5D=%25&search%5Bsex%5D=M"
men_table <- readHTMLTable(url_men,which=1)

url_female <- "http://results-2016.virginmoneylondonmarathon.com/2016/?page=1&event=MAS&pid=list&search%5Bage_class%5D=%25&search%5Bsex%5D=W"
female_table <- readHTMLTable(url_female,which=1)

url_men_end <- "&event=MAS&pid=list&search%5Bage_class%5D=%25&search%5Bsex%5D=M"
url_female_end <- "&event=MAS&pid=list&search%5Bage_class%5D=%25&search%5Bsex%5D=W"

men_pg_no <- 957
female_pg_no <- 602


function_sex <- function(pg_no,url_end,initial_table) {
  for (page_no in 2:pg_no) {
    url_sex_tmp <- paste("http://results-2016.virginmoneylondonmarathon.com/2016/?page=",
                         page_no,url_end,sep="")
    sex_table_tmp <- readHTMLTable(url_sex_tmp,which=1)
    initial_table <- rbind(initial_table,sex_table_tmp)
  }
  initial_table <- initial_table[,c(4,6)]
  return(initial_table)
}


## Can I perform it in parallel setting??

list_men <- function_sex(men_pg_no,url_men_end,men_table)
list_female <- function_sex(female_pg_no,url_female_end,female_table)

write.csv(list_men,file="list_men.csv",row.names=FALSE)
write.csv(list_female,file="list_female.csv",row.names=FALSE)


## Add a New Column and add the corresponding category there??
