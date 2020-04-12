install.packages("rvest")
library(rvest) 


##################################################################
urls <- "https://www.timeanddate.com/holidays/us/"
year <-c("2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020")

dates_vec <- vector()

day_events_vec <- vector()


for (y in year){
  curent_url <- paste(urls, y , sep="")
  get_html <- curent_url %>% read_html()
  
  get_nodes<- get_html %>% html_nodes("th") 
  node_texts <- html_text(get_nodes)
  
  node_texts <- node_texts[6:length(node_texts)]
  node_texts <- paste(y, node_texts, sep=" ")
  dates_vec <- c(dates_vec, node_texts) 
  
  get_day_event <- get_html %>% html_nodes("td") 
  day_events_text <- html_text(get_day_event)
  day_events_vec <- c(day_events_vec, day_events_text)

}

dates_vec <- as.Date(dates_vec, format='%Y %b %d')
#################################################################################


##################################################################################

days_ind <- seq(1,length(day_events_vec), by=4)
events_ind <- seq(2,length(day_events_vec), by=4)

days_vec <- vector()
events_vec <- vector()

for (d in days_ind){
  days_vec <- c(days_vec, day_events_vec[d])
}

#days_vec_last_ind <- length(days_vec)-1
#days_vec <- days_vec[1:days_vec_last_ind]

for (e in events_ind){
  events_vec <- c(events_vec, day_events_vec[e])
} 

unique(days_vec, incomparables = FALSE)


events_file <- "C:/Users/kinja/OneDrive/Desktop/All/PatentAnalysis/data-appended.xlsx"


#install.packages("openxlsx")
library("openxlsx")

wb <- createWorkbook()
addWorksheet(wb, "Worksheet 1")
writeData(wb,sheet = 1, x = days_vec)
saveWorkbook(wb, file = events_file, overwrite = TRUE)

################################################################################
length(dates_vec) 
length(days_vec)
length(events_vec)

extra <- "* Observed only in some communities of this state. ** Observed only in part of this state. *** Optional holiday in this state.Hover your mouse over the region or click on the holiday for details."

which(days_vec %in% extra) 

match(extra,days_vec)




