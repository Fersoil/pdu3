# Pocz?tkowo wczytamy sobie troche danych
# 
# 
#
#
#libki

library(stringi)
library(data.table)
library(rstudioapi)

# tutaj robimy ustawienie folderu
# wczytywanie
setwd(dirname(getActiveDocumentContext()$path)) 
getwd()

options(stringsAsFactors = FALSE)

load_data <- function(){
  page_prefix <- "https://s3.amazonaws.com/tripdata/"
  
  sufix <- "-citibike-tripdata.csv"
  
  data_set_dates <- c("201901")
  data_set_cities <- c("JC-")
  
  
  data_set_names <- paste(data_set_cities, data_set_dates, sep="")
  
  for(el in data_set_names){
    path <- paste(page_prefix, el, sufix, ".zip", sep = "")
    temp <- tempfile()
    download.file(path, temp)
    
    assign(el, read.csv(unz(temp, paste(el, sufix, sep=""))))
    unlink(temp)
  }
}

load_data()


temp <- tempfile()
download.file(path, temp)

path <- paste(page_prefix, "201901", sufix, sep = "")
path <- "201901-citibike-tripdata.csv"
set <- read.csv(unz(temp, "201901-citibike-tripdata.csv"))
unlink(temp)
getwd()


