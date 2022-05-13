library(stringi)
library(data.table)

getwd()

options(stringsAsFactors = FALSE)

page_prefix <- "https://s3.amazonaws.com/tripdata/"
  
sufix <- "-citibike-tripdata.csv"

i <- 1:12
i <- as.character(i)
i <- sapply(i, FUN = function(x) {ifelse(nchar(x) == 1, paste("0",x, sep=""), x)})


data_set_dates <- paste("2019", i, sep="")
data_set_cities <- rep("JC-", length(i))
  
  
data_set_names <- paste(data_set_cities, data_set_dates, sep="")

res <- NULL
  
for(el in data_set_dates){
  path <- paste(page_prefix, "JC-", el, sufix, ".zip", sep = "")
  temp <- tempfile()
  download.file(path, temp)
  assign(paste("jc",el, sep=""), read.csv(unz(temp, paste("JC-", el, sufix, sep=""))))
  unlink(temp)
}

filenames <- paste("jc", data_set_dates, sep="")


