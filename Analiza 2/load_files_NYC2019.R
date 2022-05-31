library(stringi)
library(data.table)
options(stringsAsFactors = FALSE)

page_prefix <- "https://s3.amazonaws.com/tripdata/"
sufix <- "-citibike-tripdata.csv"

i <- sapply(as.character(1:12), FUN = function(x) {ifelse(nchar(x) == 1, paste("0",x, sep=""), x)})

data_set_dates <- paste("2019", i, sep="")

for(el in data_set_dates){
  path <- paste(page_prefix, el, sufix, ".zip", sep = "")
  temp <- tempfile()
  download.file(path, temp)
  assign(paste("NYC",el, sep=""), read.csv(unz(temp, paste(el, sufix, sep=""))))
  unlink(temp)
}

data <- data.table()

for(el in paste("NYC", data_set_dates, sep="")){
  data <- rbind(eval(parse(text = el)), data)
}

data <- as.data.table(data)

