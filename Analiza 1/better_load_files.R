library(dplyr)
library(stringi)
library(data.table)

options(stringsAsFactors = FALSE)
getwd()

# parametr ktory opisuje czy chcemy pobierac pliki
download_data = TRUE

# przygotowujemy sie do odpowiedniego nazywania i pobierania danych
page_prefix <- "https://s3.amazonaws.com/tripdata/"
sufix <- "-citibike-tripdata.csv"
dest_dir <- "data/"


i <- sapply(as.character(1:12), FUN = function(x) {ifelse(nchar(x) == 1, paste("0",x, sep=""), x)})

data_set_dates <- paste("2019", i, sep="")

for(el in data_set_dates){
  # ustalamy sciezke docelową
  dest_path <- paste(dest_dir, "nc", el, sufix, ".zip", sep="")
  
  if(download_data) { # pobieramy dane tylko jezeli jest taka potrzeba
    # url_path <- paste(page_prefix, "JC-", el, sufix, ".zip", sep = "")
    url_path <- paste(page_prefix, el, sufix, ".zip", sep = "")
    download.file(url_path, dest_path)
    print(paste("succesfully downloaded data at", dest_path))
    
  }
  
  # updateujemy dane
  #assign(paste("JC",el, sep=""), read.csv(unz(dest_path, paste("JC-",el, sufix, sep=""))))
  assign(paste("NC",el, sep=""), read.csv(unz(dest_path, paste(el, sufix, sep=""))))
  #print(paste("succesfully saved data at ", dest_path, " filename: ", "JC", el, sep=""))
  print(paste("succesfully saved data at ", dest_path, " filename: ", "NC", el, sep=""))
}


# tworzymy wielką ramke danych do analizy

data_names <- paste("NC", data_set_dates, sep="")
# data_names <- paste("JC", data_set_dates, sep="")
data <- data.table()

for(el in data_names){
  data <- rbind(eval(parse(text = el)), data)
}

# gotowe
data <- as.data.table(data)