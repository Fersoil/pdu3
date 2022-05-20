library(leaflet)
library(dplyr)
library(stringi)
library(data.table)
options(stringsAsFactors = FALSE)

download_data = FALSE

  
  
  page_prefix <- "https://s3.amazonaws.com/tripdata/"
  sufix <- "-citibike-tripdata.csv"
  dest_dir <- "data/"
  
  
  i <- sapply(as.character(1:12), FUN = function(x) {ifelse(nchar(x) == 1, paste("0",x, sep=""), x)})
  
  data_set_dates <- paste("2019", i, sep="")
  
  for(el in data_set_dates){
    # ustalamy sciezke docelową
    dest_path <- paste(dest_dir, "JC",el, sufix, ".zip", sep="")
    
    if(download_data) { # pobieramy dane tylko jezeli jest taka potrzeba
      url_path <- paste(page_prefix, "JC-", el, sufix, ".zip", sep = "")
      download.file(url_path, dest_path)
      print(paste("succesfully downloaded data at", dest_path))
    }
    
    # updateujemy dane
    assign(paste("JC",el, sep=""), read.csv(unz(dest_path, paste("JC-",el, sufix, sep=""))))
    print(paste("succesfully saved data at ", dest_path, " filename: ", "JC", el, sep=""))
  }
  
  
  new_york_cords = c(-73.935242, 40.730610)
  
  
  
  
  data_names <- paste("JC", data_set_dates, sep="")
  data <- data.table()
  
  for(el in data_names){
    data <- rbind(eval(parse(text = el)), data)
  }
  
  data <- as.data.table(data)
  
  
  out_stations <- data[, .(lat = start.station.latitude, lng = start.station.longitude, 
                        name = start.station.name)][, .(n = .N), by=.(lat, lng, name)]
  
  in_stations <- data[, .(lat = end.station.latitude, lng = end.station.longitude, 
                           name = end.station.name)][, .(n = .N), by=.(lat, lng, name)]
  
  in_stations <- as.data.table(in_stations)
  out_stations <- as.data.table(out_stations)

#########



# ucsd.map <- get_map("UC San Diego",zoom=15)
# 
# ggmap::register_google(key = "iE3yD12i0xScfc1QZGkPifjLL_c=")
# p <- get_map(location = c(lon = -95.3632715, lat = 29.7632836),
#              zoom = "auto", scale = "auto", )
# 
# p + geom_point(aes(x = Longitude, y = Latitude,  colour = Initial.Type.Group), data = i2, size = 0.5) + 
#   theme(legend.position="bottom")
# ??get_map
# ggmap(ucsd.map)
# ??register_google
# 
# plot(getMap())
# 
# 
# install.packages("lubridate")
# install.packages("ggplot2")
# install.packages("data.table")
# install.packages("ggrepel")
# install.packages("dplyr")
# install.packages("data.table")
# install.packages("tidyverse")