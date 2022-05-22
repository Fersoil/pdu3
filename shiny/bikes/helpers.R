library(leaflet)
library(dplyr)
library(stringi)
library(data.table)
library(lubridate)
options(stringsAsFactors = FALSE)

download_data = FALSE



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


new_york_cords = c(-73.935242, 40.730610)




data_names <- paste("NC", data_set_dates, sep="")
data <- data.table()

for(el in data_names){
  data <- rbind(eval(parse(text = el)), data)
}

data <- as.data.table(data)


# time <- data[1, .(starttime)]
# time <- time[[1]]
# time <- as.POSIXct(time,format="%Y-%m-%d %H:%M:%S")
# format(time, "%H:%M")
# hm(hours = hour(time), min = minute(time))


# proccess the dates
# let us firstly ignore different dates
nch <- nchar("2019-12-01 00:07:13.9360") # to avoid unnecesary computations

data <- data[, .(s_lat = start.station.latitude, s_lng = start.station.longitude, 
                 s_name = start.station.name, e_lat = end.station.latitude, 
                 e_lng = end.station.longitude, e_name = end.station.name, 
                 s_date = as.Date(starttime),#, format="%Y-%m-%d %H:%M:%S"), 
                 e_date = as.Date(stoptime),#, format="%Y-%m-%d %H:%M:%S"), 
                 # e_date = fast_strptime(substr(stoptime, 1, nch - 5), 
                 #                        format="%Y-%m-%d %H:%M:%S"), lt = FALSE), this may be faster
                s_time = as.ITime(starttime),
                e_time = as.ITime(stoptime))
             ]#[, .(s_lat, s_lng, s_name, e_lat, e_lng, e_name, s_dtime, e_dtime, 
              #     s_date = date(s_dtime), e_date = date(e_dtime), 
              #     s_time = as.ITime(s_dtime), e_time = as.ITime(e_dtime))
             #]

# data <- data[s_name != "Grove St PATH", ]



out_stations <- data[, .(lat = s_lat, lng = s_lng, name = s_name, 
                         date = s_date, time = s_time)
                    ]#[, .(n = .N), by=.(lat, lng, name)]

in_stations <- data[, .(lat = e_lat, lng = e_lng, name = e_name, 
                        date = s_date, time = e_time)
                  ]#[, .(n = .N), by=.(lat, lng, name)]






in_stations <- as.data.table(in_stations)
out_stations <- as.data.table(out_stations)

pre_stat <- in_stations[, .(n = .N), by=.(lat, lng, name)]

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