library(leaflet)
library(dplyr)
library(stringi)
library(data.table)
library(shiny)
library(leaflet.extras)
library(RColorBrewer)
library(lubridate)
library(sf)
library(httr)
library(maptools)
library(rgdal)
library(ggplot2)
library(plotly)

options(stringsAsFactors = FALSE)

download_data = FALSE

borough_list = c("East Village", "Upper West Side", "Financial District", "Midtown", "Central Park", "Williamsburg")


## loading files


page_prefix <- "https://s3.amazonaws.com/tripdata/"
sufix <- "-citibike-tripdata.csv"
dest_dir <- "data/"


i <- sapply(as.character(4:6), FUN = function(x) {ifelse(nchar(x) == 1, paste("0",x, sep=""), x)})

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


new_york_cords = c(-74.035242, 40.730610)




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


r <- GET('https://data.beta.nyc/dataset/0ff93d2d-90ba-457c-9f7e-39e47bf2ac5f/resource/35dd04fb-81b3-479b-a074-a27a37888ce7/download/d085e2f8d0b54d4590b1e7d1f35594c1pediacitiesnycneighborhoods.geojson')
nyc_neighborhoods <- readOGR(content(r,'text'), 'OGRGeoJSON', verbose = F)

nyc_polygons <- st_as_sf(nyc_neighborhoods)

# end of loading files


prepare_to_plot <- function(df) {
  coords <- df[, .(lng, lat)]
  points <- SpatialPointsDataFrame(coords, data = df)
  
  
  points <- st_as_sf(points)
  points <- points %>% st_set_crs(st_crs(nyc_polygons))
  
  poly <- st_join(nyc_polygons, points, left=FALSE)
  poly <- st_drop_geometry(poly)
  poly <- poly[c("neighborhood", "date", "time")]
  
  poly <- as.data.table(poly)
  
  
  to_plot <- poly[neighborhood %in% borough_list, 
  ][, .(weekday=weekdays(date), neighborhood, time)
  ][weekday!="sobota" & weekday!="niedziela", 
  ][, .(time = (time %/% 300) *300, neighborhood)
  ][, .(rides = .N), by=.(time, neighborhood)]
}

in_to_plot <- prepare_to_plot(in_stations)
out_to_plot <- prepare_to_plot(out_stations)


test <- in_stations[, .(time= (time %/% 300) * 300 )
][, .(rides = .N), by=.(time)]
head(test)

plot1 <- ggplot(in_to_plot, aes(time, rides, group=neighborhood)) +
  geom_line(aes(colour = neighborhood), size=1.2) +
  xlab("godzina") +
  labs(title = "Liczba wypożyczeń rowerów w danych dzielnicach", subtitle = "odjazdy z dzielnicy") +
  scale_x_continuous(breaks = seq(0, 86400, 3600*6), labels = paste(seq(0, 24, 6), "00", sep = ":"))


fig1 <- ggplotly(plot1, showlegend=TRUE) %>%
  layout(xaxis = list(title="liczba przyjazdów do dzielnicy"))

plot2 <- ggplot(out_to_plot, aes(time, rides, group=neighborhood)) +
  geom_line(aes(colour = neighborhood), size=1.2) +
  xlab("godzina") +
  labs(title = "Liczba wypożyczeń rowerów w danych dzielnicach", subtitle = "przyjazdy do dzielnicy") +
  scale_x_continuous(breaks = seq(0, 86400, 3600*6), labels = paste(seq(0, 24, 6), "00", sep = ":"))

fig2 <- ggplotly(plot2, showlegend=FALSE) %>%
  layout(xaxis = list(title="liczba wyjazdów z dzielnicy"))

fig <- subplot(fig1, fig2, nrows = 1, 
               titleY = TRUE, titleX = TRUE, margin = 0.1) %>%
  layout(title = "Liczba odjazdów oraz przyjazdów do dzielnic",
         zerolinewidth = 2)

