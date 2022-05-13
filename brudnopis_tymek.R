# zrobimy sobie takie w miare proste zestawienie na początek. Będziemy sprawdzać,
# czy starzy ludzie jeżdżą na rowerach.
# W tym celu wczytajmy najpierw dane


source("load_files - tymek.R")
library(data.table)
library(ggplot2)



generations_1 <- function(x) {
  data.table::fcase(
    x <= 1900, 0L,
    x <= 1924, 1L, 
    x <= 1945, 2L,
    x <= 1964, 3L,
    x <= 1980, 4L,
    x <= 1996, 5L,
    x <= 2012, 6L,
    x <= 2025, 7L,
    TRUE, NA
  )
}

decode_generations_1 <- function(x) {
  gen <- c("The Lost Generation", "The Greatest Generation", "The Silent Generation",
           "Baby Boomer Generation", "Generation X", "Generation Y", "Generation Z",
           "Generation Alpha")
  return(gen[x+1])
}


decode_generations <- function(x) {paste(x, "0.", sep="")}

generations <- function(x){
  substr(x, nchar(x)-1, nchar(x)-1)
}

dist <- function(lat1, lon1, lat2, lon2) {
  lat1 <- lat1 * pi / 180
  lat2 <- lat2 * pi / 180
  lon1 <- lon1 * pi / 180
  lon2 <- lon2 * pi / 180
  
  # Equirectangular approximation
  R <- 6371000
  x <- (lon2-lon1) * cos((lat1+lat2)/2)
  y <- (lat2-lat1)
  d <- sqrt(x*x + y*y) * R;
  return(d)
}

# tutaj nazwy generacji


dist(40.71625, -74.03346, 40.71277, -74.03649)
 # odległość z mapy 439 m

data <- data.table()

for(el in filenames){
  data <- rbind(eval(parse(text = el)), res)
}

data <- as.data.table(data)

colnames(data)

data <- data[, .(gen = generations(birth.year), tripduration, 
                 dist = dist(start.station.latitude, start.station.longitude,
                             end.station.latitude, end.station.longitude))
             ][dist != 0, .(gen, speed = dist/tripduration)]

data <- data[, .(avg_speed = mean(speed), n=.N), by=.(gen)
             ][order(gen), .(gen = decode_generations(gen), avg_speed=avg_speed*3.6)]



hist(data)
s <- data$avg_speed
names(s) <- data$gen
barplot(s, las = 1)

ggplot(data, aes(x=data$gen, y=data$speed)) + 
  geom_histogram(binwidth = 1, stat="identity")

