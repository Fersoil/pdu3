# zrobimy sobie takie w miare proste zestawienie na początek. Będziemy sprawdzać,
# czy starzy ludzie jeżdżą na rowerach wolniej niż młodzi.
# W tym celu wczytajmy najpierw dane


source("better_load_files.R")
library(data.table)
library(ggplot2)


# rozwiązanie z którego nie skorzystamy - nie bedziemy rozwazac jak rózne generacje
# jezdza na rowerach
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

# skupimy sie na tym z jaka predkoscia jezdza osoby urodzone w róznych dziesiecioleciach
decode_generations <- function(x) {paste(x, "0.", sep="")}

generations <- function(x){
  substr(x, nchar(x)-1, nchar(x)-1)
}

# to jak chcemy miec ułożone generacje - od najstarszych do najmlodszych
generations_sorted <- c("40.", "50.", "60.", "70.", "80.", "90.", "00.", "10.")


# naiwne wyliczanie odległości - wyznaczamy odległość w jak najszybszy sposób
# liczymy tylko odległość między dwoma stacjami 
# być moze TODO - czy nie wywalać skrajnie wolnych podróży - jasne, że nie kazdy jedzie 
# bezposrednio ze stacji do stacji.
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


#operacje na porach roku

season_names <- c('winter', 'spring', 'summer', 'autumn')

season_func <- function(x) {
  season <- fcase(
  stri_sub(x, 6, 7) == "01" | stri_sub(x, 6, 7) == "02" | 
    stri_sub(x, 6, 7) == "12", 1,
  stri_sub(x, 6, 7) == "03" | stri_sub(x, 6, 7) == "04" | 
    stri_sub(x, 6, 7) == "05", 2,
  stri_sub(x, 6, 7) == "06" | stri_sub(x, 6, 7) == "07" | 
    stri_sub(x, 6, 7) == "08", 3,
  default = 4)
  return(season_names[season])
}

# generowanie wykresów dla ramki danych

generate_plot <- function(x) {
  ggplot(data = x, aes(gen, avg_speed)) + geom_bar(stat = "identity")
}

# sprawdzamy czy wyznaczanie odleglosci dziala
dist(40.71625, -74.03346, 40.71277, -74.03649)
 # odległość z mapy 439 m



# res <- data.table()
# 
# par(mfrow = c(3, 4))
# 
# for(el in filenames){
#   var <- eval(parse(text = el))
#   var <- as.data.table(var)
#   
#   var <- var[, .(gen = generations(birth.year), tripduration, 
#                    dist = dist(start.station.latitude, start.station.longitude,
#                                end.station.latitude, end.station.longitude))
#     ][dist != 0, .(gen, speed = dist/tripduration)]
#   
#   var <- var[, .(avg_speed = mean(speed), n=.N), by=.(gen)
#     ][order(gen), .(gen = decode_generations(gen), avg_speed=avg_speed*3.6)]
#   
#   
#   s <- var$avg_speed
#   names(s) <- var$gen
#   res <- 
#   barplot(s, las = 1)
#   
# }
# 
# par(mfrow = c(1, 1))


data <- data.table()

for(el in data_names){
  data <- rbind(eval(parse(text = el)), data)
}

data <- as.data.table(data)
data

colnames(data)

data <- data[, .(season = season_func(starttime), gen = generations(birth.year),
                 tripduration, dist = dist(start.station.latitude, 
                  start.station.longitude, end.station.latitude, end.station.longitude))
            ][dist != 0, .(season, gen, speed = dist/tripduration)
            ][, .(avg_speed = mean(speed), n=.N), by=.(season, gen)
            ][, .(season, gen = decode_generations(gen), avg_speed=avg_speed*3.6)]

data
seasons_data <- lapply(1:4, function(x)
  {data[season==season_names[x], .(gen, avg_speed)]})

par(mfrow = c(2, 2))

for(i in 1:4){
  seasons_data[[i]] <- seasons_data[[i]][match(intersect(generations_sorted, gen), gen), ]
}

#setorder(seasons_data[[3]], generations_sorted)


# raczej niepotrzebne
for(el in seasons_data){
  
  
  
  #el <- merge(as.data.table(generations_sorted), el,
  #            by.x = "generations_sorted", by.y = "gen")[
  #              , .(gen = generations_sorted, avg_speed)]
  
  print(el)
  print(typeof(el))
  s <- el$avg_speed
  names(s) <- el$gen
  barplot(s, las = 1)
}


plots <- lapply(1:4, function(x) generate_plot(seasons_data[[x]]))

cowplot::plot_grid(plotlist = plots, nrow = 2)



png("out/wykresiki.png")
dev.off()

