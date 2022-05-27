source("better_load_files.R")
library(data.table)
library(ggplot2)
library(plotly)

this_year <- 2019


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

age <- function(birth.year) {
  this_year - birth.year
}

gend <- function(gender) {
  res <- fcase(
    gender == 0, "unknown",
    gender == 1, "male",
    default = "female")
  return(res)
}


# sprawdzamy czy wyznaczanie odleglosci dziala
dist(40.71625, -74.03346, 40.71277, -74.03649)
# odległość z mapy 439 m




# mamy juz dobre dane do dalszego procesowania
# chcemy teraz patrzec sobie czy starsi ludzi jezdzą znacznie wolniej,
# istotna tez bedzie dla nas płeć
head(data)

d <- data[, .(age = age(birth.year), tripduration, 
                 dist = dist(start.station.latitude, start.station.longitude, 
                             end.station.latitude, end.station.longitude), gender)
            ][dist != 0, .(age, speed = dist/tripduration, dist, gender)
            ][between(age, 15, 70), .(age, dist, speed = speed*3.6, gender = gend(gender))
              ][gender != "unknown", ]

# i uknown gender tez sie pozbywamy bo nie wiadomo co to
# nie bedziemy rozwazac takich dziwnych przypadkow jak wiek wiekszy od 80 lat, bo raczej tacy ludzie nie żyją

# agregujemy 

# fem_data <- d[gender == "female", .(fem_avg_speed = mean(speed), fem_avg_dist = mean(dist)), 
#               by = .(age)]
# male_data <- d[gender == "male", .(mal_avg_speed = mean(speed), mal_avg_dist = mean(dist)), 
#                by = .(age)]

data_to_plot <- d[, .(avg_speed = mean(speed), avg_dist = mean(dist), rides = .N), 
                                by = .(age, gender)]

head(data_to_plot)
# plotting stuff

data_to_plot[, .(avg = mean(avg_speed)), by = .(gender)]


ggplot(data = data_to_plot, aes(x = age, y = avg_speed, group = gender)) +
  geom_line(aes(colour = gender)) +
  ylab("Average speed in km/h") + 
  xlab("User age") +
  scale_color_manual(values=c('Red','Blue')) +
  labs(title = "Average speed by gender")-> plot1
plot1
fig1 <- ggplotly(plot1) %>%
  layout(yaxis = list(title = "average speed in km/h"))

ggplot(data = data_to_plot, aes(x = age, y = avg_dist, group = gender)) +
  geom_line(aes(colour = gender)) +
  ylab("Average speed in m") + 
  xlab("User age") +
  scale_color_manual(values=c('Red','Blue')) +
  labs(title = "Average distance by gender")-> plot2
plot2
fig2 <- ggplotly(plot2) %>%
  layout(yaxis = list(title = "average distance in m"))

ggplot(data = data_to_plot, aes(x = age, y = rides, group = gender)) +
  geom_line(aes(colour = gender)) +
  ylab("number of rides") + 
  xlab("User age") +
  scale_color_manual(values=c('Red','Blue')) +
  labs(title = "Number of riders")-> plot3
plot3
fig3 <- ggplotly(plot3) %>%
  layout(yaxis = list(title = "number of rides"))

fig <- subplot(fig1, fig2, fig3, nrows = 3, shareX = TRUE, 
               titleY = TRUE, titleX = TRUE, margin = 0.1) %>%
  layout(title = "Average speed, distance and number of rides by age and gender",
         zerolinewidth = 2)

fig
