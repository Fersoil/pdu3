source("load_files_NYC2019.R")
library(ggplot2)
library(ggrepel)
library(cowplot)

# funkcje pomocnicze

# przypisanie rodzaju podróży w zależności od jej trwania
assign_trip_duration <- function(tripduration) {
  fcase(
    tripduration <= 480, "1-UltraShort",
    tripduration > 480 & tripduration <= 900, "2-Short",
    tripduration > 900 & tripduration <= 1800, "3-Medium",
    tripduration > 1800 & tripduration <= 3600, "4-Long",
    default = "5-UltraLong")
}

# przypisanie pory roku w zależności od miesiąca
assign_seasons <- function(starttime) {
  fcase(
    stri_sub(starttime, 6, 7) == "01" | stri_sub(starttime, 6, 7) == "02" | 
      stri_sub(starttime, 6, 7) == "12", "Winter",
    stri_sub(starttime, 6, 7) == "03" | stri_sub(starttime, 6, 7) == "04" | 
      stri_sub(starttime, 6, 7) == "05", "Spring",
    stri_sub(starttime, 6, 7) == "06" | stri_sub(starttime, 6, 7) == "07" | 
      stri_sub(starttime, 6, 7) == "08", "Summer",
    default = "Autumn")
}


# (1) Procentowe porównanie ilości podróży każdego rodzaju dla różnych pór roku

generate_data <- function(season) {
  
  data[, .(
    TypeOfTrip = assign_trip_duration(tripduration),
    Season = assign_seasons(starttime)
  )][, .(Count = .N), .(Season, TypeOfTrip)][Season == season,.(
    TypeOfTrip, Count, Percent = round(Count/sum(Count) * 100, 1))]
  
}

generate_pie_plot <- function(DataFrame) {
  
  ggplot(DataFrame, aes(x="", y=Count, fill=TypeOfTrip)) +
    geom_bar(stat="identity", width=1, color="white") +
    geom_label_repel(aes(label = paste(Percent, "%", sep="")),
                     position = position_stack(vjust = 0.5),
                     show.legend = FALSE) +
    coord_polar("y", start=0) + theme_void()
}

plot1 <- plot_grid(generate_pie_plot(generate_data("Spring")),
                   generate_pie_plot(generate_data("Summer")),
                   generate_pie_plot(generate_data("Autumn")),
                   generate_pie_plot(generate_data("Winter")),
                   labels = c("Spring", "Summer", "Autumn", "Winter"))
plot1


# (2) Procentowe porównanie ilości podróży każdego rodzaju dla różnych płci

generate_data_2 <- function(Gender="All") {
  
  if(Gender=="All") {
    
    x <- data[, .(TypeOfTrip = assign_trip_duration(tripduration)
    )][, .(Count = .N), .(TypeOfTrip)][, .(
      TypeOfTrip, Count, Percent = round(Count/sum(Count) * 100, 1))]
    
  }
  else{
    
    x <- data[, .(gender, TypeOfTrip = assign_trip_duration(tripduration)
    )][, .(Count = .N), .(gender, TypeOfTrip)][gender == Gender, .(
      TypeOfTrip, Count, Percent = round(Count/sum(Count) * 100, 1))]
    
  }
  
}

plot2 <- plot_grid(generate_pie_plot(generate_data_2()),
                   generate_pie_plot(generate_data_2(1)),
                   generate_pie_plot(generate_data_2(2)),
                   generate_pie_plot(generate_data_2(0)),
                   labels = c("All genders", "Men", "Women", "Undefined"))
plot2



# (3) Porównanie ilości podróży każdego rodzaju dla różnych pór roku w zależności od płci

generate_data_3 <- function(Gender = "All") {
  
  x <- data
  
  if(Gender!="All") {x <- x[gender == Gender]}

  x[, .(
    TypeOfTrip = assign_trip_duration(tripduration),
    Season = assign_seasons(starttime)
  )][, .(Count = .N), .(Season, TypeOfTrip)][,.(
    Season, TypeOfTrip, AverageCount = Count/3)]
  
}

generate_column_plot <- function(DataFrame) {
  
  ggplot(data = DataFrame, aes(Season, AverageCount)) + 
    geom_bar(stat = "identity", position = "dodge", aes(fill=TypeOfTrip)) +
    labs(x = "Season", y="AverageCount") + 
    scale_x_discrete(limits = c("Spring", "Summer", "Autumn", "Winter"))
  
}

plot3 <- plot_grid(generate_column_plot(generate_data_3()),
                  generate_column_plot(generate_data_3(1)),
                  generate_column_plot(generate_data_3(2)),
                  generate_column_plot(generate_data_3(0)),
                  labels = c("All genders", "Men", "Women", "Undefined"))

plot3

