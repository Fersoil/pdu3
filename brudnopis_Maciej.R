source("load_files_NYC2019.R")
library(ggplot2)
library(cowplot)

data <- data.table()

for(el in data_names){
  data <- rbind(eval(parse(text = el)), data)
}

x <- as.data.table(data)[, .(
  TypeOfTrip = fcase(
    tripduration <= 480, "1-UltraShort",
    tripduration > 480 & tripduration <= 900, "2-Short",
    tripduration > 900 & tripduration <= 1800, "3-Medium",
    tripduration > 1800 & tripduration <= 3600, "4-Long",
    default = "5-UltraLong"),
  Season = fcase(
    stri_sub(starttime, 6, 7) == "01" | stri_sub(starttime, 6, 7) == "02" | 
      stri_sub(starttime, 6, 7) == "12", "Winter",
    stri_sub(starttime, 6, 7) == "03" | stri_sub(starttime, 6, 7) == "04" | 
      stri_sub(starttime, 6, 7) == "05", "Spring",
    stri_sub(starttime, 6, 7) == "06" | stri_sub(starttime, 6, 7) == "07" | 
      stri_sub(starttime, 6, 7) == "08", "Summer",
    default = "Autumn")
  )][, .(Count = .N), .(Season, TypeOfTrip)][,.(
    Season, TypeOfTrip, AverageCount = Count/3)]


generate_column_plot <- function(DataFrame) {
  
  ggplot(data = DataFrame, aes(Season, AverageCount)) + 
    geom_bar(stat = "identity", position = "dodge", aes(fill=TypeOfTrip)) +
    labs(x = "Season", y="AverageCount") + 
    scale_x_discrete(limits = c("Spring", "Summer", "Autumn", "Winter"))
  
}

allgenders <- generate_plot(x)
men <- generate_plot(x)
women <- generate_plot(x)
undefined <- generate_plot(x)

plot_grid(allgenders, men, women, undefined, labels = c("All genders", "Men", "Women", "Undefined"))


# ------------------------------------------------------------------------------


x2 <- as.data.table(data)[,
  .(gender, TypeOfTrip = fcase(
     tripduration <= 480, "1-UltraShort",
     tripduration > 480 & tripduration <= 900, "2-Short",
     tripduration > 900 & tripduration <= 1800, "3-Medium",
     tripduration > 1800 & tripduration <= 3600, "4-Long",
     default = "5-UltraLong")
   )][, .(Count = .N), .(gender, TypeOfTrip)][gender == 1, .(
     TypeOfTrip, Count, Percent = round(Count/sum(Count) * 100, 1))]

x2


generate_pie_plot <- function(DataFrame) {
  
  ggplot(DataFrame, aes(x="", y=Count, fill=TypeOfTrip)) +
    geom_bar(stat="identity", width=1, color="white") +
    geom_label(aes(label = paste(Percent, "%", sep="")),
               position = position_stack(vjust = 0.5, ),
               show.legend = FALSE) +
    coord_polar("y", start=0) + theme_void()
}


Men <- generate_pie_plot(x2)
Women <- generate_pie_plot(x2)
Undefined <- generate_pie_plot(x2)
All <- generate_pie_plot(x2)


All
plot_grid(All, Men, Women, Undefined, labels = c("All genders", "Men", "Women", "Undefined"))


