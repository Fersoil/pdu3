library(data.table)
library(stringi)
library(ggplot2)

# x07 <- as.data.table(`201907`)[,
#   .(Gender = fcase( 
#     gender == 1, "Men",
#     gender == 2, "Women",
#     gender == 0, "Undefined"
#    ), TypeOfTrip = fcase(
#       tripduration <= 600, "short",
#       tripduration > 600 & tripduration <= 1800, "medium",
#       default = "long"
#    ))][, .(Count = .N), .(Gender, TypeOfTrip)][, .(
#   ShortTrips = max(fcase(
#     TypeOfTrip == 'short', Count,
#     default = 0L)),
#   MediumTrips = max(fcase(
#     TypeOfTrip == 'medium', Count,
#     default = 0L)),
#   LongTrips = max(fcase(
#     TypeOfTrip == 'long', Count,
#     default = 0L))
#   ), Gender]

x <- as.data.table(rbind(`201901`, `201902`, `201903`, `201904`, `201905`, `201906`, `201907`,
                         `201908`, `201909`, `201910`, `201911`,`201912`))[, .(
                              TypeOfTrip = fcase(
                                 tripduration <= 600, "Short",
                                 tripduration > 600 & tripduration <= 1800, "Medium",
                                 default = "Long"),
                              Season = fcase(
                                stri_sub(starttime, 6, 7) == "01" | stri_sub(starttime, 6, 7) == "02" | stri_sub(starttime, 6, 7) == "12", "Winter",
                                stri_sub(starttime, 6, 7) == "03" | stri_sub(starttime, 6, 7) == "04" | stri_sub(starttime, 6, 7) == "05", "Spring",
                                stri_sub(starttime, 6, 7) == "06" | stri_sub(starttime, 6, 7) == "07" | stri_sub(starttime, 6, 7) == "08", "Summer",
                                default = "Autumn"
                                ))][, .(Count = .N), .(Season, TypeOfTrip)][,.(Season, TypeOfTrip, Count = Count/3)]
x

ggplot(data = x, aes(Season, Count)) + geom_bar(stat = "identity", position = "dodge", aes(fill=TypeOfTrip)) +
  labs(x = "Season", y="Count") + scale_x_discrete(limits = c("Spring", "Summer", "Autumn", "Winter"))
