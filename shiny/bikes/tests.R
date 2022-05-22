# tests for proccessing the dates
date1 <- ymd("2019-01-04 UTC")
date2 <- ymd("2019-05-04 UTC")
date_interval <- interval(date1, date2)


print(head(out_stations$date))
out_stations[date %within% date_interval, ][, .(n = .N), by=.(lat, lng, name)]

#tests for proccessing the time 
time1 <- lubridate::origin+minutes(50)
time2 <- lubridate::origin+minutes(180)
time_interval <- interval(time1, time2)
time_interval

t <- "2019-12-01 06:07:13.9360"

t <- as.ITime(t)
typeof(t)
t
between(t, as.ITime('06:00:00'), as.ITime('09:00:00'))
?between

in_stations[between(time, as.ITime('06:00:00'), as.ITime('09:00:00'))]


head(out_stations[time %within% time_interval, ])

as.POSIXct("1970-01-01 10:00:00")
