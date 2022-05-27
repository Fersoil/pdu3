# tests for proccessing the dates
date1 <- ymd("2019-01-04 UTC")
date2 <- ymd("2019-05-04 UTC")
date_interval <- interval(date1, date2)

# zabawy z czasem
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




color_pallete <- "RdYlBu"
colorpal <- function() {
  colorNumeric(color_pallete, pre_stat$n)
}

pal <- colorpal()

map <- leaflet(data = pre_stat) %>%
  addTiles() %>%
  fitBounds(~min(lng), ~min(lat), ~max(lng), ~max(lat)) %>%
  clearMarkers() %>%
  addCircleMarkers(lng = ~lng, lat = ~lat,
                   weight = 1,
                   fillColor = ~pal(n),
                   color = "#777777",
                   popup = ~paste(n),
                   stroke = FALSE,
                   radius = 5,
                   label = ~name,
                   fillOpacity = 0.9
  )
map

heat_map <- pre_stat %>%
  leaflet() %>%
  addTiles() %>%
  fitBounds(~min(lng), ~min(lat), ~max(lng), ~max(lat)) %>%
  addHeatmap(lng=~lng, lat=~lat, intensity=NULL, blur = 15, max = 0.1, radius = 10)

heat_map <- addHeatmap()

??readOGR



leaflet(nyc_neighborhoods) %>%
  addTiles() %>% 
  addPolygons(popup = ~neighborhood) %>%
  addProviderTiles("CartoDB.Positron")


pre_stat_hood <- 
stat
points_by_hood <- merge(pre)


?readOGR


##### jeszcze jedna próba
library(data.table)
library(sp)
library(rgdal)
library(dplyr)
library(ggpubr)
library(grid)
library(leaflet)


pre_stat <- in_stations[1:10000, ]
coordinates(pre_stat) = c("lng","lat")
crs.geo1 = CRS("+proj=longlat")
proj4string(pre_stat) = crs.geo1



library(httr)
library(maptools)
library(rgdal)


r <- GET('https://data.beta.nyc/dataset/0ff93d2d-90ba-457c-9f7e-39e47bf2ac5f/resource/35dd04fb-81b3-479b-a074-a27a37888ce7/download/d085e2f8d0b54d4590b1e7d1f35594c1pediacitiesnycneighborhoods.geojson')
nyc_neighborhoods <- readOGR(content(r,'text'), 'OGRGeoJSON', verbose = F)
summary(nyc_neighborhoods)

proj4string(nyc_neighborhoods) = crs.geo1


pre_stat <- as.data.table(pre_stat)
pre_stat <- pre_stat[, .(n = .N), by=.(lat, lng, name)]
agg_stat <- aggregate(x=pre_stat$n, by=nyc_neighborhoods, FUN = sum)

qpal = colorBin("Reds", agg_stat$n, bins=4)
leaflet(pre_stat) %>%
  addPolygons(stroke = TRUE, opacity = 1, fillOpacity = 0.5, smoothFactor = 0.5,
              color = "black", fillColor = ~qpal(n), weight = 1) %>%
  addLegend(values = ~n, pal = qpal, title="rowerki")


##### koniec próby


library(albersusa)
library(leaflet.extras)
library(sf)

spatial <- st_as_sf(nyc_neighborhoods)

coords <- pre_stat[, .(lng, lat)]
spatial_points <- SpatialPointsDataFrame(coords, data = pre_stat)

joined <- st_join(nyc_neighborhoods, spatial_points)

spdf <- rmapshaper::ms_simplify(nyc_neighborhoods, keep = 0.1)
pal <- colorNumeric("Blues", domain = spdf$pop_2014)
epsg2163 <- leafletCRS(
  crsClass = "L.Proj.CRS",
  code = "EPSG:2163",
  proj4def = "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs",
  resolutions = 2^(16:7))

leaflet(spdf, options = leafletOptions(crs = epsg2163)) %>%
  addTiles() %>%
  addPolygons(weight = 1, color = "#444444", opacity = 1, 
              fillOpacity = 0.7, smoothFactor = 0.5,
              label = ~neighborhood,
              labelOptions = labelOptions(direction = "auto"))  %>%
  addProviderTiles("CartoDB.Positron")



####### another try
leaflet() %>% 
  addProviderTiles(providers$CartoDB.Voyager) %>% 
  addPolygons(data = nyc_neighborhoods) %>% 
  addCircles(data = pre_stat)



coords <- pre_stat[, .(lng, lat)]
spatial_points <- SpatialPointsDataFrame(coords, data = pre_stat)


points <- st_as_sf(spatial_points)
polygons <- st_as_sf(nyc_neighborhoods)

points
polygons
# musimy ustalic crs dla pointsow - teraz jest to null
points <- points %>% st_set_crs(st_crs(polygons))

joined_polygons <- st_join(polygons, points, left=FALSE)
joined_polygons <- as.data.table(joined_polygons)
joined_polygons <- joined_polygons[, .(neighborhood, boroughCode, borough, n, geometry)]
joined



colnames(joined_polygons)


joined_polygons <- as.data.table(joined_polygons)
joined_polygons <- joined_polygons[, .(n=sum(n)), by=.(neighborhood)]

agg_poly <- joined_polygons[, .(sum=sum(n)), by=.(neighborhood)]
joined_poly <- merge(agg_poly, polygons)
joined_poly
pal <- colorBin("YlOrRd", domain = poly$avg, bins = 5)
poly$area <- st_area(poly)
poly$avg <- poly$sum / poly$area

leaflet(poly) %>%
  addTiles() %>%
  addPolygons(fillColor = ~pal(avg*1000),
              weight = 2,
              opacity = 1,
              color = "black",
              dashArray = "3",
              fillOpacity = 0.7,
              label = labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")
              ) %>%
  addProviderTiles("CartoDB.Positron") 

labels <- sprintf(
  "<strong>%s</strong><br/>%g rents / m<sup>2</sup>",
  poly$neighborhood, poly$avg*1000
) %>% lapply(htmltools::HTML)

poly <- st_as_sf(joined_poly)
poly
