#install.packages("ggplot2")
#install.packages("maps")
#install.packages("sf")
#install.packages("rnaturalearth")
#install.packages("rnaturalearthdata")

library(ggplot2)
library(maps)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

# Adjust as needed
scriptDir <- "/Users/felixkim/Desktop/case-competition-/Experimental/Felix_files"
outputDir <- "/Users/felixkim/Desktop/case-competition-/Experimental/Felix_files/OutputGraphs/Geo/"
currYear <- 2021

setwd(scriptDir)

exposuresData <- read.csv("../dataset/Exposures.csv", skip = 4, header = TRUE, sep = ",")
exposuresData <- exposuresData[exposuresData$PolicyYear == currYear,]
str(exposuresData)

# Latitude and longitude coordinates (e.g., city locations) along with Location ID's and TIV
geo_df <- exposuresData[c("Location", "Latitude", "Longitude", "Total.Insured.Value")]

coords <- data.frame(
  city = geo_df[["Location"]],
  lat = geo_df[["Latitude"]],
  lon = geo_df[["Longitude"]],
  tiv_millions = format(round(geo_df[["Total.Insured.Value"]] / 1e6, 2), nsmall = 2)
)

# Create map
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

png(filename=paste0(outputDir, "locations_of_interest.png"), width=1200, height=1200)

p<-ggplot(data = world) +
  geom_sf() +
  geom_point(data = coords, aes(x = lon, y = lat, size = tiv_millions), color = "green") +
  geom_text(data = coords, aes(x = lon, y = lat, label = paste0(city, " (", tiv_millions, ")")), 
            vjust = -1, color = "black", fontface = "bold") +
  coord_sf(xlim = c(-125, -50), ylim = c(-25, 60), expand = FALSE) +
  ggtitle("Locations in portfolio with TIV (in millions of USD)")+
  theme(plot.title = element_text(size = 40, face = "bold"))

print(p)

invisible(dev.off())