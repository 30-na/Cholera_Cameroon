
# load libraries
library(ncdf4)
library(dplyr)
library(reshape2)
library(ggplot2)
library(viridis)

#open netCDF files precipitation
nc_SOILMOISTURE = nc_open("rawData/ESACCI-SOILMOISTURE-L3S-SSMS-ACTIVE-20180101000000-fv06.2.nc")

# get the variable names
names(nc_SOILMOISTURE$var)
names(nc_SOILMOISTURE$dim)

# set the variables names
x = "lon"
y = "lat"
t = "time"

# get longitude, latitude and time
lon <- sort(ncvar_get(nc_SOILMOISTURE,x))
lat <- sort(ncvar_get(nc_SOILMOISTURE,y))
time <- ncvar_get(nc_SOILMOISTURE,t)

# Convert numeric time values to date format
date <- as.Date("1970-01-01") + (time)

# get precipitation
SOILMOISTURE_array <- ncvar_get(nc_SOILMOISTURE, "sm")
dim(SOILMOISTURE_array)

# extract some information from file 
fullname <- ncatt_get(nc_SOILMOISTURE,"sm","long_name")
dunits <- ncatt_get(nc_SOILMOISTURE,"sm","units")
title <- ncatt_get(nc_SOILMOISTURE,0,"title")
institution <- ncatt_get(nc_SOILMOISTURE,0,"institution")
datasource <- ncatt_get(nc_SOILMOISTURE,0,"source")
references <- ncatt_get(nc_SOILMOISTURE,0,"references")
history <- ncatt_get(nc_SOILMOISTURE,0,"history")
Conventions <- ncatt_get(nc_SOILMOISTURE,0,"Conventions")

# replace netCDF fill values with NA's
fillvalue <- ncatt_get(nc_SOILMOISTURE,"sm","_FillValue")
SOILMOISTURE_array[SOILMOISTURE_array==fillvalue$value] <- NA

# close the file
nc_close(nc_SOILMOISTURE)

# Reshape the array to a long format
long_SOILMOISTURE <- melt(SOILMOISTURE_array)
dim(long_SOILMOISTURE)
# Rename the columns
colnames(long_SOILMOISTURE) <- c("Longitude", "Latitude", "SoilMoisture")

# Create vectors for standard latitude and longitude ranges
#standard_latitude <- seq(-89.75, 89.75, length.out = 360)
#standard_longitude <- seq(-179.75, 179.75, length.out = 720)

# add date and standardize the cordination
SOILMOISTURE_data = long_SOILMOISTURE %>%
    mutate(Date = rep(date, each = (dim(SOILMOISTURE_array)[1] * dim(SOILMOISTURE_array)[2]))
          # Latitude = standard_latitude[Latitude],
          # Longitude = standard_longitude[Longitude]
    )


# save file as RDA
save(SOILMOISTURE_data,
     file = "processedData/CEDA_SOILMOISTURE_data.rda")


# Create the ggplot object and specify the aesthetics and geometry
g = ggplot(SOILMOISTURE_data,
           aes(x = Longitude,
               y = Latitude,
               color = SoilMoisture)) +
    geom_point() +
    scale_color_viridis(option = "magma", direction = -1) +
    labs(title = "Random Precipitation Sample from CEDA Dataset (2018-01-01)",
         x = "Longitude",
         y = "Latitude")

ggsave("Figures/CEDA_sample_SOILMOISTURE.jpg",
       g,
       height=4,width=8,scale=1.65)



# Create the ggplot object and specify the aesthetics and geometry
# Create the ggplot object and specify the aesthetics and geometry
ggplot(SOILMOISTURE_data, aes(x = Longitude, y = Latitude, fill = SoilMoisture)) +
    geom_raster() +  # Use geom_raster for regular grids
    scale_fill_viridis(option = "magma", direction = 1, na.value = "gray") +
    labs(title = "Soil Moisture Map (2018-01-01)",
         x = "Longitude",
         y = "Latitude") +
    coord_cartesian(xlim = c(min(Longitude), max(Longitude)), ylim = c(min(Latitude), max(Latitude)))

ggsave("Figures/SOILMOISTURE_map.jpg", g, height = 6, width = 10, scale = 1.5)
