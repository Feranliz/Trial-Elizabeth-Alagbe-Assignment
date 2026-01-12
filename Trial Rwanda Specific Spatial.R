if (!require('geodata')) install.packages("geodata")
if (!require('terra')) install.packages("terra")

library(geodata)
library(terra)

# Administrative Shapfiles---------------------------------------------------

rwa0 = gadm(country = "RWA", level = 0, path = "Geodata")
plot(rwa1)
rwa1 = gadm(country = "RWA", level = 1, path = "Geodata")
plot(rwa1)

# Cropland mask ---------------------------------------------------------------

cropland = geodata::cropland(source = "WorldCover", year = 2019, path = "Geodata")
cropland = crop(cropland, rwa0)
cropland = mask(cropland, rwa0)
cropland = ifel(cropland$cropland > 0, 1, NA)
plot(rwa0)
plot(cropland, col = "forestgreen", add = T, legend = F)
plot(rwa1, add = T)

# Elevation and slope -------------------------------------

elevation = geodata::elevation_30s(country = "RWA", path = "Geodata")
elevation = crop(elevation, cropland)
elevation = mask(elevation, cropland)
plot(rwa0)
plot(elevation, add = T, legend = T)
plot(rwa1, add = T)

slope = terrain(elevation, v = "slope", unit = "degrees")
names(slope) = "slope"
plot(rwa0)
plot(slope, add = T, legend = T)
plot(rwa1, add = T)

# Rainfall -------------------------------------------------

prec = worldclim_country(country = "RWA", var = "prec", path = "Geodata", res = 0.5)
prec = sum(prec)
prec = crop(prec, cropland)
prec = mask(prec, cropland)
names(prec) = "prec"
plot(rwa0)
plot(prec, add = T, legend = T)
plot(rwa1, add = T)

# Soil texture---------------------------------------

sand_5  = soil_af(var = "sand", depth = 5, path = "Geodata")
sand_15 = soil_af(var = "sand", depth = 15, path = "Geodata")
sand_30 = soil_af(var = "sand", depth = 30, path = "Geodata")

sand = (sand_5*5 + sand_15*10 + sand_30*15) / 30
sand = crop(sand, cropland)
sand = mask(sand, cropland)
names(sand) = "sand"
plot(rwa0)
plot(sand, add = T, legend = T)
plot(rwa1, add = T)

# Population ---------------------------------------------------------

pop = geodata::population(year = 2020, path = "Geodata")
pop = resample(pop, cropland)
pop = crop(pop, cropland)
pop = mask(pop, cropland)
names(pop) = "pop"
plot(rwa0)
plot(pop, add = T, legend = T)
plot(rwa1, add = T)


travel_time = travel_time(to = "city", size = 6, up = TRUE, path = "Geodata")
travel_time = crop(travel_time, cropland)
travel_time = mask(travel_time, cropland)
names(travel_time) = "travel_time"
plot(rwa0)
plot(travel_time, add = T, legend = T)
plot(rwa1, add = T)
