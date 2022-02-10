# Installing packages
install.packages("readr")
install.packages("ggplot2")
install.packages("sf")

# Loading packages
# library(readr)
# library(ggplot2)
# library(sf)

# Nice plot theme
theme_set(theme_light())

# Setting to home directory
setwd("~")


## Reading in data

#############################
## NOTE -- file path needs to be set to the directory where the "Mapping in R" folder is located / In my case, this is Downloads
#############################
file_path <- "~/Downloads/"

# Census data
cleaned_census_data <- sf::st_read(paste0(file_path, "Mapping in R/cleaned_census_data.shp"))

# Durham school data
durham_schools_tbl <- read_csv(paste0(file_path, "Mapping in R/durham_school_data.csv"))




###################
## Creating maps ##
###################

# simple ggplot map
cleaned_census_data %>%
  ggplot() + geom_sf()

# Plotting schools as points on map
cleaned_census_data %>%
  ggplot() + geom_sf() +
  geom_point(durham_schools_tbl, mapping = aes(long, lat))

# Woah?! What happend?

# Need to make CRS match
# Converting coordinate system datum from NAD83 to wgs84
cleaned_census_data <- st_transform(cleaned_census_data, crs = "wgs84")

# Map with durham schools (custom axis ticks and labels)
p1 <- cleaned_census_data %>%
  ggplot() + geom_sf() +
  geom_point(durham_schools_tbl, mapping = aes(long, lat), size=.2) +
  scale_x_continuous(breaks = c(-79, -78.7)) +
  scale_y_continuous(breaks = seq(35.9, 36.2, .1)) +
  labs(x = "Longitude", y = "Latitude", title = "Map of Durham Schools")
p1

# Dropping missing data for capacity19_20 variable
durham_schools_tbl <- durham_schools_tbl[durham_schools_tbl$capacity19_20 != -1,]

# New plot where point size is scaled by school capacity
p2 <- cleaned_census_data %>%
  ggplot() + geom_sf() +
  geom_point(durham_schools_tbl, mapping = aes(long, lat, size = numstudent)) +
  scale_x_continuous(breaks = c(-79, -78.7)) +
  scale_y_continuous(breaks = seq(35.9, 36.2, .1)) +
  scale_size(range = c(-1.5, 4)) +
  theme(legend.position = "none") +
  labs(x = "Longitude", y = "Latitude", title = "Map of Durham Schools")
p2

# Color by whether school is overcapacity
durham_schools_tbl$over <- "At or Below Capacity"
durham_schools_tbl$over[durham_schools_tbl$numstudent > durham_schools_tbl$capacity19_20] <- "Over Capacity"
durham_schools_tbl <- durham_schools_tbl[!is.na(durham_schools_tbl$over),]
durham_schools_tbl$over <- factor(durham_schools_tbl$over)
durham_schools_tbl$over <- relevel(durham_schools_tbl$over, 2)

 # mutate(over = fct_rev(over))

p3 <- cleaned_census_data %>%
  ggplot() + geom_sf() +
  geom_point(durham_schools_tbl, mapping = aes(long, lat, size = numstudent, color = over)) +
  scale_x_continuous(breaks = c(-79, -78.7)) +
  scale_y_continuous(breaks = seq(35.9, 36.2, .1)) +
  scale_size(range = c(-1.5, 4), guide = 'none') +
  labs(x = "Longitude", y = "Latitude", title = "Map of Durham Schools")

# Dropping tracts that do not have median income
cleaned_census_data <- cleaned_census_data[!is.na(cleaned_census_data$med_inc),]
cleaned_census_data$med_inc_std <- (cleaned_census_data$med_inc - mean(cleaned_census_data$med_inc))/
  sd(cleaned_census_data$med_inc)

# Shade census tracts by income level
p4 <- ggplot(cleaned_census_data) + geom_sf(aes(fill = med_inc_std), alpha = .5) +
  scale_fill_gradientn(colors = c('red', '#0c88f5', 'blue')) +
  geom_point(durham_schools_tbl, mapping = aes(long, lat, size = numstudent, color = over), alpha = .8) +
  scale_color_manual(values = c("red", "blue")) +
  scale_x_continuous(breaks = c(-79, -78.7)) +
  scale_y_continuous(breaks = seq(35.9, 36.2, .1)) +
  scale_size(range = c(-1.5, 4), guide = 'none') +
  labs(x = "Longitude", y = "Latitude",
       title = "Map of Durham Schools",
       fill = "Std. Median Income",
       color = "School Status") +
  theme(plot.title = element_text(hjust = .5))
p4


#######################################################
################# ALTERING THIS PLOT ##################
#######################################################

cleaned_census_data_mod <- cleaned_census_data %>% mutate(prop_male = male/pop, 
                                                          prop_white = white/pop,
                                                          prop_black = black/pop, 
                                                          prop_asian = asian/pop,
                                                          prop_hisp = hisp/pop,
                                                          prop_ba = ba/(ba+no_ba))

## To change the map shading, replace ### in the following line with 
## prop_male, prop_white, prop_black, prop_asian, prop_hisp, prop_ba
## Try to adjust the legend accordingly 
ggplot(cleaned_census_data_mod) + geom_sf(aes(fill = prop_white), alpha = .5) +
  scale_fill_gradientn(colors = c('red', '#0c88f5', 'blue')) +
  geom_point(durham_schools_tbl, mapping = aes(long, lat, size = numstudent, color = over), alpha = .8) +
  scale_color_manual(values = c("red", "blue")) +
  scale_x_continuous(breaks = c(-79, -78.7)) +
  scale_y_continuous(breaks = seq(35.9, 36.2, .1)) +
  scale_size(range = c(-1.5, 4), guide = 'none') +
  labs(x = "Longitude", y = "Latitude",
       title = "Map of Durham Schools",
       fill = "Std. Median Income",
       color = "School Status") +
  theme(plot.title = element_text(hjust = .5))











