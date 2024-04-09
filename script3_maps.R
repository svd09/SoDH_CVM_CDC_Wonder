## ---------------------------
##
## Script name: Environmental and social factors associated with DM attributable CVM in the US
##
## Purpose of script: To get the data cleaned, do some summary statistics
## and understand the data. Also make some maps to present the variation
## in the data. 
## The next script will be used to do the analysis.
##
## Author: Dr. Salil Deo
##
## Date Created: 
Sys.Date()


## Email: svd14@case.edu
##
## ---------------------------
## 
## 
##   
##
## ---------------------------

## set working directory if not using Rproject

## ---------------------------

options(scipen = 6, digits = 4) # I prefer to view outputs in non-scientific notation

## ---------------------------

## load up the packages we will need:  (uncomment as required)


library(easypackages)

libraries('commentr')
libraries(c("tidyverse","sf","spdep","sf",
            "splancs","pgirmess","classInt",
            "raster","broom","rgdal","janitor","ggsn","paletteer"))

library(tigris)

# source("functions/packages.R")       # loads up all the packages we need

## ---------------------------

## load up our functions into memory

# source("functions/summarise_data.R") 

## ---------------------------
## 
## START CODING ######################
## 
## 

# get the cleanded data here with the geom 
# 


df = read_rds("data/comb_rds.rds")

class(df)




# now to make a simple mapp 
# 
# 

ggplot(df) + geom_sf() # so we have removed all the counties that we are not using 

# we have removed Alaska, American Samoa, Hawaii and other islands that are part of the Census shapefiles
# this map looks good. Now to simply plot the rate as a continous value on the map.

ggplot(df) + geom_sf(aes(fill = age_adjusted_rate), color = NA)

# make plot for the paper 
# 

map_cvm = ggplot(df) + geom_sf(aes(fill = age_adjusted_rate), color = NA) + blank() + 
    ggsn::scalebar(data = df,location = "bottomleft",dist_unit = "mi",
                   dist = 500,
                   transform = T) + 
    ggsn::north(data = df,scale = 0.2,
                location = "bottomright")

map_cvm2 = map_cvm + scale_fill_paletteer_c(`"ggthemes::Gray"`)

map_cvm2

# for this and further maps would be good to provide census regions
# 

state = tigris::states(year = 2015,resolution = "500k")

class(state)

ggplot() + geom_sf(data = state)

# limit to 48 states
# 

library(readxl)
list = read_excel("data/states.xlsx")

state2 = state %>% dplyr::filter(NAME %in% list$state)

class(state2)

ggplot() + 
    geom_sf(data = df, aes(fill = age_adjusted_rate), color = NA) + scale_fill_paletteer_c(`"ggthemes::Gray"`) + 
    geom_sf(data = state2, fill = NA, color = "red", lwd = 1) + blank() 

at = ggplot(df) + geom_sf(aes(fill = age_adjusted_rate), color = NA) + scale_fill_paletteer_c(`"ggthemes::Blue-Teal"`)
    geom_sf(fill = "transparent", size = 1, data = state2, color = "darkblue") +
    coord_sf(ndiscr = FALSE)

at








reg = sf::st_read("data\\cb_2022_us_region_5m.shp",stringsAsFactors = FALSE)


class(reg)

glimpse(reg)


shape = sf::st_read("data/cb_2022_us_county_5m.shp", stringsAsFactors = F)

glimpse(shape)

# shape has state name 
# 

library(readxl)

states = read_excel("data/states.xlsx")

dim(states)

try = shape %>% dplyr::filter(STATE_NAME %in% states$state)

dim(try)

#### 
# we have 2 shapefile data 
# 
library(tmap)

# need to get the state border shapefile and then repeat this...


at = ggplot(df) + geom_sf(aes(fill = age_adjusted_rate)) + 
    geom_sf(fill = "transparent", size = 1, data = try, color = "darkblue") +
    coord_sf(ndiscr = FALSE)

at
