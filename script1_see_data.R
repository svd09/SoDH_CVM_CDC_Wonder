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
                 "raster","broom","rgdal","janitor","ggsn","paletteer",
            "readxl"))



# source("functions/packages.R")       # loads up all the packages we need

## ---------------------------

## load up our functions into memory

# source("functions/summarise_data.R") 

## ---------------------------
## 
## START CODING ######################
## 
## 

# get the cleaned data ...
# 

df = read_csv("data/diabetes_cvd_cleaned_new.csv")

# see the data 
# 

glimpse(df)

# we have the the county_code and age adjusted rate

# we should join with the spatial data to create a spatial dataset
# we can then plot the data to see the spatial variation in CVD 

# ge the shapefile from the maps library
# now we have the shapefile object 
# the maps library contains the shapefile for continental USA
# 

shape = sf::st_read("data/cb_2022_us_county_5m.shp", stringsAsFactors = F)


ggplot(shape) + geom_sf() # map does not look good because of American Samoa and partly Alaska
# we are going to only use continental US for our study.
# 

class(shape)

glimpse(shape)

# the GEOID is the fips code, so use that to join this to our data 
# we only need the geomtry and GEOID from this dataset, so only keep that 
# 

shape2 = shape %>% dplyr::select(GEOID) %>% rename(county_code = GEOID)

class(shape2)

glimpse(shape2) # the geometry part of the df always stays , so we do not need to select that 

# now to join this with the df 
# 

glimpse(df)

df$county_code = as.numeric(df$county_code)
shape2$county_code = as.numeric(shape2$county_code)

df2 = left_join(shape2, df, by = "county_code")

class(df2) # now this is our sf object with all the data that we need.

# remove those with missing CVD 
# 

summary(df2$age_adjusted_rate)

df3 = df2 %>% drop_na(age_adjusted_rate)

# save this data set so that we can call it directly later 
# 
# this contains the county code, join it to the state and then region.

st = read_excel("data/state_fips.xlsx")

glimpse(st)

st$code = with(st,
               ifelse(
                   nchar(code) == 1, paste0(0,code), code
               ))

st$code

# now to get the state code from the main data 

df3$county_code2 = with(df3, 
                        ifelse(
                            nchar(county_code) == 4, paste0(0,county_code), county_code))

nchar(df3$county_code2)

df3$code = substr(df3$county_code2,start = 1,stop = 2)

df3$code

# now to join the states to this data

df4 = left_join(df3, st, by = "code")

df4$state

df4$abbr

# now to get the regions here...

r = read_excel("data/regions.xlsx")

glimpse(r)

r = r %>% arrange(State)

head(r,20)

r2 = r %>% dplyr::select(Region,State) %>% rename(state = State, region = Region)

r

r2

# now to join to main data 

df5 = left_join(df4, r2, by = "state")

glimpse(df5$region)

tabyl(df5$region)

# now looks fine here. no NA and all 4 regions present.

write_rds(df5, "data/comb_rds.rds") # writing as an rds object maintains the geometry of the data
# so that we can open it and preserve it as an sf object.

