#################################
# GEO RANDOM FOREST             #
#################################

Sys.Date()


## Email: svd14@case.edu
##


## set working directory if not using Rproject

## 

options(scipen = 6, digits = 4) # I prefer to view outputs in non-scientific notation

## 

## load up the packages we will need:  (uncomment as required)
# get the cleansed data here with the geom----
# 
library(GWmodel)      ## GW models
library(plyr)         ## Data management
library(sp)           ## Spatial Data management
library(spdep)        ## Spatial autocorrelation
library(RColorBrewer) ## Visualization
library(classInt)     ## Class intervals
library(raster)       ## spatial data
library(grid)         ## plot
library(gridExtra)    ## Multiple plot
library(ggplot2)      #  plotting
library(tidyverse)    # data 
library(SpatialML)    # Geographically weigted regression


# source("functions/packages.R")       # loads up all the packages we need

## ---------------------------

## load up our functions into memory

# source("functions/summarise_data.R") 


##%######################################################%##
#                                                          #
####                    START CODING                    ####
#                                                          #
##%######################################################%##




# get the cleaned data


df = read_rds("data/dm_clean_ready.rds")
#state = read_rds("data/state_shape.rds")

df = df %>% dplyr::rename(
    cvm  = age_adjusted_rate,
    pm = PM25,
    race = R_RacialEthnic_Minorities,
    food = Food_insecurity,
    inc = Med_H_income,
    pri = R_Pri_Care_Phys,
    edu = R_college,
    loc = Rural_R
    
) %>% dplyr::select( -abbr, -state, -county_code)

#state <- cbind(state, st_coordinates(st_centroid(state))) # get the centroid for 
# state labels.

# am going to fit RF with the whole data set.

df = cbind(df, st_coordinates(st_centroid(df)))

df.sp = sf::as_Spatial(df,cast = TRUE)

glimpse(df.sp)

Coords = df.sp[,7:8]

Coords

coordinates = data.frame(X = df.sp$X, Y = df.sp$Y)

data = data.frame(
    cvm = df.sp$cvm,
    pm = df.sp$pm,
    race = df.sp$race,
    pri = df.sp$pri,
    inc = df.sp$inc,
    food = df.sp$food,
    edu = df.sp$edu,
    loc = df.sp$loc,
    region = df.sp$region
)

str(data)

# local model 


formula = cvm ~ pm + race + pri + inc + food + edu + loc + region


grf.model = grf(formula,
                dframe =  data,
                ntree = 500,
                mtry = 2,
                kernel = "adaptive",
                forests = T,
                coords = coordinates,
                bw = 162)


grf.model$Global.Model

write_rds(grf.model, "tables_res/geo_model_randomf.rds")

grf.model = readRDS("tables_res/geo_model_randomf.rds")


res = grf.model$Global.Model$variable.importance/max(grf.model$Global.Model$variable.importance)


str(res)

attributes(res)

attributes(res)$names

td = data.frame(
    var = attributes(res)$names,
    values = res[1:8]
)



td %>% 
    arrange(desc(values)) %>%
    mutate(var = factor(var, levels = var)) %>%
    ggplot(aes(x = var, y = values)) + geom_point(size = 2) + 
    geom_segment(aes(x = 0, xend = values))
    coord_flip() 

