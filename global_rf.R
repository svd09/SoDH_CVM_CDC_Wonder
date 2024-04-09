#################################
# RANDOM FOREST             #
#################################

Sys.Date()


## Email: svd14@case.edu
## This script is to make the global random forest model
## using the ranger, caret package


## set working directory if not using Rproject

## 

options(scipen = 6, digits = 4) # I prefer to view outputs in non-scientific notation

## 

#install.packages(c("ggsn","paletteer","ggprism"), dep = T)

#install.packages(c("broom","janitor","pgirmess","classInt","sp","splancs"), dep = T)

## load up the packages we will need:  (uncomment as required)


library(easypackages)

libraries('commentr')
libraries(c("tidyverse","sf","spdep","sf",
            "splancs","pgirmess","classInt",
            "raster","broom","rgdal","janitor","ggsn","paletteer","tigris",
            "ggprism","readxl","cowplot","Hmisc",
            "ranger","tidymodels","caret","lime","h2o",
            "vip","pdp","iml"))



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
state = read_rds("data/state_shape.rds")

state <- cbind(state, st_coordinates(st_centroid(state))) # get the centroid for 
# state labels.


# as this is a global RF we will not need ot use the state 
# data. Best to convert the df to simple df without 
# geometry class.

df.s = df %>% st_drop_geometry()

df.s2 = df.s %>% rename(
    cvm = age_adjusted_rate,
    air = PM25,
    eth = R_RacialEthnic_Minorities,
    inc = Med_H_income,
    food = Food_insecurity,
    pri = R_Pri_Care_Phys
) %>% dplyr::select(
    cvm, air, eth, inc, food, pri, county_code, region, abbr
)

glimpse(df.s2) # clean dataset with only selected var


# variables scaled, now we can fit the random forest model

# so now this is a tibble without the geometry class.
# am going to now split to test and train data.

# seed

set.seed(1974)

# fit model for the whole data using ranger
# fit model using ranger 

# creating random forest model with caret.

cvm ~ air + eth + inc + food + pri + region

fit.ranger <- ranger::ranger(
    formula = cvm ~ air + eth + inc + food + pri + region,
    data = df.s2, 
    importance = 'impurity',
    probability = TRUE,num.trees = 50,write.forest = T)

fit.ranger

imp = fit.ranger$variable.importance/max(fit.ranger$variable.importance)


vip(fit.ranger)

d = data.frame(imp = fit.ranger$variable.importance/max(fit.ranger$variable.importance))

d$colnames = c("PM2.5","Racial Minorities","Income","Food Insecurity","Primary care access","US Region")

d %>% ggplot(aes(x = reorder(colnames,imp), y = imp)) + 
    geom_point(stat = "identity", size = 3) + 
    geom_segment(aes(x = colnames, xend = colnames, y = 0, yend = imp)) +
    coord_flip()


fit.ranger %>% partial(pred.var = "air",grid.resolution = 50) %>%
    autoplot(rug = T, train = df.s2)



a = fit.ranger %>% partial(pred.var = c("pri","inc"),grid.resolution = 50) %>%
    autoplot(rug = T, train = df.s2)

