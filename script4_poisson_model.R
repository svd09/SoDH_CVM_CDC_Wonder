## ---------------------------
##
## Script name: Make geo-weighted poisson model for CVM
##
## Purpose of script: To fit the geo-weighted Poisson model for CVM
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
libraries(c("tidyverse","sf","spdep",
                 "splancs","pgirmess","classInt",
                 "raster","broom","rgdal","janitor","ggsn","paletteer",
            "GWmodel","sp","spdep"))



# source("functions/packages.R")       # loads up all the packages we need

## ---------------------------

## load up our functions into memory

# source("functions/summarise_data.R") 

## ---------------------------
## 
## START CODING ######################
## 
## 
## Get the data with the covariates

df = readRDS("data/dm_clean_ready.rds") # got the data 

glimpse(df)

# make the var simpler to read
# 
#

df2 = df %>% rename(
    pm25 = PM25,
    race_minor = R_RacialEthnic_Minorities,
    income = Med_H_income,
    food = Food_insecurity,
    pri_care = R_Pri_Care_Phys,
    rural = Rural_R,
    edu = R_college
)

str(df2)

summary(df2)

df2$age_adjusted_rate_i = round(df2$age_adjusted_rate,0) # for poisson model, need integer as outcomes

df2$age_adjusted_rate_i

# to use the GWmodel, we need to convert this to the sp format.
# 

df2.sp = sf::as_Spatial(df2,cast = TRUE) # to run gwss we need a sp object 
# we can convert an sf object to an sp object using the code above.
# THE SP + GWMODEL ARE OLD PACKAGES THAT NEED THE GEOM OBJECT TO BE AN SP OBJECT - THE gwverse is a newer package 
# that is going to use the sf object to do geo weighted models, but using this as we have the code available for now...

str(df2.sp)


# get the bandwidth 
# 

bandwidth = gw.dist(dp.locat = coordinates(df2.sp))

str(bandwidth)

# 

bandwidth2 = bw.ggwr(age_adjusted_rate_i ~ 
                    pm25 + race_minor + income + food + pri_care + edu + rural,
                family = "poisson",
                approach = "CV",
                kernel = "bisquare",
                adaptive = FALSE,
                dMat = bandwidth,
                data = df2.sp
)


bandwidth2


model_pg = ggwr.basic(age_adjusted_rate_i ~ 
    pm25 + race_minor + income + food + pri_care + edu + rural,
    data = df2.sp,
    kernel = "bisquare",
    adaptive = F,
    bw = bandwidth2,
    dMat = bandwidth
)

model_pg

library(spgwr)

bandwidth_get = spgwr::gwr.sel(age_adjusted_rate ~ 
                  pm25 + race_minor + income + food + pri_care + edu + rural,
              data = df2.sp,
              gweight = gwr.gauss,
              verbose = TRUE)

bandwidth_get

str(bandwidth_get)



bw.gwr = bw.ggwr(age_adjusted_rate_i ~ 
                    pm25 + race_minor + income + food + pri_care + edu + rural,
                family = "poisson",
                approach = "CV",
                kernel = "bisquare",
                adaptive = FALSE,
                dMat = bandwidth,
                data = df2.sp)


# fit the model 

model_pg = ggwr.basic(age_adjusted_rate_i ~ 
    pm25 + race_minor + income + food + pri_care + edu + rural,
    data = df2.sp,
    kernel = "bisquare",
    adaptive = F,
    bw = bwG,
    dMat = bandwidth
)

model_pg

saveRDS(model_pg, "results/model_pg.rds")

m = readRDS("results/model_pg.rds")

summary(m)

m # can get it back, so best to save as rds object.


# create spatial dataframe 


df2.sp@data$y = m$SDF$y

df2.sp@data$yhat = m$SDF$yhat
df2.sp@data$residual = m$SDF$residual


df2.sp

rsd = sd(df2.sp@data$residual)

rsd

df2.sp@data$stdRes = (df2.sp@data$residual)/sd(df2.sp@data$residual)

df2.sp@data$LLN = df2.sp@data$yhat - 1.96*rsd

df2.sp@data$ULN = df2.sp@data$yhat + 1.96*rsd

# get estimates for each county
pm25 + race_minor + income + food + pri_care + edu + rural

df2.sp@data$est_edu = m$SDF$edu
df2.sp@data$est_pm25 = m$SDF$pm25
df2.sp@data$est_income = m$SDF$income
df2.sp@data$est_race = m$SDF$race_minor
df2.sp@data$est_food = m$SDF$food
df2.sp@data$est_pri_care = m$SDF$pri_care
df2.sp@data$est_rural = m$SDF$rural



# convert this to sf object

df.m_sf = st_as_sf(df2.sp)


class(df)

class(df.m_sf)

# plotting estimates
state = read_rds("data/state_shape.rds")
state <- cbind(state, st_coordinates(st_centroid(state))) #
# also get the data for US regions

regions = sf::st_read("us_regions/Deo_regions.shp", stringsAsFactors = F)
regions = cbind(regions, st_coordinates(st_centroid(regions)))

# plots for each covariate

library(cartography)
library(ggrepel)

pmplot = ggplot(df.m_sf) + geom_sf(aes(fill = est_pm25), color = NA) +
    scale_fill_continuous(type = "viridis", direction = -1)



pmplot2 = pmplot +
    geom_sf(data = state, fill = NA, color = "black", lwd = 0.7)+ 
    geom_sf(data = regions, fill = NA, color = "red", lwd = 1.2) + 
    geom_label_repel(data = state, aes(X,Y,label = STUSPS)) + 
  blank()

#-------


foodplot = ggplot(df.m_sf) + geom_sf(aes(fill = est_food), color = NA) +
    scale_fill_continuous(type = "viridis", direction = -1)

foodplot2 = food +
    geom_sf(data = state, fill = NA, color = "black", lwd = 0.7)+ 
    geom_sf(data = regions, fill = NA, color = "red", lwd = 1.2) + 
    geom_label_repel(data = state, aes(X,Y,label = STUSPS)) + 
    blank()


str(foodplot2)

spplot(df2.sp, "est_pm25")


df2.sp@data$irr_race = exp(m$SDF$race_minor)

summary(df2.sp@data$irr_race)


# t-values 
# 

df2.sp@data$t_Intercept = m$SDF$Intercept_TV
df2.sp@data$t_pm25 = m$SDF$pm25_TV
df2.sp@data$t_race = m$SDF$race_minor_TV
df2.sp@data$t_income = m$SDF$income_TV
df2.sp@data$t_food = m$SDF$food_TV
df2.sp@data$t_pri_care = m$SDF$pri_care_TV

# calculate pseudo t-values
# 

# plot estimates here...
# 
# 


# to convert to sf object 
# 

df.m_sf = st_as_sf(df2.sp)

class(df.m_sf)

ggplot(data = df.m_sf) + geom_sf(aes(fill = est_pm25)) +
    scale_fill_paletteer_c(`"ggthemes::Red-Blue-White Diverging"`)


ggplot(data = df.m_sf) + geom_sf(aes(fill = irr_race)) +
    scale_fill_paletteer_c(`"ggthemes::Red-Blue-White Diverging"`) + 
    blank()




spplot(df2.sp, "est_pm25")