#################################
# EDA of the data               #
#################################

Sys.Date()


## Email: svd14@case.edu
##


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
            "ggprism","readxl","cowplot","Hmisc","ggrepel"))



# source("functions/packages.R")       # loads up all the packages we need

## ---------------------------

## load up our functions into memory

# source("functions/summarise_data.R") 


##%######################################################%##
#                                                          #
####                    START CODING                    ####
#                                                          #
##%######################################################%##
 

# get the cleansed data here with the geom----
# 


df = read_rds("data/comb_rds.rds")

class(df)

# need to get the regions here
# now to make a simple mapp 
# 
# 

ggplot(df) + geom_sf() # so we have removed all the counties that we are not using 

# we have removed Alaska, American Samoa, Hawaii and other islands that are part of the Census shapefiles
# this map looks good. Now to simply plot the rate as a continuous value on the map.

ggplot(df) + geom_sf(aes(fill = age_adjusted_rate), color = NA)


# now to see the data 


length(unique(df$county_code))

summary(df$age_adjusted_rate)

# see data for each US region 

tabyl(df$region)

tapply(df$age_adjusted_rate,df$region,summary)

# group wise comparison using bonferroni correction.

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

map_cvm3 = map_cvm + scale_fill_paletteer_c(`"ggthemes::Red-Blue Diverging"`,
                                            direction = -1)

# for the map would be better to also have state boundaries, so that we can report states that appear to have outlier and high county rates
# use the tigris library to get shapefile for state boundaries.

state = tigris::states(year = 2015,resolution = "500k")

class(state)

ggplot() + geom_sf(data = state) # good plot of only state boundaries
# but also contains Alaska, other regions + Hawaii, we will remove those and only keep 48 continental US states.

# limit to 48 states
# 

library(readxl)

list = read_excel("data/states.xlsx")

state2 = state %>% dplyr::filter(NAME %in% list$state)

class(state2) # contains only continental US states now.

saveRDS(state2, "data/state_shape.rds")

# get the data and then make plot for CVM 


df = read_rds("data/comb_rds.rds")

state = read_rds("data/state_shape.rds")

state <- cbind(state, st_coordinates(st_centroid(state))) #
# also get the data for US regions

regions = sf::st_read("us_regions/Deo_regions.shp", stringsAsFactors = F)
regions = cbind(regions, st_coordinates(st_centroid(regions)))

class(regions)

glimpse(regions)

# making figure 1 for the paper 
# county-level CVM in the US 

cvm = ggplot(df) + geom_sf(aes(fill = age_adjusted_rate), color = NA) + blank() + 
    scale_fill_paletteer_c(`"ggthemes::Blue"`) 

cvm2 = cvm +
    geom_sf(data = state, fill = NA, color = "black", lwd = 0.7) + 
    geom_sf(data = regions, fill = NA, color = "red", lwd = 1.2) + 
    geom_label_repel(data = state, aes(X,Y,label = STUSPS)) +
    geom_label_repel(data = regions, aes(X,Y,label = NAME), fill = "cornsilk")
ggsn::scalebar(data = df,location = "bottomleft",dist_unit = "mi",
                   dist = 500,
                   transform = T) + 
    ggsn::north(data = df,scale = 0.2,
                location = "bottomright") 

### - save this plot as figure for the paper
# plot of CVM at county level 
# can label regions later if needed using ppt.

ggsave(plot = cvm2,
       filename = "paper_figures/cvm.tiff", 
       height = 10, width = 16,
       unit = "in",dpi = 600)

ggsave(plot = cvm2,
       filename = "paper_figures/cvm.pdf", 
       height = 10, width = 16,
       unit = "in",dpi = 600)

# now to get the data back to df
# 

df 

summary(df$age_adjusted_rate) 

# > summary(df3$age_adjusted_rate)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 11.6    73.6    92.9    98.4   118.5   377.2 
# 


df$neighborhood = poly2nb(df, queen = T)

# par(mar = c(0,0,0,0))
# plot(df, border = "gray")
# plot(neighborhood,
#      coords = coordinates(df),
#      col = "red",
#      add = T)

# get the neighborhood weight list 
# 

neighborhood_weights_list = nb2listw(df$neighborhood, style = "W", zero.policy = TRUE)

neighborhood_weights_list$weights

moran.mc(df$age_adjusted_rate, neighborhood_weights_list, 
         nsim = 10000,zero.policy = TRUE)

# Monte-Carlo simulation of Moran I
# 
# data:  df3$age_adjusted_rate 
# weights: neighborhood_weights_list  
# number of simulations + 1: 10001 
# 
# statistic = 0.46, observed rank = 10001, p-value = 0.0001
# alternative hypothesis: greater
# 

# now to see which areas are outliers using the local Moran test 
# 

local_moran = localmoran(df$age_adjusted_rate,
                         neighborhood_weights_list,zero.policy = TRUE,
                         alternative = "greater")

local_moran

summary(local_moran)

local_moran


df$lmoran_i = local_moran[,1]
df$lmoran_p = local_moran[,5]
df$l_moran_sig_p = local_moran[,5]< 0.05
df$l_moran_sig_p2 = local_moran[,5]< 0.01

df$l_moran_sig_p = factor(df$l_moran_sig_p)
df$l_moran_sig_p2 = factor(df$l_moran_sig_p2)

class(df)

library(janitor)

tabyl(df$l_moran_sig_p)

a = ggplot(df) + geom_sf(aes(fill = l_moran_sig_p), color = NA) + 
    scale_fill_manual(values = c("gray80","darkblue")) + 
    geom_sf(data = state2, fill = NA, color = "red", lwd = 0.3) + blank() 


b = ggplot(df) + geom_sf(aes(fill = l_moran_sig_p2), color = NA) + 
    scale_fill_manual(values = c("gray80","blue")) +  
    geom_sf(data = state2, fill = NA, color = "red", lwd = 0.3) + blank() 


# plots a & b look good for the paper 
# along with the map to show cvm

library(cowplot)

d = plot_grid(a,b,nrow = 2)

d # plot to report those counties that are outliers according to Moran I

ggsave(plot = d,filename = "figures/cvm_moran.pdf", device = "pdf")


# Do getis Gi* test for the data 
# 

wr = poly2nb(df, row.names = df$county_code, queen = FALSE)

lstw = nb2listw(wr,style = "B",zero.policy = TRUE)

Gi = localG(df$age_adjusted_rate, lstw,zero.policy = TRUE)

head(Gi)

Gcuts = cut(Gi,5)

Gcutsi = as.integer(Gcuts)

cols = rev(gray(seq(0,1,0.2)))

plot(df, col = cols[Gcutsi])

library(rgeoda)

queen_w = queen_weights(df)

gd = local_gstar(queen_w, df["age_adjusted_rate"])

gdl = lisa_pvalues(gd)

gdl

gdl2 = tibble(p_val_g = gdl)

str(gdl2)

df$p_val_g = gdl2$p_val_g

ggplot(data = df) + geom_sf(aes(fill = p_val_g)) + blank()



#  WE ARE GOING TO USE THE FOLL COVARIATES
#  AIR POLLUTION - PM2.5 
#  RACE ETHNICITY - PERCENTAGE OF MINORITIES IN COUNTY
#  LOCATION - RURAL VS URBAN
#  ACCESS TO HEALTHCARE - PERCENTAGE INSURED // PRIMARY HEALTHCARE COVERAGE
#  POVERTY - MEDIAN HOUSEHOLD INCOME
#  FOOD INSECURITY - FOOD INSECURITY INDEX
#  Education
#  SEE WHAT COLUMNS WE NEED AND LIMIT TO ONLY THOSE COLUMNS WE NEED

df2 = df %>% dplyr::select(county_code, age_adjusted_rate, PM25, R_RacialEthnic_Minorities,
                           Med_H_income,Food_insecurity,R_Pri_Care_Phys, region,
                           abbr,state,Rural_R,R_college)


# need to also add the SDI & rural/urban information here...


colnames(df2)

class(df2)



# we can now save this... save as rds so that we save the geometry too.
# 

write_rds(df2, "data/dm_clean_ready.rds")

# am going to import this now as as a df... so that the code does not change.
# 

df = read_rds("data/dm_clean_ready.rds")
state = read_rds("data/state_shape.rds")

state <- cbind(state, st_coordinates(st_centroid(state))) # get the centroid for 
# state labels.

regions = sf::st_read("us_regions/Deo_regions.shp", stringsAsFactors = F)

class(regions)

glimpse(regions)

regions = cbind(regions, st_coordinates(st_centroid(regions)))


# see this data now
# 
# 
# 

class(df) # sf object 

glimpse(df)

# map the distribution of each covariates.
# 6 exposures that we are going to look at.

summary(df$PM25)

tapply(df$PM25, df$region, summary)

# identify counties in the highest tertile 

library(Hmisc)

df$pm_tertile = cut2(df$PM25,g = 3)

tabyl(df$pm_tertile)
    

table(df$region, df$pm_tertile)
prop.table(table(df$region, df$pm_tertile), 1)*100

#               [4.13, 7.59) [7.59, 8.55) [8.55,14.98]
# Midwest       42.31678     36.05201     21.63121
# Northeast     62.24490     17.85714     19.89796
# South         16.10032     38.83495     45.06472
# West          59.16399     13.82637     27.00965


library(cartography)

my_pal_blue = carto.pal(pal1 = "blue.pal",n1 = 3)


high_pm = ggplot(df) + geom_sf(aes(fill = pm_tertile), color = NA) + blank() +
    scale_fill_manual(values = my_pal_blue)
    
high_pm2 = high_pm +     
    geom_sf(data = state, fill = NA, color = "black", lwd = 0.7) + 
    geom_sf(data = regions, fill = NA, color = "red", lwd = 1) + 
    geom_label_repel(data = state, aes(X,Y,label = STUSPS)) +
ggsn::scalebar(data = df,location = "bottomleft",dist_unit = "mi",
               dist = 500,
               transform = T) + 
    ggsn::north(data = df,scale = 0.2,
                location = "bottomright") 
    
    
# save this map for PM2.5 

ggsave(plot = high_pm2,
       filename = "paper_figures/pm.tiff",
       height = 5, width = 8, units = "in",dpi = 600,device = "tiff")
    
# now to proceed using the same palette for all the other figures...
# food insecurity - higher is bad.

summary(df$Food_insecurity)

tapply(df$Food_insecurity, df$region, summary)

# tertile for food insecurity

df$foodt = cut2(df$Food_insecurity, g = 3)


table(df$region, df$foodt)
prop.table(table(df$region, df$foodt),1)

            # [ 3.4,11.7) [11.7,14.5) [14.5,36.3]
# Midwest    0.53427896  0.32151300  0.14420804
# Northeast  0.58673469  0.38265306  0.03061224
# South      0.16181230  0.30582524  0.53236246
# West       0.38263666  0.40514469  0.21221865

food = ggplot(df) + geom_sf(aes(fill = foodt), color = NA) + blank() +
    scale_fill_manual(values = my_pal_blue)

food2 = food +     
    geom_sf(data = state, fill = NA, color = "black", lwd = 0.7) + 
    geom_sf(data = regions, fill = NA, color = "red", lwd = 1) + 
    geom_label_repel(data = state, aes(X,Y,label = STUSPS)) 



ggsave(plot = food2,
       filename = "paper_figures/food.tiff",
       height = 5, width = 8, units = "in",dpi = 600,device = "tiff")

# racial minorities 

summary(df$R_RacialEthnic_Minorities)


df$eth = cut2(df$R_RacialEthnic_Minorities, g = 3)


table(df$region, df$eth)
prop.table(table(df$region, df$eth),1)

#               [ 2.13,10.5) [10.50,28.3) [28.29,97.2]
# Midwest        0.58392      0.34515      0.07092
# Northeast      0.56122      0.30612      0.13265
# South          0.17799      0.30178      0.52023
# West           0.12540      0.44373      0.43087




race = ggplot(df) + geom_sf(aes(fill = eth), color = NA) + blank() +
    scale_fill_manual(values = my_pal_blue)

race2 = race +     
    geom_sf(data = state, fill = NA, color = "black", lwd = 0.7) + 
    geom_sf(data = regions, fill = NA, color = "red", lwd = 1) + 
    geom_label_repel(data = state, aes(X,Y,label = STUSPS)) 



ggsave(plot = race2,
       filename = "paper_figures/race.tiff",
       height = 5, width = 8, units = "in",dpi = 600,device = "tiff")



# poverty - median household income 
# low median income 

summary(df$Med_H_income)

tapply(df$Med_H_income,df$region, summary)

df$income = cut2(df$Med_H_income,g=3)

prop.table(table(df$region,df$income),1)*100

                # [24783, 44798) [44798, 53985) [53985,136191]
# Midwest        16.784870      40.070922      43.144208
# Northeast       5.102041      37.755102      57.142857
# South          52.103560      27.669903      20.226537
# West           21.543408      34.726688      43.729904

inc = ggplot(df) + geom_sf(aes(fill = income), color = NA) + blank() +
    scale_fill_manual(values = my_pal_blue)

inc2 = inc +     
    geom_sf(data = state, fill = NA, color = "black", lwd = 0.7) + 
    geom_sf(data = regions, fill = NA, color = "red", lwd = 1) + 
    geom_label_repel(data = state, aes(X,Y,label = STUSPS)) 



ggsave(plot = inc2,
       filename = "paper_figures/income.tiff",
       height = 5, width = 8, units = "in",dpi = 600,device = "tiff")


library(patchwork)
race2/inc2 # looks good 

# primary care coverage - higher is better 

summary(df$R_Pri_Care_Phys)

df$pri = cut2(df$R_Pri_Care_Phys, g = 3)

prop.table(table(df$region,df$pri),1)*100

# [ 2.14, 38.3) [38.33, 62.3) [62.30,514.5]
# Midwest        30.73286      33.80615      35.46099
# Northeast      12.24490      30.61224      57.14286
# South          42.15210      34.54693      23.30097
# West           18.64952      28.93891      52.41158


pri = ggplot(df) + geom_sf(aes(fill = pri), color = NA) + blank() +
    scale_fill_manual(values = my_pal_blue)

pri2 = pri +     
    geom_sf(data = state, fill = NA, color = "black", lwd = 0.7) + 
    geom_sf(data = regions, fill = NA, color = "red", lwd = 1) + 
    geom_label_repel(data = state, aes(X,Y,label = STUSPS)) 

ggsave(plot = pri2,
       filename = "paper_figures/primary_care.tiff",
       height = 5, width = 8, units = "in",dpi = 600,device = "tiff")


# rurality of counties

summary(df$Rural_R)

tapply(df$Rural_R, df$region, summary)

df$rural_g = cut2(df$Rural_R,g=3)

prop.table(table(df$region,df$rural_g),1)*100


#               [ 0.01, 39.0) [38.98, 67.4) [67.38,100.0]
# Midwest           30.14         37.23         32.62
# Northeast         41.84         32.65         25.51
# South             28.80         31.47         39.72
# West              54.66         30.55         14.79



rural = ggplot(df) + geom_sf(aes(fill = rural_g), color = NA) + blank() +
    scale_fill_manual(values = my_pal_blue)

rural2 = rural +     
    geom_sf(data = state, fill = NA, color = "black", lwd = 0.7) + 
    geom_sf(data = regions, fill = NA, color = "red", lwd = 1) + 
    geom_label_repel(data = state, aes(X,Y,label = STUSPS)) 

ggsave(plot = rural2,
       filename = "paper_figures/rurality.tiff",
       height = 5, width = 8, units = "in",dpi = 600,device = "tiff")


# college education


summary(df$R_college)

df$educ = cut2(df$R_college,g=3)

tapply(df$R_college, df$region, summary)

prop.table(table(df$region,df$educ),1)*100

#               [19.2,53.0) [53.0,63.2) [63.2,90.7]
# Midwest         16.90       32.98       50.12
# Northeast       15.31       37.76       46.94
# South           50.08       32.36       17.56
# West            22.83       36.01       41.16

educ = ggplot(df) + geom_sf(aes(fill = educ), color = NA) + blank() +
    scale_fill_manual(values = my_pal_blue)

educ2 = educ +     
    geom_sf(data = state, fill = NA, color = "black", lwd = 0.7) + 
    geom_sf(data = regions, fill = NA, color = "red", lwd = 1) + 
    geom_label_repel(data = state, aes(X,Y,label = STUSPS)) 

ggsave(plot = educ2,
       filename = "paper_figures/education.tiff",
       height = 5, width = 8, units = "in",dpi = 600,device = "tiff")


############################
#  DONE FOR THE EXPOSURES  #
############################
#  NOW WE ARE GOING TO LOOK AT THE BIVARIATE ASSOCIAITON BETWEEN EACH COVARITE AND OUR OUTCOME - AGE ADJUSTED MORTALITY
# THIS NEEDS TO CONVERT THE DATA TO AN OLDER FORMAT, THE SP FORMAT (SPATIAL).


library(GWmodel)

df.sp = sf::as_Spatial(df,cast = TRUE) # to run gwss we need a sp object 
# we can convert an sf object to an sp object using the code above.
# THE SP + GWMODEL ARE OLD PACKAGES THAT NEED THE GEOM OBJECT TO BE AN SP OBJECT - THE gwverse is a newer package 
# that is going to use the sf object to do geo weighted models, but using this as we have the code available for now...


# LOOK AT THE BIVARIATE RELATIONSHIP FOR EACH AND THE OUTCOME 
# PLOT DATA TO SEE WHICH FACTORS ARE MORE IMPORTANT IN WHICH AREA OF THE US.
# 

gwss.pm25 <- GWmodel::gwss(df.sp,vars = c("age_adjusted_rate", "PM25"),
                           kernel="bisquare", adaptive=TRUE, bw=48)


str(gwss.pm25)

gwss.pm25 # this is the summary statistics for PM2.5

# ***********************************************************************
#     *                       Package   GWmodel                             *
#     ***********************************************************************
#     
#     ***********************Calibration information*************************
#     
#     Local summary statistics calculated for variables:
#     age_adjusted_rate PM25
# Number of summary points: 2589
# Kernel function: bisquare 
# Summary points: the same locations as observations are used.
# Adaptive bandwidth: 48 (number of nearest neighbours)
# Distance metric: Euclidean distance metric is used.
# 
# ************************Local Summary Statistics:**********************
#     Summary information for Local means:
#     Min. 1st Qu. Median 3rd Qu.  Max.
# age_adjusted_rate_LM  55.55   85.41  95.50  110.40 179.7
# PM25_LM                5.07    7.36   8.19    8.73  10.9
# Summary information for local standard deviation :
#     Min. 1st Qu. Median 3rd Qu.  Max.
# age_adjusted_rate_LSD 12.931  22.458 27.564  33.937 71.69
# PM25_LSD               0.106   0.301  0.405   0.579  3.12
# Summary information for local variance :
#     Min.   1st Qu.    Median   3rd Qu.    Max.
# age_adjusted_rate_LVar  167.2161  504.3757  759.7999 1151.7065 5139.99
# PM25_LVar                 0.0113    0.0907    0.1641    0.3349    9.71
# Summary information for Local skewness:
#     Min. 1st Qu. Median 3rd Qu. Max.
# age_adjusted_rate_LSKe -1.256   0.216  0.532   0.859 2.81
# PM25_LSKe              -2.411  -0.179  0.284   0.711 3.19
# Summary information for localized coefficient of variation:
#     Min. 1st Qu. Median 3rd Qu. Max.
# age_adjusted_rate_LCV 0.1352  0.2425 0.2891  0.3431 0.55
# PM25_LCV              0.0119  0.0362 0.0510  0.0820 0.38
# Summary information for localized Covariance and Correlation between these variables:
#     Min.  1st Qu.   Median  3rd Qu.  Max.
# Cov_age_adjusted_rate.PM25          -21.0874  -2.8368  -0.6231   1.8098 93.22
# Corr_age_adjusted_rate.PM25          -0.7260  -0.2770  -0.0588   0.1503  0.82
# Spearman_rho_age_adjusted_rate.PM25  -0.7172  -0.2770  -0.0719   0.1299  0.79
# 
# ************************************************************************
# 
# 
# make a plot to see the correlation between PM2.5 and CVM
# 

gwss.pm25$SDF$Spearman_rho_age_adjusted_rate.PM25

# we can get this correlation into our main dataset df3 to be able to plot it.
# 

df$pm2.5_corr = gwss.pm25$SDF$Spearman_rho_age_adjusted_rate.PM25

pm = ggplot(df) + geom_sf(aes(fill = pm2.5_corr), color = NA) + 
    scale_fill_paletteer_c(`"grDevices::RdBu"`, direction = -1)  + 
    blank() + ggtitle("PM2.5")

pm 

pm2 = pm +  
    geom_sf(data = state, fill = NA, color = "black", lwd = 0.7) + blank() +
    geom_label(data = state, aes(X,Y,label = STUSPS))

pm2



# FOOD INSECURITY

gwss.fsi <- GWmodel::gwss(df.sp,vars = c("age_adjusted_rate", "Food_insecurity"),
                          kernel="bisquare", adaptive=TRUE, bw=48)


gwss.fsi # this is the summary statistics for food insecurity

# we can get this correlation into our main dataset df3 to be able to plot it.
# 

df$fsi_corr = gwss.fsi$SDF$Spearman_rho_age_adjusted_rate.Food_insecurity


fsi = ggplot(df) + geom_sf(aes(fill = fsi_corr), color = NA) + 
    scale_fill_paletteer_c(`"grDevices::RdBu"`, direction = -1) + 
    blank() + ggtitle("Food Insecurity Index")

fsi2 = fsi + 
    geom_sf(data = state, fill = NA, color = "black", lwd = 0.7) + blank() +
    geom_label(data = state, aes(X,Y,label = STUSPS))



pcorr1 = cowplot::plot_grid(pm2,fsi2, nrow = 1) # plotting together shows
# how the PM2.5 and food insecurity are differently correlated with DM/CVM 
# in different regions of the US 

pcorr1

ggsave(pcorr1, filename = "figures/corr1.tiff", height = 10, width = 18, units = "in",
       dpi = 600,device = "tiff")


# median income


gwss.inc <- GWmodel::gwss(df.sp,vars = c("age_adjusted_rate", "Med_H_income"),
                          kernel="bisquare", adaptive=TRUE, bw=48)


gwss.inc # this is the summary statistics for food insecurity

# we can get this correlation into our main dataset df3 to be able to plot it.
# 

df$inc_corr = gwss.inc$SDF$Spearman_rho_age_adjusted_rate.Med_H_income


income = ggplot(df) + geom_sf(aes(fill = inc_corr), color = NA) + 
    scale_fill_paletteer_c(`"grDevices::RdBu"`, direction = -1) + 
    blank() + ggtitle("Median Household Income")

income2 = income + 
    geom_sf(data = state, fill = NA, color = "black", lwd = 0.7) + blank() +
    geom_label(data = state, aes(X,Y,label = STUSPS))

income2

# primary care coverage


gwss.pri <- GWmodel::gwss(df.sp,vars = c("age_adjusted_rate", "R_Pri_Care_Phys"),
                          kernel="bisquare", adaptive=TRUE, bw=48)


gwss.pri # this is the summary statistics for food insecurity

# we can get this correlation into our main dataset df3 to be able to plot it.
# 

df$pri_corr = gwss.pri$SDF$Spearman_rho_age_adjusted_rate.R_Pri_Care_Phys


primary = ggplot(df) + geom_sf(aes(fill = pri_corr), color = NA) + 
    scale_fill_paletteer_c(`"grDevices::RdBu"`, direction = -1) + 
    blank() + ggtitle("Primary Care Coverage")

primary2 = primary + 
    geom_sf(data = state, fill = NA, color = "black", lwd = 0.7) + blank() 

primary2


primary3 = primary2 + 
    geom_sf(data = state, fill = NA, color = "black", lwd = 0.7) + blank() + 
    geom_label(data = state,aes(X,Y,label = STUSPS))



# racial minorities 


gwss.eth <- GWmodel::gwss(df.sp,vars = c("age_adjusted_rate", "R_RacialEthnic_Minorities"),
                          kernel="bisquare", adaptive=TRUE, bw=48)


gwss.eth # this is the summary statistics for food insecurity

# we can get this correlation into our main dataset df3 to be able to plot it.
# 

df$eth_corr = gwss.eth$SDF$Spearman_rho_age_adjusted_rate.R_RacialEthnic_Minorities


eth = ggplot(df) + geom_sf(aes(fill = eth_corr), color = NA) + 
    scale_fill_paletteer_c(`"grDevices::RdBu"`, direction = -1) + 
    blank() + ggtitle("Racial & Ethnic Minorities")

eth2 = eth + 
    geom_sf(data = state, fill = NA, color = "black", lwd = 0.7) + blank() 

eth2


eth3 = eth2 + 
    geom_sf(data = state, fill = NA, color = "black", lwd = 0.7) + blank() + 
    geom_label(data = state,aes(X,Y,label = STUSPS))

eth3


pcorr2 = plot_grid(income2, primary3,eth3, ncol=2)

ggsave(plot = pcorr2,filename = "figures/corr2.tiff",height=10,width=18,units="in",dpi=600,
            device = "tiff")

##########
# FERTIG #
##########