## ---------------------------
##
## Script name: Geo weighted linear regression model
##
## Purpose of script: to fit the geo weighted linear regression model for the DM CVM paper 
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

## load up the packages we will need:  (uncomment as required)
# run from here...

options(scipen = 6, digits = 4) # I prefer to view outputs in non-scientific notation


library(easypackages)

libraries(c("tidyverse","sf","spdep",
            "splancs","pgirmess","classInt",
            "raster","broom","rgdal","janitor","ggsn","paletteer",
            "GWmodel","sp","spdep","ggrepel"))
library(spgwr)

## ---------------------------
## 
## START CODING ######################
## 
## 

# geographically weighted regression models 




# get the data 


df = readRDS("data/dm_clean_ready.rds") # got the data 

class(df)


df2 = df %>% dplyr::rename(
    pm25 = PM25,
    race_minor = R_RacialEthnic_Minorities,
    income = Med_H_income,
    food = Food_insecurity,
    pri_care = R_Pri_Care_Phys,
    rural = Rural_R,
    edu = R_college
)

class(df2)

summary(df2$age_adjusted_rate)

hist(df2$age_adjusted_rate)

hist(log(df2$age_adjusted_rate))

library(car)

p <- ggplot(df2, aes(sample = age_adjusted_rate))

a = p + stat_qq() + stat_qq_line()

p.log <- ggplot(df2, aes(sample = log(age_adjusted_rate)))

b = p.log + stat_qq() + stat_qq_line()

qqPlot(df2$age_adjusted_rate)

qqPlot(log(df2$age_adjusted_rate))


library(cowplot)

plot_grid(a,b,nrow=1) # some improvement in distribution present.

# convert to sp object 

df_sp = as(df2, "Spatial")

b_width = bw.gwr(
    log(age_adjusted_rate) ~ pm25 + race_minor + income + food + 
        rural + edu + pri_care, 
    approach = "AIC",
    adaptive = T,
    data = df_sp)

b_width


# b_width_fixed = bw.gwr(
#     log(age_adjusted_rate) ~ pm25 + race_minor + income + food + rural + edu + pri_care, 
#     approach = "AIC",
#     adaptive = F,
#     data = df_sp)
# 
# b_width_fixed

geo_w_model = gwr.basic(
    log(age_adjusted_rate) ~ pm25 + race_minor + 
        income + food + rural + edu + pri_care, 
    adaptive = T,
    data = df_sp, 
    bw = b_width
)

geo_w_model

# ***********************************************************************
#     *                       Package   GWmodel                             *
#     ***********************************************************************
#     Program starts at: 2023-06-04 08:20:52 
# Call:
#     gwr.basic(formula = log(age_adjusted_rate) ~ pm25 + race_minor + 
#                   income + food + rural + edu + pri_care, data = df_sp, bw = b_width, 
#               adaptive = T)
# 
# Dependent (y) variable:  age_adjusted_rate
# Independent variables:  pm25 race_minor income food rural edu pri_care
# Number of data points: 2589
# ***********************************************************************
#     *                    Results of Global Regression                     *
#     ***********************************************************************
#     
#     Call:
#     lm(formula = formula, data = data)
# 
# Residuals:
#     Min      1Q  Median      3Q     Max 
# -2.0721 -0.1924  0.0182  0.2296  1.0165 
# 
# Coefficients:
#                   Estimate   Std. Error t value  Pr(>|t|)    
# (Intercept)  5.003742424  0.089282193   56.04   < 2e-16 ***
# pm25         0.006767998  0.005808001    1.17     0.244    
# race_minor   0.001054688  0.000446920    2.36     0.018 *  
# income      -0.000008284  0.000000871   -9.51   < 2e-16 ***
# food         0.006512809  0.002808050    2.32     0.020 *  
# rural       -0.000561015  0.000313416   -1.79     0.074 .  
# edu         -0.002186245  0.000909305   -2.40     0.016 *  
# pri_care    -0.001242419  0.000260322   -4.77     0.0000019 ***
#     
#     ---Significance stars
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
# Residual standard error: 0.343 on 2581 degrees of freedom
# Multiple R-squared: 0.19
# Adjusted R-squared: 0.188 
# F-statistic: 86.6 on 7 and 2581 DF,  p-value: <2e-16 
# ***Extra Diagnostic information
# Residual sum of squares: 303.6
# Sigma(hat): 0.3426
# AIC:  1816
# AICc:  1816
# BIC:  -649.3
# ***********************************************************************
#     *          Results of Geographically Weighted Regression              *
#     ***********************************************************************
#     
#     *********************Model calibration information*********************
#     Kernel function: bisquare 
# Adaptive bandwidth: 139 (number of nearest neighbours)
# Regression points: the same locations as observations are used.
# Distance metric: Euclidean distance metric is used.
# 
# ****************Summary of GWR coefficient estimates:******************
#                   Min.     1st Qu.      Median     3rd Qu.  Max.
# Intercept  -0.49846033  4.18057397  4.82460508  5.61885936 10.19
# pm25       -0.55165603 -0.07422382  0.00974440  0.07802365  0.60
# race_minor -0.02522727 -0.00291962  0.00209365  0.00575910  0.02
# income     -0.00003059 -0.00001186 -0.00000627 -0.00000216  0.00
# food       -0.10544830 -0.00564542  0.01644510  0.03684077  0.14
# rural      -0.01108796 -0.00263679 -0.00121981  0.00058933  0.01
# edu        -0.01546665 -0.00726839 -0.00377867  0.00014031  0.01
# pri_care   -0.00674569 -0.00213330 -0.00084626 -0.00001529  0.01
# ************************Diagnostic information*************************
#     Number of data points: 2589 
# Effective number of parameters (2trace(S) - trace(S'S)): 455 
#    Effective degrees of freedom (n-2trace(S) + trace(S'S)): 2134 
# AICc (GWR book, Fotheringham, et al. 2002, p. 61, eq 2.33): 922.1 
# AIC (GWR book, Fotheringham, et al. 2002,GWR p. 96, eq. 4.22): 469.1 
# BIC (GWR book, Fotheringham, et al. 2002,GWR p. 61, eq. 2.34): 241.9 
# Residual sum of squares: 159.1 
# R-square value:  0.5757 
# Adjusted R-square value:  0.4852 
# 
# ***********************************************************************
#     Program stops at: 2023-06-04 08:20:55 


names(geo_w_model)

str(geo_w_model)

table_gwr = rbind(apply(geo_w_model$SDF@data[,1:8], 2, summary), coef(geo_w_model))


t(table_gwr) # this table provides coeff for the local model.

#               Min.     1st Qu.       Median         Mean      3rd Qu.        Max.
# Intercept  -0.49846033  4.18057397  4.824605076  4.954735881  5.618859355 10.18571809
# pm25       -0.55165603 -0.07422382  0.009744401 -0.001045255  0.078023652  0.60205815
# race_minor -0.02522727 -0.00291962  0.002093649  0.001346942  0.005759099  0.02175026
# income     -0.00003059 -0.00001186 -0.000006274 -0.000006702 -0.000002165  0.00002197
# food       -0.10544830 -0.00564542  0.016445098  0.015065144  0.036840768  0.13515168
# rural      -0.01108796 -0.00263679 -0.001219809 -0.001024484  0.000589331  0.00813835
# edu        -0.01546665 -0.00726839 -0.003778666 -0.003467087  0.000140313  0.01499093
# pri_care   -0.00674569 -0.00213330 -0.000846256 -0.001071975 -0.000015293  0.00525463

# map the results for the geo weighted model 

model_sf = st_as_sf(geo_w_model$SDF)

glimpse(model_sf)


# get maps for each covariate to understand the results of the linear regression model.

# air pollution 

p = ggplot(data = model_sf) + geom_sf(aes(fill = pm25_TV)) + ggtitle("pm2.5") + 
    scale_fill_viridis_c(option = "magma", direction = -1)

# race_minor 


r = ggplot(data = model_sf) + geom_sf(aes(fill = race_minor_TV)) + ggtitle("race") + 
    scale_fill_viridis_c(option = "magma", direction = -1)


# income 


i = ggplot(data = model_sf) + geom_sf(aes(fill = income_TV)) + ggtitle("income") + 
    scale_fill_viridis_c(option = "magma", direction = -1)

# food insecurity


f = ggplot(data = model_sf) + geom_sf(aes(fill = food_TV)) + ggtitle("food ") + 
    scale_fill_viridis_c(option = "magma", direction = -1)


# rural 


l = ggplot(data = model_sf) + geom_sf(aes(fill = rural_TV)) + ggtitle("rural ") + 
    scale_fill_viridis_c(option = "magma", direction = -1)

# edu 


e = ggplot(data = model_sf) + geom_sf(aes(fill = edu_TV)) + ggtitle("education") + 
    scale_fill_viridis_c(option = "magma", direction = -1)


# primary care 

c = ggplot(data = model_sf) + geom_sf(aes(fill = pri_care_TV)) + ggtitle("education") + 
    scale_fill_viridis_c(option = "magma", direction = -1)



library(cowplot)

gm = cowplot::plot_grid(p,r,i,f,nrow = 2) 


ggsave(plot = gm, filename = "paper_figures/gm_plot.pdf",
       width = 10,height=10,units = "in",device = "pdf")

gm2 = cowplot::plot_grid(l,e,c,nrow = 2)


ggsave(plot = gm2, filename = "paper_figures/gm_plot2.pdf",
       width = 10,height=10,units = "in",device = "pdf")


# better than plotting the coefficients is to plot the local p-values or t-values

t = GWmodel::gwr.t.adjust(geo_w_model)

glimpse(t)

summary(t$results$p)

sum_p = t$results$p

sum.p.df = data.frame(sum_p)

# now to only plot those places that have p < 0.05 for pm2.5 
# can add these p-values to the model_sf
# need to plot them using state boundaries + region boundaries

state = read_rds("data/state_shape.rds")

state <- cbind(state, st_coordinates(st_centroid(state))) # get the centroid for 
# state labels.

regions = sf::st_read("us_regions/Deo_regions.shp", stringsAsFactors = F)

class(regions)

glimpse(regions)

regions = cbind(regions, st_coordinates(st_centroid(regions)))



model_sf$pm25_p = sum.p.df$pm25_p
model_sf$pm_p_group = with(model_sf, 
                           ifelse(
                               pm25_p < 0.01, 1, 
                               ifelse(pm25_p >= 0.01 & pm25_p < 0.05, 2, 3
                           )))

model_sf$pm_p_group = factor(model_sf$pm_p_group)

pm1 = ggplot(data = model_sf) + geom_sf(aes(fill = pm_p_group), color = NA) + ggtitle("pm2.5 p-values") + 
    scale_fill_grey() + theme(legend.position = "none") + blank()

pm2 = pm1 + 
    geom_sf(data = state, fill = NA, color = "black", lwd = 0.7) + 
    geom_sf(data = regions, fill = NA, color = "red", lwd = 0.7) + 
    geom_label_repel(data = state, aes(X,Y,label = STUSPS)) 

pm2

ggsave(plot = pm2,filename = "paper_figures/pm25_p.tiff", height = 10, 
       width = 16, units = "in", device = "tiff",dpi = 600)

# food insecurity areas

model_sf$food_p = sum.p.df$food_p
model_sf$food_p_group = with(model_sf, 
                           ifelse(
                               food_p < 0.01, 1, 
                               ifelse(food_p >= 0.01 & food_p < 0.05, 2, 3
                               )))

model_sf$food_p_group = factor(model_sf$food_p_group)

food1 = ggplot(data = model_sf) + geom_sf(aes(fill = food_p_group), color = NA) + ggtitle("food security p-values") + 
    scale_fill_grey() + theme(legend.position = "none") + blank()

food2 = food1 + 
    geom_sf(data = state, fill = NA, color = "black", lwd = 0.7) + 
    geom_sf(data = regions, fill = NA, color = "red", lwd = 0.7) + 
    geom_label_repel(data = state, aes(X,Y,label = STUSPS)) 

food2

ggsave(plot = food2,filename = "paper_figures/food_p.tiff", height = 10, 
       width = 16, units = "in", device = "tiff",dpi = 600)



# race minorities 


model_sf$race_p = sum.p.df$race_minor_p
model_sf$race_p_group = with(model_sf, 
                             ifelse(
                                 race_p < 0.01, 1, 
                                 ifelse(race_p >= 0.01 & race_p < 0.05, 2, 3
                                 )))

model_sf$race_p_group = factor(model_sf$race_p_group)

race1 = ggplot(data = model_sf) + geom_sf(aes(fill = race_p_group), color = NA) + 
    ggtitle("racial minorities p-values") + 
    scale_fill_grey() + theme(legend.position = "none")+ blank()

race2 = race1 + 
    geom_sf(data = state, fill = NA, color = "black", lwd = 0.7) + 
    geom_sf(data = regions, fill = NA, color = "red", lwd = 0.7) + 
    geom_label_repel(data = state, aes(X,Y,label = STUSPS)) 

race2

ggsave(plot = race2,filename = "paper_figures/race_p.tiff", height = 10, 
       width = 16, units = "in", device = "tiff",dpi = 600)

# income_p


model_sf$income_p = sum.p.df$income_p
model_sf$income_p_group = with(model_sf, 
                             ifelse(
                                 income_p < 0.01, 1, 
                                 ifelse(income_p >= 0.01 & income_p < 0.05, 2, 3
                                 )))

model_sf$income_p_group = factor(model_sf$income_p_group)

income1 = ggplot(data = model_sf) + geom_sf(aes(fill = income_p_group), color = NA) + 
    ggtitle("median household income p-values") + 
    scale_fill_grey() + theme(legend.position = "none")+ blank()

income2 = income1 + 
    geom_sf(data = state, fill = NA, color = "black", lwd = 0.7) + 
    geom_sf(data = regions, fill = NA, color = "red", lwd = 0.7) + 
    geom_label_repel(data = state, aes(X,Y,label = STUSPS)) 

income2

ggsave(plot = income2,filename = "paper_figures/income_p.tiff", height = 10, 
       width = 16, units = "in", device = "tiff",dpi = 600)

# rural counties 


model_sf$rural_p = sum.p.df$rural_p
model_sf$rural_p_group = with(model_sf, 
                               ifelse(
                                   rural_p < 0.01, 1, 
                                   ifelse(rural_p >= 0.01 & rural_p < 0.05, 2, 3
                                   )))

model_sf$rural_p_group = factor(model_sf$rural_p_group)

rural1 = ggplot(data = model_sf) + geom_sf(aes(fill = rural_p_group), color = NA) + 
    ggtitle("rurality of counties p-values") + 
    scale_fill_grey() + theme(legend.position = "none") + blank()


rural2 = rural1 + 
    geom_sf(data = state, fill = NA, color = "black", lwd = 0.7) + 
    geom_sf(data = regions, fill = NA, color = "red", lwd = 0.7) + 
    geom_label_repel(data = state, aes(X,Y,label = STUSPS)) 

rural2

ggsave(plot = rural2,filename = "paper_figures/rural_p.tiff", height = 10, 
       width = 16, units = "in", device = "tiff",dpi = 600)


# education 


model_sf$education_p = sum.p.df$edu_p
model_sf$education_p_group = with(model_sf, 
                              ifelse(
                                  education_p < 0.01, 1, 
                                  ifelse(education_p >= 0.01 & education_p < 0.05, 2, 3
                                  )))

model_sf$education_p_group = factor(model_sf$education_p_group)

educ1 = ggplot(data = model_sf) + geom_sf(aes(fill = education_p_group), color = NA) + 
    ggtitle("college education p-values") + 
    scale_fill_grey() + theme(legend.position = "none") + blank()

educ2 = educ1 + 
    geom_sf(data = state, fill = NA, color = "black", lwd = 0.7) + 
    geom_sf(data = regions, fill = NA, color = "red", lwd = 0.7) + 
    geom_label_repel(data = state, aes(X,Y,label = STUSPS)) 

educ2

ggsave(plot = educ2,filename = "paper_figures/educ_p.tiff", height = 10, 
       width = 16, units = "in", device = "tiff",dpi = 600)


# primary care access 


model_sf$pri_care_p = sum.p.df$pri_care_p
model_sf$pri_care_p_group = with(model_sf, 
                                  ifelse(
                                      pri_care_p < 0.01, 1, 
                                      ifelse(pri_care_p >= 0.01 & pri_care_p < 0.05, 2, 3
                                      )))

model_sf$pri_care_p_group = factor(model_sf$pri_care_p_group)

pricare1 = ggplot(data = model_sf) + geom_sf(aes(fill = pri_care_p_group), color = NA) + 
    ggtitle("primary care p-values") + 
    scale_fill_grey() + theme(legend.position = "none") + blank()

pricare2 = pricare1 + 
    geom_sf(data = state, fill = NA, color = "black", lwd = 0.7) + 
    geom_sf(data = regions, fill = NA, color = "red", lwd = 0.7) + 
    geom_label_repel(data = state, aes(X,Y,label = STUSPS)) 

pricare2

ggsave(plot = pricare2,filename = "paper_figures/pricare_p.tiff", height = 10, 
       width = 16, units = "in", device = "tiff",dpi = 600)


# local R2

summary(model_sf$Local_R2)

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.225   0.431   0.552   0.544   0.662   0.887

str(model_sf$Local_R2) # add this to the main dataset

df$local_r = model_sf$Local_R2

tapply(df$local_r, df$region, summary)


###########
# finished here
###########


#- some diagnostic tests for the model

vif(geo_w_model)


model_diag = GWmodel::gwr.collin.diagno(
    log(age_adjusted_rate) ~ pm25 + race_minor + 
        income + food + rural + edu + pri_care, 
    adaptive = T,
    data = df_sp, 
    bw = b_width
)

model_diag$VIF

dim(model_diag$VIF)

vif = data.frame(model_diag$VIF)

dim(vif)

vif

ggplot(data = vif,aes(x = pm25)) + geom_histogram()

colnames(vif) = c('pm25' , 'race_minor' , 
                      'income' , 'food', 'rural' , 'edu' , 'pri_care')


str(vif)


plots = list()


pm25_vif =  ggplot(data = vif,aes(x = pm25)) + geom_histogram()

race_vif = ggplot(data = vif,aes(x = race_minor)) + geom_histogram()

income = ggplot(data = vif,aes(x = income)) + geom_histogram()

food = ggplot(data = vif,aes(x = food)) + geom_histogram()

colnames(vif)


dim(vif)

rural = ggplot(data = vif,aes(x = rural)) + geom_histogram()

edu = ggplot(data = vif,aes(x = edu)) + geom_histogram()

pri_care = ggplot(data = vif,aes(x = pri_care)) + geom_histogram()

library(cowplot)

cowplot::plot_grid(pm25_vif, race_vif, income, food, 
                   rural, edu, pri_care,nrow = 4)

# save this data

write_rds(vif,"results/vif.rds")

vif = read_rds("results/vif.rds")

# calculate the number of 

vif

vif2 = vif %>% mutate(
    c_pm25 = ifelse(pm25 >= 10, 1, 0),
    c_race_minor = ifelse(race_minor >= 10, 1, 0),
    c_income = ifelse(income >= 10, 1, 0),
    c_food = ifelse(food >= 10, 1, 0),
    c_rural = ifelse(rural >= 10, 1, 0),
    c_edu = ifelse(edu >= 10, 1, 0),
    c_pri = ifelse(pri_care >= 10, 1, 0)
)


vif3 = vif2[,8:14]

vif3

vif3$total = rowSums(vif3)

head(vif3,2)

(sum(vif3$total)/(2589*7))*100

# 1.23% locations had a VIF >= 10 in the analysis, so very little collinearity.

#- making better plots 

df = read_rds("results/vif.rds")

str(df)

#- pm25

pm25 = df %>% dplyr::select(pm25)

pm25$high = with(pm25, ifelse(pm25 >= 10, 1, 0))



ggplot(data = pm25, aes(x = pm25,group = factor(high), fill = factor(high))) + geom_histogram()



income = df %>% dplyr::select(income)

income$high = with(income, ifelse(income >= 10, 1, 0))

summary(income)

income$high = factor(income$high)

ggplot(data = income, aes(x = income, group = high, fill = high)) + geom_histogram() + 
    scale_fill_manual(values = c("gray","red"))



# to make a panel 

cowplot::plot_grid(pm25_vif, race_vif, income, food, rural, edu, pri_care,nrow = 4)
