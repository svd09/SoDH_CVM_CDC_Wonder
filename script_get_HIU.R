library(tidyverse);library(readxl);library(glue)

t = read_excel("G:/table.xlsx")

glimpse(t)

# keep with county fips not 0 
# 

t2 = t %>% filter(countyfips != 0)

# limit to those counties that are in my data 
# 

t2$countyfips = as.numeric(t2$countyfips)

t3 = t2 %>% filter(fips %in% df3$county_code)

t3$fips = as.numeric(t3$fips)

t4 = t3 %>% filter(agecat == 5)

t5 = t4 %>% filter(iprcat == 0)

t6 = t5 %>% dplyr::select(fips, PCTUI)

summary(t6$PCTUI)

