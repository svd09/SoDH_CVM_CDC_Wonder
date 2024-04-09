#- making better plots 

library(tidyverse)

df = read_rds("results/vif.rds")

str(df)

#- pm25

pm25 = df %>% dplyr::select(pm25)

pm25$high = with(pm25, ifelse(pm25 >= 10, 1, 0))



ggplot(data = pm25, aes(x = pm25,group = factor(high), fill = factor(high))) + geom_histogram()

# follow the example for the income code.


income = df %>% dplyr::select(income)

glimpse(income)


# saving a plot in base R 

tiff(filename = ,width = ,height = ,units = ,res = )
hist(income$income)
dev.off()

# create multiple plots in 1 image 

tiff()
par(mfrow = c(2,2)) # split to the canvas 

plot1 
plot2
plot3
plot4
dev.off() # use this to reset the graphics device 




income$high = with(income, ifelse(income > 10, 1, 0))

summary(income)

income$high = factor(income$high)

p = ggplot(data = income, aes(x = income, group = high, fill = high)) + geom_histogram() + 
    scale_fill_manual(values = c("gray","red"))

p




library(ggprism)

p2 = p + theme_prism()

p3 = p2 + ggtitle("My plot") + xlab("Income")

library(ggthemes)


p4 = p3 + theme_clean()

p4





ggsave(filename = "location/name.tiff",
       plot = p,
       device = "tiff",
       width = 18,
       height = 12,
       dpi = 600,
       units = "in")


# to make a panel 

t = cowplot::plot_grid(pm25_vif, race_vif, income, food, rural, edu, pri_care,nrow = 4)



dev.off()


tiff("figures/histo.tiff")
p
dev.off()

hist(pm25$pm25)
legend("topright",legend = "PM2.5")

library(survival)


s = survfit(Surv(time, status) ~ 1, data = lung)


tiff("figures/histo.tiff")

plot(s)
abline(v = 400)
legend("topleft",legend = "Survival Curve")

dev.off()