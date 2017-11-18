# Clear the console
cat("\f")

# CLEAR MEMORY
rm(list=ls())

library(lspline)

## data prep
x <- wb(country = "countries_only", indicator = c("NY.GDP.PCAP.PP.KD", "SP.DYN.LE00.IN", "SP.POP.TOTL") , startdate = 2015, enddate = 2015)
countries <- data.table(x)

dt <- countries[, c('value', 'country', 'indicator')]
dt <- spread(dt, key = 'indicator', value = 'value', fill = NA)  

s_country = colnames(dt)[1]
s_gdp = colnames(dt)[2]
s_life_exp = colnames(dt)[3]
s_pop = colnames(dt)[4]

setnames(dt, old = colnames(dt), new = c('country', 'gdp', 'life_exp', 'pop'))

dt <- dt[is.na(gdp) == FALSE, ]
dt <- dt[is.na(pop) == FALSE, ]
dt <- dt[is.na(life_exp) == FALSE, ]

dt[, pop := pop / 1000000]
dt[, lngdp := log(gdp)]

write.csv(x = dt, file = 'WD_Data_Filtered.csv')

##data viz
ggplot(data = dt, aes(gdp)) + geom_histogram(binwidth = 5000) + 
  scale_x_continuous(labels = scales::comma)

ggplot(data = dt, aes(lngdp)) + geom_histogram(binwidt = 2) + 
  scale_x_continuous(labels = scales::comma)

ggplot(data = dt, aes(life_exp)) + geom_histogram(binwidth = 2.5) + 
  scale_x_continuous(labels = scales::comma)

ggplot(data = dt, aes(pop)) + geom_histogram(binwidth = 25) + 
  scale_x_continuous(labels = scales::comma)

ggplot(data = dt, aes(lngdp, life_exp)) + geom_point() + 
  geom_smooth(method = 'loess')

ggplot(data = dt, aes(lngdp, pop)) + geom_point() + 
  geom_smooth(method = 'loess')

ggplot(data = dt, aes(life_exp, pop)) + geom_point() + 
  geom_smooth(method = 'loess')

ggplot(data = dt, aes(lngdp, life_exp)) + geom_point(size = log(dt$pop) / 2.5) + 
  geom_smooth(method = 'loess')

## estimating lowess
lowess_model <- loess(life_exp ~ lngdp, data = dt)
summary(lowess_model)
dt$lowess_pred <- predict(lowess_model)

ggplot(data = dt, aes(lngdp, life_exp)) + geom_point(size = log(dt$pop) / 2.5) + 
  geom_line(data = dt, aes(lngdp, lowess_pred), color = 'blue')



## estimating linear regression
lm_model <- lm(life_exp ~ lngdp, data = dt)
summary(lm_model)
dt$lm_pred <- predict(lm_model)

ggplot(data = dt, aes(lngdp, life_exp)) + geom_point(size = log(dt$pop) / 2.5) + 
  geom_line(data = dt, aes(lngdp, lm_pred), color = 'blue')

r2_lm <- summary(lm_model)$r.squared
r2_lm

## estimating spline
knot = 10.5
spline_model <- lm(life_exp ~ lspline(lngdp, knot), data = dt)
summary(spline_model)
dt$spline_pred <- predict(spline_model)

ggplot(data = dt, aes(gdp, life_exp)) + geom_point(size = log(dt$pop) / 1.5, color = 'orange') + 
  geom_line(data = dt, aes(gdp, spline_pred), color = 'blue', size = .7) +
  geom_vline(xintercept = exp(knot), color = 'red', size = .7)

r2_spline <- summary(spline_model)$r.squared
r2_spline

## polynomial
dt$lngdp_sq <- dt$lngdp ^ 2
dt$lngdp_cub <- dt$lngdp ^ 3

lm_sq_model <- lm(life_exp ~ lngdp + lngdp_sq, data = dt)
summary(lm_sq_model)
dt$lm_sq_pred <- predict(lm_sq_model)

ggplot(data = dt, aes(gdp, life_exp)) + geom_point(size = log(dt$pop) / 2.5) +
  geom_line(data = dt, aes(gdp, lm_sq_pred), color = 'blue', size = .7)

r2_lm_sq <- summary(lm_sq_model)$r.squared
r2_lm_sq


lm_cub_model <- lm(life_exp ~ lngdp + lngdp_sq + lngdp_cub, data = dt)
summary(lm_cub_model)
dt$lm_cub_pred <- predict(lm_cub_model)

ggplot(data = dt, aes(gdp, life_exp)) + geom_point(size = log(dt$pop) / 2.5) +
  geom_line(data = dt, aes(gdp, lm_cub_pred), color = 'blue', size = .7)

r2_lm_cub <- summary(lm_cub_model)$r.squared
r2_lm_cub

## weighted regression
lm_weighted_model <- lm(life_exp ~ lngdp, weights = pop, data = dt)
summary(lm_weighted_model)
dt$lm_weighted_pred <- predict(lm_weighted_model)

ggplot(data = dt, aes(gdp, life_exp)) + geom_point(size = log(dt$pop) / 2.5) +
  geom_line(data = dt, aes(gdp, lm_weighted_pred), color = 'blue', size = .7)

r2_lm_weighted <- summary(lm_weighted_model)$r.squared
r2_lm_weighted
