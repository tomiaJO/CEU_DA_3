library(data.table)
library(ggplot2)

dt <- fread('hotels_all_nov21.csv')

str(dt)
head(dt)


dt[, .N, by = accommodation_type][order(-N)]

dt <- dt[accommodation_type %in% c('Hotel', 'Hostel'), ]
dt <- dt[stars >= 2, ]
dt <- dt[city == 'Barcelona', ]

###

ggplot(dt, aes(price)) + geom_histogram(binwidth = 10)
ggplot(dt[distance<5, ], aes(distance)) + geom_histogram(binwidth = .25)

p <- ggplot(dt, aes(distance, price)) + geom_point() + geom_smooth(method = 'lm') 
p
p + facet_wrap(~stars)

###

dt <- dt[price <= 150, ]
dt <- dt[distance <= 3.5, ]
dt <- dt[order(-price), c('name', 'stars', 'rating', 'distance', 'price')]


ggplot(dt, aes(rating, price)) + geom_point() + geom_smooth(method = 'lm')
ggplot(dt, aes(distance, price)) + geom_point() + geom_smooth(method = 'lm')
ggplot(dt, aes(distance, rating)) + geom_point() + geom_smooth(method = 'lm')


ggplot(dt, aes(1, distance)) + geom_boxplot()

cutoff <- dt[, median(distance)]
dt[, is_close := distance < cutoff]
head(dt)

## binary

binary_model <- dt[, .(avg_price = mean(price)), by = is_close]
ggplot(binary_model, aes(is_close, avg_price)) + geom_point() + 
                                                  scale_y_continuous(expand = c(0, 0), limits = c(0,150)) +
                                                  geom_text(aes(label = round(avg_price, 2)), hjust = - 0.1, vjust = 1)

dt[, binary_pred := mean(price), by = is_close]
r2_binary <- var(dt$binary_pred) / var(dt$price)
r2_binary

## lowess

loess_model <- loess(price ~ distance, dt)
dt$loess_pred <- predict(loess_model)

ggplot(dt, aes(distance, price)) + geom_point() + geom_smooth(method = 'loess')
ggplot(dt, aes(distance, price)) + geom_point() + geom_line(data=dt,aes(x=distance,y=loess_pred),colour="blue")

r2_loess <- var(dt$loess_pred) / var(dt$price)
r2_loess

## simple linear regression
simpleLM <- lm(price ~ distance, dt)
summary(simpleLM)

dt$simpleLM_pred <- predict(simpleLM)

ggplot(dt, aes(distance, price)) + geom_point() + geom_smooth(method = 'lm')
ggplot(dt, aes(distance, price)) + geom_point() + 
                                    geom_line(data=dt,aes(x=distance,y=simpleLM_pred),colour="blue")

r2_simpleLM <- var(dt$simpleLM_pred) / var(dt$price)
r2_simpleLM 

## non-linears

## log(price)
dt[, ln_price := log(price)]

logLM <- lm(ln_price ~ distance, dt)
summary(logLM)

dt$logLM_pred <- predict(logLM) ##transform this back to original price level

ggplot(dt, aes(distance, ln_price)) + geom_point() + geom_smooth(method = 'lm')
ggplot(dt, aes(distance, ln_price)) + geom_point() + 
  geom_line(data=dt,aes(x=distance,y=logLM_pred),colour="blue")

r2_logLM <- var(dt$logLM_pred) / var(dt$ln_price)
r2_logLM 

## spline (knots 1 - 2)
library(lspline)

##1 knot
spline_1knot_model <- lm(price ~ lspline(distance, cutoff), data=dt)
summary(spline_1knot_model)
dt$spline_1knot_pred <- predict(spline_1knot_model)

ggplot(data = dt, aes(x=distance, y=price)) +
  geom_point(size=1.5, colour="orange",shape=4) +
  labs(x="Distance to city center (km)",y="Hotel price (EUR)")+
  geom_line(data=dt,aes(x=distance,y=spline_1knot_pred),colour="blue")+
  geom_vline(xintercept=cutoff,colour="red")

r2_spline_1knot <- var(dt$spline_1knot_pred) / var(dt$price)
r2_spline_1knot

## 2 knots
knots = c(1, 2.5)
spline_2knot_model <- lm(price ~ lspline(distance, knots), data = dt)
summary(spline_2knot_model)
dt$spline_2knot_pred <- predict(spline_2knot_model)

ggplot(data = dt, aes(x = distance, y = price)) +
  geom_point(size = 1.5, colour = "orange", shape = 4) +
  labs(x = "Distance to city center (km)", y = "Hotel price (EUR)") +
  geom_line(data = dt, aes(x = distance, y = spline_2knot_pred), colour = "blue") +
  geom_vline(xintercept = knots, colour = "red")

r2_spline_2knot <- var(dt$spline_2knot_pred) / var(dt$price)
r2_spline_2knot

## poly (quadratic - cubic)
dt$distance_sq <- dt$distance ^ 2
dt$distance_cub <- dt$distance ^ 3

## square
sqLM_model <- lm(price ~ distance + distance_sq, dt)
summary(sqLM_model)
dt$sqLM_pred <- predict(sqLM_model)

ggplot(data = dt, aes(x = distance, y = price)) + geom_point(size = 2, color = "orange", shape = 4) +
  labs(x = "Distance to city center (km)", y = "Hotel price (EUR)") +
  geom_line(data = dt, aes(x = distance, y = sqLM_pred), color = "blue")

r2_sqLM <- var(dt$sqLM_pred) / var(dt$price)
r2_sqLM

##cubic
cubLM_model <- lm(price ~ distance + distance_sq + distance_cub, dt)
summary(cubLM_model)
dt$cubLM_pred <- predict(cubLM_model)
  
ggplot(data = dt, aes(x = distance, y = price)) + geom_point(size = 2, color = "orange", shape = 4) +
  labs(x = "Distance to city center (km)", y = "Hotel price (EUR)") +
  geom_line(data = dt, aes(x = distance, y = cubLM_pred), color = "blue")

r2_cubLM <- var(dt$cubLM_pred) / var(dt$price)
r2_cubLM

## sub-sample
dt[, .N, by = stars][order(as.numeric(-stars))]
dt[, .N, by = rating][order(-rating)]

ggplot(data = dt, aes(x = stars, y = rating)) + geom_point() + geom_smooth()
