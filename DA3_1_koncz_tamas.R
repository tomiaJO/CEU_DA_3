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

## 

binary_model <- dt[, .(avg_price = mean(price)), by = is_close]
ggplot(binary_model, aes(is_close, avg_price)) + geom_point() + 
                                                  scale_y_continuous(expand = c(0, 0), limits = c(0,150)) +
                                                  geom_text(aes(label = round(avg_price, 2)), hjust = - 0.1, vjust = 1)

dt[, binary_pred := mean(price), by = is_close]
r2_binary <- var(dt$binary_pred) / var(dt$price)
r2_binary

##

?lowess
lowess(dt$distance, dt$price)
summary(loess(distance ~ price, dt))


ggplot(dt, aes(distance, price)) + geom_point(aes(color = dt$is_close)) + geom_smooth(method = 'lm')
