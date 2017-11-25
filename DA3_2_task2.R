library(data.table)
library(ggplot2)

hotels <- fread('hotels_all_nov21.csv')

##dt[, .N, by = accommodation_type][order(-N)]

dt <- hotels[accommodation_type %in% c('Hotel', 'Hostel'), ]
dt <- dt[stars >= 2, ]
dt <- dt[stars <= 1000, ]

ggplot(dt, aes(distance, price)) + geom_point() + geom_smooth() + facet_wrap(~city)
