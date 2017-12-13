library(data.table)
library(ggplot2)

# dt <- fread('bisnode_all.csv')
# x <- dt


##filtering
dt <- x[year == 2015, ]
yr_2014 <- x[year == 2014, c('comp_id', 'sales', 'labor_avg')]
setnames(yr_2014,old = names(yr_2014), new = c("comp_id", "sales_2014", "labor_avg_2014") )



?setnames
dt <- dt[is.na(dt$birth_year) == FALSE, ]
dt <- dt[is.na(dt$sales) == FALSE, ]
dt <- dt[is.na(dt$labor_avg) == FALSE, ]
dt <- dt[is.na(dt$ind) == FALSE, ]
dt <- dt[ind != "", ]
dt <- dt[is.na(region_m) == FALSE, ]
dt <- dt[region_m != "", ]

dt <- dt[labor_avg >= 2, ]

dt <- merge(x = dt, y = yr_2014, by = 'comp_id', all = FALSE)
dt <- dt[sales_2014 >= 100000, ]
# unique(dt$ind)
# sum(is.na(dt$sales))
# sum(is.na(dt$birth_year))
# min(dt$labor_avg)
# max(dt$labor_avg)

##variable creation
dt[, ceo_age := 2015 - birth_year]
dt[, sales_growth := sales / sales_2014 - 1]
dt[, ceo_age_sq := ceo_age ^ 2]
dt[, ceo_age_cub := ceo_age ^ 3]
dt[, ceo_young := ceo_age <= 35]
## dt[, ceo_age_cat := cut(ceo_age, breaks = 5*(0:20))]

breaks = quantile(dt$ceo_age, c(0, 30, 65, Inf))

dt[, ceo_age_cat := cut(ceo_age, breaks = c(0, 29.9, 45, 65, Inf), right = TRUE, left = FALSE)]

tmp <- dt[, .(sales_growth = mean(sales_growth), 
       count = .N), 
   by = c('ceo_age_cat', 'ind', 'region_m')][order(ceo_age_cat)]

## age cat vs sales_growth avg.
ggplot(tmp, aes(ceo_age_cat, sales_growth, size = count)) + 
  geom_point() + 
  facet_grid(ind ~ region_m)

ggplot(dt, aes(sales_growth)) + 
  geom_histogram() + 
  facet_grid(ind ~ region_m)

##age filter -- need good reasoning
dt <- dt[ceo_age >= 30 & ceo_age <= 65, ]

ggplot(dt, aes(labor_avg)) + geom_histogram(binwidth = 2.5)
ggplot(dt, aes(sales)) + geom_histogram()

ggplot(dt, aes(labor_avg, sales / 1000)) + 
  geom_point() + 
  geom_smooth() +
  scale_y_continuous(labels = scales::comma) +
  facet_grid (region_m~ind) 


ggplot(dt, aes(ceo_age)) + geom_histogram(binwidth = 2)
ggplot(dt, aes(sales_growth)) + geom_histogram(binwidth = 0.1)

ggplot(dt, aes(ceo_age, sales_growth)) + 
  geom_point() + 
  geom_smooth() + 
  scale_y_continuous(labels = scales::comma) +
  facet_grid(ind ~ region_m)

ggplot(dt, aes(ceo_age, sales_growth)) + 
  geom_point() + 
  geom_smooth() + 
  scale_y_continuous(labels = scales::comma) +
  facet_grid(~ceo_young)

dt[, .(sales_growth = median(sales_growth)), by = c('ind', 'ceo_young')]

lm <- lm(sales_growth ~ ceo_age_cat, data = dt)
summary(lm)
head(dt[order(labor_avg), c('labor_avg', 'sales', 'ind')][labor_avg != 0, ])
