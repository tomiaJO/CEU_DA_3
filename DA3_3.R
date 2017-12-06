library(data.table)
library(ggplot2)

dt <- fread('mortality_oldage_eu.csv')

#### Filter data: Keep respondents between 50 and 80 years of age. 
#### The variables you will need are whether the person deceased within 6 years of the interview (“deceased”), gender (“female”), age (“age”), years of education (“eduyears_mod”), income group within country (“income10g), and the explanatory variables of your focus, physical activities (variable “sports”: 1: more than once a week, 2: once a week, 3: one to three times a month, 4: hardly ever, or never). 

dt <- dt[age >= 50 & age <= 80, ]
dt <- dt[, c("deceased", "female", "age", "eduyears_mod", "income10g", "sports")]

#### Do exploratoty analysis: Create binary variables from the sports variable. 
#### Describe these variables in your dataset. Drop observations that have missing value for either.

options(na.action = 'na.pass')
m <- model.matrix(~ -1 + deceased + female + age + eduyears_mod + income10g + factor(sports), data = dt)
dt <- data.table(m)
options(na.action = 'na.omit')

dt <- na.omit(dt)

ggplot(dt, aes(age)) + geom_histogram(binwidth = 5) + facet_wrap(~ female)
##lognormal distribution -- consider transformation?

ggplot(dt, aes(eduyears_mod)) + geom_histogram(binwidth = 1) + facet_wrap(~ female)
## normal distribution

## both similar for female / male. test means?

##heatmap on age vs edu
dt[, age_cat := cut(age, breaks = 5 *(10:16), include.lowest = TRUE)]
dt[, edu_cat := cut(eduyears_mod, breaks = 2*(0:10), include.lowest = TRUE)]

dt_heatmap1 <- dt[, .(cnt_cat = .N), by = c("female", "age_cat", "edu_cat")]
dt_temp <- dt[, .(cnt_gender = .N), by = female]

dt_heatmap1 <- merge(dt_heatmap1, dt_temp, all.x=TRUE)

dt_heatmap1[, ratio_cat := cnt_cat / cnt_gender] 

ggplot(data = dt_heatmap1, aes(x = age_cat, y = edu_cat)) +
  geom_tile(aes(fill = ratio_cat), colour = "white") + 
  scale_fill_gradient(low = "white", high = "steelblue") + 
  facet_wrap(~ female)

dt[eduyears_mod == 0, ]

round(cor(dt[, c("age", "eduyears_mod", "income10g")]), 2)
## we don't have enough evidence to drop

##heatmap on age vs income
dt_heatmap2 <- dt[, .(cnt_cat = .N), by = c("female", "age_cat", "income10g")]

dt_heatmap2 <- merge(dt_heatmap2, dt_temp, all.x=TRUE)
dt_temp <- NULL

dt_heatmap2[, ratio_cat := cnt_cat / cnt_gender] 

ggplot(data = dt_heatmap2, aes(x = age_cat, y = income10g)) +
  geom_tile(aes(fill = ratio_cat), colour = "white") + 
  scale_fill_gradient(low = "white", high = "steelblue") + 
  facet_wrap(~ female)
## income gradually decreasing with age (most likely people retire / work less)


ggplot(data = dt, aes(x = age_cat, y = income10g)) + geom_boxplot()
##might not want to include, but it re-affirms the above