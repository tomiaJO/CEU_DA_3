library(data.table)
library(ggplot2)

dt <- fread('mortality_oldage_eu.csv')
head(dt)

#### Filter data: Keep respondents between 50 and 80 years of age. 
#### The variables you will need are whether the person deceased within 6 years of the interview (“deceased”), gender (“female”), age (“age”), years of education (“eduyears_mod”), income group within country (“income10g), and the explanatory variables of your focus, physical activities (variable “sports”: 1: more than once a week, 2: once a week, 3: one to three times a month, 4: hardly ever, or never). 

ggplot(dt, aes(age)) + geom_histogram()

dt <- dt[age >= 50 & age <= 80, ]
dt <- dt[, c("deceased", "female", "age", "eduyears_mod", "income10g", "sports")]

unique(dt$sports)
?model.matrix

options(na.action='na.pass')

m <- model.matrix(~ -1 + factor(sports), data = dt)
nrow(m)
#return option to default
options(na.action='na.omit')
nrow(dt)
