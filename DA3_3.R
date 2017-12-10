library(data.table)
library(ggplot2)
library(stargazer)
library(sandwich)
library(mfx)
##library(gmodels)
library(descr)

#### Filter data: Keep respondents between 50 and 80 years of age. 
#### The variables you will need are whether the person deceased within 6 years of the interview (“deceased”), gender (“female”), age (“age”), years of education (“eduyears_mod”), income group within country (“income10g), and the explanatory variables of your focus, physical activities (variable “sports”: 1: more than once a week, 2: once a week, 3: one to three times a month, 4: hardly ever, or never). 

dt <- fread('mortality_oldage_eu.csv')
dt <- dt[age >= 50 & age <= 80, ]
dt <- dt[, c("deceased", "female", "age", "eduyears_mod", "income10g", "sports")]

#### Do exploratoty analysis: Create binary variables from the sports variable. 
#### Describe these variables in your dataset. Drop observations that have missing value for either.

options(na.action = 'na.pass')
m <- model.matrix(~ -1 + deceased + female + age + eduyears_mod + income10g + sports + factor(sports), data = dt)
dt <- data.table(m)
options(na.action = 'na.omit')
dt <- na.omit(dt)

dt[, gender := ifelse(female == 1, "female", "male")]
freq(dt$deceased, plot = FALSE)
freq(dt$female, plot = FALSE)

descr::CrossTable(dt$deceased, dt$female, digits = 2, prop.r = FALSE, prop.c = FALSE, prop.t = FALSE, prop.chisq = FALSE)

ggplot(dt, aes(age)) + geom_histogram(binwidth = 5) + 
  geom_vline(data = dt[, .(age = mean(age)), by = gender], aes(xintercept = age), color = "red") +
  geom_vline(data = dt[, .(age = median(age)), by = gender], aes(xintercept = age), color = "blue") + 
  labs(x = "Age (years)") +
  facet_grid(~ gender)
##lognormal distribution -- consider transformation?

ggplot(dt, aes(eduyears_mod)) + geom_histogram(binwidth = 1) +  
  geom_vline(data = dt[, .(eduyears_mod = mean(eduyears_mod)), by = gender], aes(xintercept = eduyears_mod), color = "red") +
  geom_vline(data = dt[, .(eduyears_mod = median(eduyears_mod)), by = gender], aes(xintercept = eduyears_mod), color = "blue") + 
  labs(x = "Years of education") + 
  facet_grid(~ gender)
## normal distribution

## both similar for female / male. test means?

##heatmap on age vs edu
dt[, age_cat := cut(age, breaks = 5 * (10:16), include.lowest = TRUE)]
dt[, edu_cat := cut(eduyears_mod, breaks = 2 * (0:10), include.lowest = TRUE)]

dt_heatmap1 <- dt[, .(cnt_cat = .N), by = c("female", "age_cat", "edu_cat")]
dt_temp <- dt[, .(cnt_gender = .N), by = female]

dt_heatmap1 <- merge(dt_heatmap1, dt_temp, all.x=TRUE)

dt_heatmap1[, ratio_cat := cnt_cat / cnt_gender] 

ggplot(data = dt_heatmap1, aes(x = age_cat, y = edu_cat)) +
  geom_tile(aes(fill = ratio_cat), colour = "white") + 
  scale_fill_gradient(low = "white", high = "steelblue") + 
  facet_wrap(~ female)

dt[eduyears_mod == 0, ]

round(cor(dt[, c("age", "eduyears_mod", "income10g", "sports")]), 2)
## we don't have enough evidence to drop
## age-edu negative: expected
## edu-income positive
## sports - careful to interpret. with age people sport less, but more with more edu. we can build later x-x regressions if needed

##heatmap on age vs income
dt_heatmap2 <- dt[, .(cnt_cat = .N), by = c("female", "age_cat", "income10g")]

dt_heatmap2 <- merge(dt_heatmap2, dt_temp, all.x=TRUE)
dt_temp <- NULL

dt_heatmap2[, ratio_cat := cnt_cat / cnt_gender] 

ggplot(data = dt_heatmap2, aes(x = age_cat, y = income10g)) +
  geom_tile(aes(fill = ratio_cat), colour = "white") + 
  scale_fill_gradient(low = "white", high = "steelblue") + 
  facet_grid(~ female) 
## income gradually decreasing with age (most likely people retire / work less)

ggplot(data = dt, aes(x = age_cat, y = income10g)) + geom_boxplot()
##might not want to include, but it re-affirms the above

#### Estimate a linear probability model (LPM) of mortality on sports. Report and interpret the results. 
lpm <- lm(deceased ~ `factor(sports)1` + `factor(sports)2` + `factor(sports)3`,data = dt)
dt$deceased_pred <- predict.lm(lpm)

sum(dt$deceased) / dt[,.N]
## The unconditional probability of someone dieing in the six years, based on the dataset is 5.7%
summary(lpm)
lpm$coefficients
## People who "hardly ever, or never" do sports die within 6 years with 9.2% chance
## People who do sports "one to three times a month" die with 4.2% less chance compared to people in the 4th (previous) group. They die with a 5% likeliness
## Peope who exercise even more die with 5.9% less chance compared to people in the first category (there were no significant difference between group 1 and 2). They die with a 3.3% likelihood
## Based on the p values, all coefficients are significant on the usual levels

## Not suprisingly, people who do more sports are a lot less likely to die based on the dataset.
## Just based on this we cannot say however that doing sports make them "healthier" (clarification: but I do consider not dying healthier).
## We *only* know that there is a correlation in the dataset.
## However, it can be that even if the correlation is present in the general population as well, there is no causal relationship.
## Example: it could be that healthier people (who die less likely) are fit to more sports in general. Or that older people do less sport on average.

## We are not interpreting the R2 for the model.

##stargazer(lpm, type = "html")


#### 3. If you are interested in the causal effect of doing sports on mortality, 
#### would you want to control for some of the other variables in the dataset to get closer to the causal effect you are after? 
#### Would that controlling get you the causal effect you are after? 

## TODO

#### 4. Control for those variables in another LPM, interpret its results on sports, 
#### and compare those to the previous regression estimates. Discuss the differences and similarities. 
names(dt)

lpm2 <- lm(deceased ~ `factor(sports)1` + `factor(sports)2` + `factor(sports)3` + age,data = dt)
summary(lpm2)

## negative intercept -- problem with linear model. transfor age variable (given the difference for death of children vs adults, it only makes sense not to extrapolate)
dt[, age_diff := age - 50]
lpm2.1 <- lm(deceased ~ `factor(sports)1` + `factor(sports)2` + `factor(sports)3` + age_diff,data = dt)
summary(lpm2.1) 
lpm$coefficients
lpm2$coefficients
lpm2.1$coefficients

lpm3 <- lm(deceased ~ `factor(sports)1` + `factor(sports)2` + `factor(sports)3` + age_diff + female, data = dt)
summary(lpm3)
lpm3$coefficients
lpm3.1 <- lm(deceased ~ `factor(sports)1` + `factor(sports)2` + `factor(sports)3` + age_diff + female + (age_diff * female), data = dt)
summary(lpm3.1)
lpm3.1$coefficients

lpm4 <- lm(deceased ~ `factor(sports)1` + `factor(sports)2` + `factor(sports)3` + 
             age_diff + female + income10g, data = dt)
summary(lpm4)
lpm4$coefficients

lpm5 <- lm(deceased ~ `factor(sports)1` + `factor(sports)2` + `factor(sports)3` + 
             age_diff + female + eduyears_mod, data = dt)
summary(lpm5)
lpm5$coefficients
lpm2.1$coefficients

lpm6 <- lm(deceased ~ `factor(sports)1` + `factor(sports)2` + `factor(sports)3` + 
             age_diff + female + income10g, data = dt)
summary(lpm6)
lpm6$coefficients
lpm$coefficients
stargazer(lpm6, type = "html", out = "xd.doc")
## basically: as soon as we control for age, we almost get the same results (argue with CI overlap? for simple model)


#### 5. Re-do exercises 3 & 5 using logit. 
#### Calculate and interpret the marginal differences of the sports variables. 
#### Discuss the differences and similarities to the LPM results. 

logit <- glm(deceased ~ `factor(sports)1` + `factor(sports)2` + `factor(sports)3`, data = dt, family = "binomial")
summary(logit)


logit_marg <- logitmfx(formula = deceased ~ `factor(sports)1` + `factor(sports)2` + `factor(sports)3`, data = dt, atmean=FALSE)
lpm$coefficients

logit2 <- glm(deceased ~ `factor(sports)1` + `factor(sports)2` + `factor(sports)3` + age_diff, data = dt, family = "binomial")
summary(logit2)

logit2_marg <- logitmfx(formula = deceased ~ `factor(sports)1` + `factor(sports)2` + `factor(sports)3` + age_diff, data = dt, atmean=FALSE)
lpm2.1$coefficients
logit2_marg$mfxest

dt$deceased_pred_lpm2.1 <- predict.lm(lpm2.1)
dt$deceased_pred_logit2 <- predict.glm(logit2, type="response")

ggplot(data = dt, aes(x = deceased_pred_lpm2.1, y = deceased_pred_logit2)) + geom_point()

logit <- glm(deceased ~ `factor(sports)1` + `factor(sports)2` + `factor(sports)3`, data = dt, family = "binomial")
logit_marg <- logitmfx(formula = deceased ~ `factor(sports)1` + `factor(sports)2` + `factor(sports)3`, data = dt, atmean=FALSE)

data.table(logit2_marg$mfxest)
