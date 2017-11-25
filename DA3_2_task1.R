library(ggplot2)
library(data.table)
library(wbstats)
library(tidyverse)
library(lspline)


multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


x <- wb(country = "countries_only", indicator = c("NY.GDP.PCAP.PP.KD", "SP.DYN.LE00.IN", "SP.POP.TOTL") , startdate = 2015, enddate = 2015)
countries <- data.table(x)


dt <- countries[, c('value', 'country', 'indicator')]
dt <- spread(dt, key = 'indicator', value = 'value', fill = NA)  

#saving string names for later use (charts, ...)
s_country = colnames(dt)[1]
s_gdp = "GDP / capita, $" ##colnames(dt)[2]
s_life_exp = "Life exp. at birth, yrs" ##colnames(dt)[3]
s_pop = "Population, million" ##colnames(dt)[4]
s_lnpop = "log(Population, million)"
s_lngdp = "log(GDP per capita, $)"

setnames(dt, old = colnames(dt), new = c('country', 'gdp', 'life_exp', 'pop'))

dt <- dt[is.na(gdp) == FALSE, ]
dt <- dt[is.na(pop) == FALSE, ]
dt <- dt[is.na(life_exp) == FALSE, ]

dt[, pop := pop / 1000000]
dt[, lngdp := log(gdp)]

write.csv(x = dt, file = 'WD_Data_Filtered.csv')


p1 <- ggplot(data = dt, aes(gdp)) + geom_histogram(binwidth = 2500, fill = 'orange') + 
  scale_x_continuous(labels = scales::comma) + 
  labs(x = s_gdp) +
  geom_text(aes(x = median(dt$gdp), y = 35), label ='median', color = 'blue', angle = 90, vjust = 1.0) +
  geom_vline(xintercept = median(dt$gdp), color = 'blue') +
  geom_text(aes(x = mean(dt$gdp), y = 35), label ='mean', color = 'blue', angle = 90, vjust = 1.0) +
  geom_vline(xintercept = mean(dt$gdp), color = 'blue')


p2 <- ggplot(data = dt, aes(lngdp)) + geom_histogram(binwidth = .25, fill = 'orange') +
  labs(x = s_lngdp) +
  geom_text(aes(x = median(dt$lngdp), y = 12.5), label ='median', color = 'blue', angle = 90, vjust = 1.0) +
  geom_vline(xintercept = median(dt$lngdp), color = 'blue') +
  geom_text(aes(x = mean(dt$lngdp), y = 12.5), label ='mean', color = 'blue', angle = 90, vjust = 1.0) +
  geom_vline(xintercept = mean(dt$lngdp), color = 'blue')

multiplot(p1, p2, cols = 2)

p3 <- ggplot(data = dt, aes(life_exp)) + geom_histogram(binwidth = 5, fill = 'orange') + 
  labs(x = s_life_exp) +
  geom_text(aes(x = median(dt$life_exp), y = 45), label ='median', color = 'blue', angle = 90, vjust = 1.0) +
  geom_vline(xintercept = median(dt$life_exp), color = 'blue') +
  geom_text(aes(x = mean(dt$life_exp), y = 45), label ='mean', color = 'blue', angle = 90, vjust = 1.0) +
  geom_vline(xintercept = mean(dt$life_exp), color = 'blue')

p4 <- ggplot(data = dt, aes(pop)) + geom_histogram(binwidth = 25, fill = 'orange') + 
  scale_x_continuous(labels = scales::comma) + 
  labs(x = s_pop) +
  geom_text(aes(x = median(dt$pop), y = 90), label ='median', color = 'blue', angle = 90, vjust = 1.0) +
  geom_vline(xintercept = median(dt$pop), color = 'blue') +
  geom_text(aes(x = mean(dt$pop), y = 90), label ='mean', color = 'blue', angle = 90, vjust = 1.0) +
  geom_vline(xintercept = mean(dt$pop), color = 'blue')

p5 <- ggplot(data = dt, aes(log(pop))) + geom_histogram(fill = 'orange') + 
  scale_x_continuous(labels = scales::comma) + 
  labs(x = paste("log(", s_pop, ")", sep = "")) +
  geom_text(aes(x = median(log(dt$pop)), y = 15), label ='median', color = 'blue', angle = 90, vjust = 1.0) +
  geom_vline(xintercept = median(log(dt$pop)), color = 'blue') +
  geom_text(aes(x = mean(log(dt$pop)), y = 15), label ='mean', color = 'blue', angle = 90, vjust = 1.0) +
  geom_vline(xintercept = mean(log(dt$pop)), color = 'blue')

multiplot(p3, p4, p5, cols = 3)

gp <- geom_point(size = log(dt$pop) / 1.5, color = 'orange')
gs <- geom_smooth(method = 'loess')

p6 <- ggplot(data = dt, aes(lngdp, life_exp)) + gp + 
gs + 
labs(x = s_lngdp, y = s_life_exp)

p8 <- ggplot(data = dt, aes(lngdp, pop)) + geom_point(size = log(dt$pop) / 1.5, color = 'orange') + 
geom_smooth(method = 'loess') + 
labs(x = s_lngdp, y = s_pop)

p7 <- ggplot(data = dt, aes(life_exp, pop)) +  gp + 
gs +
labs(x = s_life_exp, y = s_pop)

p9 <- ggplot(data = dt, aes(lngdp, log(pop))) + gp + 
gs + 
labs(x = s_lngdp, y = s_lnpop)

multiplot(p6, p7, p8, p9, cols = 2)

##lowess
lowess_model <- loess(life_exp ~ lngdp, data = dt)
dt$lowess_pred <- predict(lowess_model)
##summary(lowess_model)

r2_lowess <- var(dt$lowess_pred) / var(dt$life_exp)

##linear regression
lm_ln_model <- lm(life_exp ~ lngdp, data = dt)
dt$lm_ln_pred <- predict(lm_ln_model)

r2_lm_ln <- summary(lm_ln_model)$r.squared
##summary(lm_ln_model)
formula_lm_ln = paste(s_life_exp,"~", round(coefficients(lm_ln_model)[1], 2), 
"+", round(coefficients(lm_ln_model)[2], 2), "*", s_lngdp, sep = " ")

##significant on all usual confidence levels!

lm_model <- lm(life_exp ~ gdp, data = dt)
dt$lm_pred <- predict(lm_model)

r2_lm <- summary(lm_model)$r.squared
##summary(lm_model)
formula_lm = paste(s_life_exp,"~", round(coefficients(lm_model)[1], 2), 
"+", format(coefficients(lm_model)[2], digits = 2), "*", s_gdp, sep = " ")
##significant on all usual confidence levels!

##spline
knot = 10.5
spline_model <- lm(life_exp ~ lspline(lngdp, knot), data = dt)
dt$spline_pred <- predict(spline_model)

r2_spline <- summary(spline_model)$r.squared
##summary(spline_model)
formula_spline = paste(s_life_exp,"~", round(coefficients(spline_model)[1], 2), 
"+\n    ", round(coefficients(spline_model)[2], 2), "*", s_lngdp, "(<10.5)", 
"+\n    ", round(coefficients(spline_model)[3], 2), "*", s_lngdp, "(>10.5)", sep = " ")


## polynomial
dt$lngdp_sq <- dt$lngdp ^ 2
dt$lngdp_cub <- dt$lngdp ^ 3

lm_sq_model <- lm(life_exp ~ lngdp + lngdp_sq, data = dt)
dt$lm_sq_pred <- predict(lm_sq_model)

r2_lm_sq <- summary(lm_sq_model)$r.squared
##summary(lm_sq_model)
formula_lm_sq = paste(s_life_exp,"~", round(coefficients(lm_sq_model)[1], 2), 
"+\n    ", round(coefficients(lm_sq_model)[2], 2), "*", s_lngdp, 
"+\n    ", round(coefficients(lm_sq_model)[3], 2), "*", s_lngdp, sep = " ")

lm_cub_model <- lm(life_exp ~ lngdp + lngdp_sq + lngdp_cub, data = dt)
dt$lm_cub_pred <- predict(lm_cub_model)

r2_lm_cub <- summary(lm_cub_model)$r.squared
##summary(lm_cub_model)
formula_lm_cub = paste(s_life_exp,"~", round(coefficients(lm_cub_model)[1], 2), 
"+\n    ", round(coefficients(lm_cub_model)[2], 2), "*", s_lngdp, 
"+\n    ", round(coefficients(lm_cub_model)[3], 2), "*", s_lngdp, 
"+\n    ", round(coefficients(lm_cub_model)[4], 2), "*", s_lngdp, sep = " ")



l <- labs(x = s_gdp, y = s_life_exp)
s_y <- scale_y_continuous(limits = c(45, 90))
s_x <-scale_x_continuous(expand = c(0, 0), limits = c(0, 13500))

p_lowess <- ggplot(data = dt, aes(gdp, life_exp)) + gp + l + s_y + s_x +
geom_line(data = dt, aes(gdp, lowess_pred), color = 'blue') +
ggtitle("Model: Lowess") +
geom_text(aes(x = 200, y = 86), label = paste("R^2 =", round(r2_lowess, 4), sep=" "), size = 4, hjust = 0)

p_lm <- ggplot(data = dt, aes(gdp, life_exp)) + gp + l + s_y + s_x +
geom_line(data = dt, aes(gdp, lm_pred), color = 'blue') +
ggtitle("Model: Linear Regression") +
geom_text(aes(x = 200, y = 89), label = formula_lm, size = 4, hjust = 0) +
geom_text(aes(x = 200, y = 86), label = paste("R^2 =", round(r2_lm, 4), sep =" ") , size = 4, hjust = 0)

p_lm_ln <- ggplot(data = dt, aes(gdp, life_exp)) + gp +  l + s_y + s_x +
geom_line(data = dt, aes(gdp, lm_ln_pred), color = 'blue') +
ggtitle("Model: Log - Level Linear Regression") +
geom_text(aes(x = 200, y = 89), label = formula_lm_ln, size = 4, hjust = 0) +
geom_text(aes(x = 200, y = 86), label = paste("R^2 =", round(r2_lm_ln, 4), sep =" "), size = 4, hjust = 0)

p_spline <- ggplot(data = dt, aes(gdp, life_exp)) + gp +  l + s_y + s_x +
geom_line(data = dt, aes(gdp, spline_pred), color = 'blue', size = .7) +
geom_vline(xintercept = exp(knot), color = 'red', size = .7) +
ggtitle("Model: Spline w/ 1 knot") +
geom_text(aes(x = 200, y = 87), label = formula_spline, size = 4, hjust = 0) +
geom_text(aes(x = 200, y = 82), label = paste("R^2 =", round(r2_spline, 4), sep =" "), size = 4, hjust = 0)

p_lm_sq <- ggplot(data = dt, aes(gdp, life_exp)) + gp + l + s_y + s_x +
geom_line(data = dt, aes(gdp, lm_sq_pred), color = 'blue', size = .7) +
ggtitle("Model: Linear Regression w/ square polinom") +
geom_text(aes(x = 200, y = 87), label = formula_lm_sq, size = 4, hjust = 0) +
geom_text(aes(x = 200, y = 82), label = paste("R^2 =", round(r2_lm_sq, 4), sep =" "), size = 4, hjust = 0)

p_lm_cub <- ggplot(data = dt, aes(gdp, life_exp)) + gp + l + s_y + s_x +
geom_line(data = dt, aes(gdp, lm_cub_pred), color = 'blue', size = .7) +
ggtitle("Model: Linear Regression w/ cubic polinom") +
geom_text(aes(x = 200, y = 85), label = formula_lm_cub, size = 4, hjust = 0) +
geom_text(aes(x = 200, y = 79), label = paste("R^2 =", round(r2_lm_cub, 4), sep =" "), size = 4, hjust = 0)

multiplot(p_lowess, p_lm, p_spline, p_lm_ln, p_lm_sq, p_lm_cub, cols = 2)

## TODO Report the coefficient estimates as well as their confidence interval, interpret and visualize the results. 


dt_summaries <- data.table(all.vars(as.formula(lm_ln_model)))
dt_summaries <- cbind(dt_summaries, data.table(summary(lm_ln_model)$coefficient))
dt_summaries <- cbind(dt_summaries, "Level - Log Linear Regression")

temp <- data.table(all.vars(as.formula(lm_model)))
temp <- cbind(temp, data.table(summary(lm_model)$coefficients))
temp <- cbind(temp, "Linear Regression")

dt_summaries <- rbind(dt_summaries, temp)

temp <- data.table(all.vars(as.formula(spline_model)))
temp <- cbind(temp, data.table(summary(spline_model)$coefficients))
temp <- cbind(temp, "Spline w/ 1 knot")

dt_summaries <- rbind(dt_summaries, temp)

temp <- data.table(all.vars(as.formula(lm_sq_model)))
temp <- cbind(temp, data.table(summary(lm_sq_model)$coefficients))
temp <- cbind(temp, "Linear Regression w/ square polinom")

dt_summaries <- rbind(dt_summaries, temp)

temp <- data.table(all.vars(as.formula(lm_cub_model)))
temp <- cbind(temp, data.table(summary(lm_cub_model)$coefficients))
temp <- cbind(temp, "Linear Regression w/ cubic polinom")

dt_summaries <- rbind(dt_summaries, temp)

setnames(dt_summaries, c('x', 'beta', 'se', 'model'))
dt_summaries[, ci_low := beta - se]
dt_summaries[, ci_high := beta + se]
dt_summaries[x != "life_exp", ]
dt_summaries[, model := factor(model, 
                               levels = c("Level - Log Linear Regression", 
                                          "Spline w/ 1 knot", 
                                          "Linear Regression w/ square polinom", 
                                          "Linear Regression w/ cubic polinom")) ]

ggplot(dt_summaries[x == "lngdp", ], aes(x = x, y = beta, ymin = ci_low, ymax = ci_high)) + 
  geom_pointrange() + 
  facet_grid(~model, scales="free")

dt_summaries
#### 3. Estimate a weighted regression (weight=population). Compare results to what we saw in class. 

lm_weighted_model <- lm(life_exp ~ lngdp, weights = pop, data = dt)
dt$lm_weighted_pred <- predict(lm_weighted_model)
r2_lm_weighted <- summary(lm_weighted_model)$r.squared


ggplot(data = dt, aes(gdp, life_exp))  + gp + l + s_y + s_x +
geom_line(data = dt, aes(gdp, lm_weighted_pred), color = 'blue', size = .7) +
ggtitle("Model: Population weighted linear regression") +
geom_text(aes(x = 6000, y = 49), label = "lngdp ~ dfs", size = 4, hjust = 0) +
geom_text(aes(x = 6000, y = 46), label = paste("R^2 =", round(r2_lm_weighted, 4), sep =" "), size = 4, hjust = 0)
```


## TODO explain
## TODO update formula