# The purpose of the this work is to analyze the ToothGrowth data set by comparing the guinea tooth growth
# by supplement and dose. Firstly, I will do exploratory data analysis on the data set.

library(UsingR)
library(tidyverse)
data(ToothGrowth)
t <- ToothGrowth
summary(t)

# Graph
t_summary <- t %>% group_by(supp, dose) %>%
summarise(mean_len = mean(len), .groups = "drop")

ggplot(t_summary, aes(x = dose, y = mean_len, color = supp)) + geom_line(size = 1) +
geom_point(size = 3) + scale_y_continuous(breaks = seq(0, max(t_summary$mean_len) + 5, by = 2)) +
labs(title = "Mean Tooth Length by Dose for Each Supplement", 
     x = "Dose (mg/day)", y = "Mean Tooth Length", color = "Supplement") +
theme_minimal()

# COnfidence Interval
## Hypothesis 1
hypoth1<-t.test(len ~ supp, data = t)
round(hypoth1$conf.int, 3)

### P.value
round(hypoth1$p.value, 3)

## Hypothesis 2
hypoth2<-t.test(len ~ supp, data = subset(t, dose == 0.5))
round(hypoth2$conf.int, 3)

### P.Value
round(hypoth2$p.value, 3)

## Hypotheis 3
hypoth3<-t.test(len ~ supp, data = subset(t, dose == 1))
round(hypoth3$conf.int, 3)

### P.Values
round(hypoth3$p.value, 3)

## Hypothesis 4
hypoth4 <-t.test(len ~ supp, data = subset(t, dose == 2))
round(hypoth4$conf.int, 3)

### P.Value
round(hypoth4$p.value, 3)

