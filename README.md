## Linear Regression in R
In this I created different models in R studio to explore the influence of GDP, social support, and other factors on happiness using linear regression and multiple linear regression. Analysis of the project's dataset reveals strong correlations between economic prosperity, social ties, health, and happiness levels, while also examining the nuanced role of corruption perception

```
rm(list = ls())
```
```
Happiness <- read.csv('Case2.csv', stringsAsFactors = T)
```


```{r}
str(Happiness)
```

```{r}
library(tidyverse)
library(lmtest)
library(mltools)
library(semTools)
library(ggfortify)
library(broom)
library(car)
```

## GDP per capita Linear Model

```{r }
lmGDP <- lm(Happiness ~ GDP.per.capita, data = Happiness)
summary(lmGDP)
```

explanation: Multiple R-squared:  0.6303,	Adjusted R-squared:  0.6278 -- GDP explains 63% of the variable Happiness
p-value: < 2.2e-16 -- Model significant

```{r}
library(ggplot2)

ggplot(Happiness, aes(x = GDP.per.capita, y = Happiness)) +
  geom_point(col = 'blue') +
  stat_smooth(method = 'lm', color = 'red') +
  labs(title = 'GDP Per Capita on Overall Happiness Score') +
  theme(panel.grid.major = element_blank(),  # Removes the major grid lines
        panel.grid.minor = element_blank())  # Removes the minor grid lines

```
![powerbi-1](https://github.com/dbenjamin9/R-studio-test1/blob/main/GDP%20PER%20CAPITA%20LM%20Plot1.png)

Looking at the graph we can see that GDP per capita and Happiness have positive inear Relationship

```{r cars}
autoplot(lmGDP)
```

![Autoplot](https://github.com/dbenjamin9/R-studio-test1/blob/main/image2r.png)

Looking at our autoplots we can see that Observations 12, 148, and 156 may be skewing our data
```{r}
bptest(lmGDP)
```
homoskedastic is good-- p-value > 0.05 -- assumption of normally distributed errors

```{r}
skew(lmGDP$residuals)
```

```{r}
hist(lmGDP$residuals)
```
![Hist Plot](https://github.com/dbenjamin9/R-studio-test1/blob/main/histlmgdp.png)
```{r}
mean(lmGDP$residuals) 
mse(lmGDP$fitted.values, Happiness$Happiness) 
coef(lmGDP)
```

Assumptions: The four linear assumptions are checking for linearity, the normality of errors, the independence of errors, and homoskedasticity.
After testing above, the ggplot of the data appears linear. The errors have a p-value > 0.05 and a mean of the residuals close to one assumes normally distributed errors. 
Lastly, the bptest resulted in a p-value > 0.05, indicating a normal distribution (homoskedasticity).Therefore, none of our assumptions have been violated.

Implications: This data is useful to us to understand whether happiness is affected as the GDP and economic status of a country improves. The relationship between happiness and GDP is positive linear, meaning that as GDP of a country increases, overall happiness score increases. For every 1 unit increase in GDP, happiness score increases by 2.22. This assumes that more economically sound countries are happier.

## Social support Linear Model


```{r }
lmSocial <- lm(Happiness ~ Social.support, data = Happiness)
summary(lmSocial) 
```

```{r}
ggplot(Happiness, aes(x = Social.support, y = Happiness)) +
  geom_point(col = 'blue') + stat_smooth(method = 'lm', color = 'red') +
  labs(title = 'Social Support on Overall Happiness Score') ##Linear Relationship
```
![](https://github.com/dbenjamin9/R-studio-test1/blob/main/social%20support%20on%20onverall%20Happiness%20SCORE.png)
```{r}
autoplot(lmSocial) ##Observations 102, 148, 155, and maybe 151 can be skewing the data
```
![Autoplot LM SOCIAL](https://github.com/dbenjamin9/R-studio-test1/blob/main/autoplot(lmSocial).png)
```{r}
bptest(lmSocial) ##homoskedastic (good)-- p-value > 0.05 -- assumption of normally distributed errors
```

```{r}
skew(lmSocial$residuals) ###p-value > .05 - indicative of normal distribution
```
```{r}
hist(lmSocial$residuals) ##look pretty normal
```
![HistLMSOCIAL](https://github.com/dbenjamin9/R-studio-test1/blob/main/hist%20lmSocial%24residuals.png)
```{r}
mean(lmSocial$residuals) ##mean close to 0 indicates normally distributed
```

```{r}
mse(lmSocial$fitted.values, Happiness$Happiness) #0.487736 -- worse (larger than GDP) -- errors are larger
  ##The models errors are 0.4877 squared units on average
coef(lmSocial) ##Regression Equation: 1.912430 + 2.890987 * Social.support
```

Asumptions: The four linear assumptions are checking for linearity, the normality of errors, the independence of errors, and homoskedasticity. After testing above, the ggplot of the data appears linear. The errors have a p-value > 0.05 and a mean of the residuals close to one assumes normally distributed errors.
Lastly, the bptest resulted in a p-value > 0.05, indicating a normal distribution (homoskedasticity).Therefore, none of our assumptions have been violated.

Implications: This data is useful for us to determine if Social Support impacts Happiness. This relationship is positive linear, concluding that as an increase in social support from a country increases, happiness increases. For every one unit increase in social support, happiness score increases by 2.89. This assumes that citizens of a country who feel more supported are happier.

## Healthy life expectancy Linear Model

```{r}
lmHealth <- lm(Happiness ~ Healthy.life.expectancy, data = Happiness)
summary(lmHealth)
```


```{r}
ggplot(Happiness, aes(x = Healthy.life.expectancy, y = Happiness)) +
  geom_point(col = 'blue') + stat_smooth(method = 'lm', color = 'red') +
  labs(title = 'Healthy Life Expectancy on Overall Happiness Score')
```
![Healthy Life Expectancy on Overall Happiness Score](https://github.com/dbenjamin9/R-studio-test1/blob/main/Healthy%20Life%20Expectancy%20on%20Overall%20Happiness%20Score.png)
```{r}
autoplot(lmHealth) ##86, 152, and maybe 99 may be skewing the data
```
![lmHealth](https://github.com/dbenjamin9/R-studio-test1/blob/main/autoplot(lmHealth).png)
```{r}
bptest(lmHealth) ##homoskedastic (good)-- p-value > 0.05 -- assumption of normally distributed errors
```

```{r}
skew(lmHealth$residuals) ###p-value > .05 - indicative of normal distribution
```
```{r}
hist(lmHealth$residuals)
```
![](https://github.com/dbenjamin9/R-studio-test1/blob/main/hist(lmHealth%24residuals).png)

```{r}
mean(lmHealth$residuals) ##mean close to 0 indicates normally distributed
mse(lmHealth$fitted.values, Happiness$Happiness) #0.4823205 -- worse (larger than GDP, slightly smaller than Social) errors are larger with this variable
  ##The models errors are 0.4823 squared units on average
coef(lmHealth) ##Regression Equation: 2.806832 + 3.585367 * Healthly.life.expectancy
```


Asumptions: The four linear assumptions are checking for linearity, the normality of errors, the independence of errors, and homoskedasticity. After testing above, the ggplot of the data appears linear. The errors have a p-value > 0.05 and a mean of the residuals close to one assumes normally distributed errors.
Lastly, the bptest resulted in a p-value > 0.05, indicating a normal distribution (homoskedasticity).Therefore, none of our assumptions have been violated.

Implications: This data is useful to determine if life expectancy length of the citizens of a county affects the overall happiness. Life expectancy and happiness assume a positive linear relationship.For every one increase in healthy life expectancy, happiness score increases by We can conclude that as life expectancy of citizens within a country increases, overall happiness score increases.

## Perception of corruption Linear Model

```{r}
lmPerception <- lm(Happiness ~ Perceptions.of.corruption, data = Happiness)
summary(lmPerception) ##Multiple R-squared:  0.1487,	Adjusted R-squared:  0.1432 -- The variable Perception explains 14% of Happiness
##p-value: 6.654e-07 -- Model is significant
```

```{r}
ggplot(Happiness, aes(x = Perceptions.of.corruption, y = Happiness)) +
  geom_point(col = 'blue') + stat_smooth(method = 'lm', color = 'red') +
  labs(title = 'Perception of Corruption on Overall Happiness Score')
```
![](https://github.com/dbenjamin9/R-studio-test1/blob/main/P1erception%20of%20Corruption%20on%20Overall%20Happiness%20Score.png)
```{r}
autoplot(lmPerception) ##The observations 152, 153, 156 are skewing the model
```
![](https://github.com/dbenjamin9/R-studio-test1/blob/main/autoplot(lmPerception)2.png)
```{r}
bptest(lmPerception) ##heteroskedastic (bad)-- p-value < 0.05 -- assumption of skewed errors
skew(lmPerception$residuals) ###p-value < .05 - indicative of skewed distribution
```

```{r}
hist(lmPerception$residuals) ##left skewed
```
![](https://github.com/dbenjamin9/R-studio-test1/blob/main/histlmPerception%24residuals.png)

```{r}
mean(lmPerception$residuals) ##mean close to 0 indicates normally distributed
mse(lmPerception$fitted.values, Happiness$Happiness) #1.048033 -- worst
```


```{r}
modelResults <- augment(lmPerception) %>%
  mutate(index = 1:n())
```

```{r}
ggplot(modelResults, aes(index, .std.resid)) + 
  geom_point(aes(color = ifelse(abs(.std.resid) > 3, 'Above Threshold', 'Below Threshold'))) +
  scale_color_manual(values = c('Above Threshold' = 'red', 'Below Threshold' = 'purple')) +
  guides(color = 'none')
```
![](https://github.com/dbenjamin9/R-studio-test1/blob/main/Above%20%20threshold%20ggplot.png)
```{r}
sum(abs(modelResults$.std.resid) > 3)
subset(modelResults$index, abs(modelResults$.std.resid) > 3)
```

Asumption of Model 1: The four linear assumptions are checking for linearity, the normality of errors, the independence of errors, and homoskedasticity.
After testing above, the ggplot of the data does not appear linear. The errors have a p-value < 0.05 indicating skew, but a mean of the residuals close to one assuming normally distributed errors.
Lastly, the bptest resulted in a p-value < 0.05, indicating a skew (heteroskedasticity). Therefore, one or more of our assumptions have been violated.

## New model with 152 removed

```{r}
Happiness <- Happiness[-c(152),]
```

```{r}
lmPerception <- lm(Happiness ~ Perceptions.of.corruption, data = Happiness)
summary(lmPerception)##Multiple R-squared:  0.1969,	Adjusted R-squared:  0.1916
  #Now perception.of.corruption explains 19% of happiness, not 14%
```

```{r}
autoplot(lmPerception) ##The observations 153, 156 are still slightly skewing the model
```
![](https://github.com/dbenjamin9/R-studio-test1/blob/main/autoplotlmPerception%20152%20removed.png)
```{r}
bptest(lmPerception) ##homoskedastic: p-value > 0.05 -- assumption of normality 
skew(lmPerception$residuals) ###p-value > .05 - indicative of normal
```

```{r}
hist(lmPerception$residuals) ##look pretty normal -- maybe a slight right skew
```
![](https://github.com/dbenjamin9/R-studio-test1/blob/main/hist1lmPerception%24residuals.png)
```{r}
mean(lmPerception$residuals) ##mean close to 0 indicates normally distributed
mse(lmPerception$fitted.values, Happiness$Happiness) #0.9727083
coef(lmPerception) ##Regression equation: 4.839868 + 5.343076 * Perception.of.corruption
```


Asumption of Model 2: The four linear assumptions are checking for linearity, the normality of errors, the independence of errors, and homoskedasticity. After removing observation 152 and testing above,the errors have a p-value > 0.05 and a mean of the residuals close to one assumes normally distributed errors.

Lastly, the bptest resulted in a p-value > 0.05, indicating a normal distribution (homoskedasticity).Therefore, none of our assumptions have been violated after removing the outlier 152. 

Implications: This data is helpful in determining if perception of corruption affects happiness.After running model 2 that has removed Rwanda which was skewing the data, we can determine that perception of corruption and happiness do not assume a linear relationship. 

Many of our assumptions were violated and the observations do not have a linear form in the ggplot.The data follows a more clustered approach, with most of the observations towards a low perception of corruption score and an average happiness score. The correlation between perception of corruption and happiness is not linear.

## Multiple LM on Happiness

```{r}
Happiness <- read.csv('Case2.csv')
```

```{r}
lmHappy <- lm(Happiness ~ GDP.per.capita + Social.support + Healthy.life.expectancy +
                Freedom.to.make.life.choices + Perceptions.of.corruption + Generosity, 
              data = Happiness)
summary(lmHappy) ##Model 1: Multiple R-squared:  0.7792,	Adjusted R-squared:  0.7703
```

```{r}
lmHappy <- lm(Happiness ~ GDP.per.capita + Social.support + Healthy.life.expectancy +
                Freedom.to.make.life.choices + Perceptions.of.corruption, 
              data = Happiness)
summary(lmHappy) ##Model 2: Removed Generosity: Multiple R-squared:  0.7777,	Adjusted R-squared:  0.7703 
```

```{r}
vif(lmHappy) ##None are above 10 so none are are high correlated to where they would be problematic
```

```{r}
hist(lmHappy$residuals) ##slightly left skewed
```
![](https://github.com/dbenjamin9/R-studio-test1/blob/main/slightly%20left%20skewed11.png)

```{r}
skew(lmHappy$residuals) ##heteroskedastic -- p-value < 0.05
bptest(lmHappy) ##heteroskedastic -- p-value < 0.05 -- skewed distributed errors
autoplot(lmHappy) ##observation 152,153,148, and maybe 34 seem to be outliers
```
![](https://github.com/dbenjamin9/R-studio-test1/blob/main/and%20maybe%2034%20seem%20to%20be%20outliers.png)
```{r}
plot(lmHappy, which = c(4)) ##shows cook's distance-in which 34,148,152 looks problematic
```
![](https://github.com/dbenjamin9/R-studio-test1/blob/main/plot(lmHappy%2C%20which%20%3D%20c(4)).png)
```{r}
modelResults <- augment(lmHappy) %>%
  mutate(index = 1:n())
```

```{r}
ggplot(modelResults, aes(index, .std.resid)) + 
  geom_point(aes(color = ifelse(abs(.std.resid) > 3, 'Above Threshold', 'Below Threshold'))) +
  scale_color_manual(values = c('Above Threshold' = 'red', 'Below Threshold' = 'blue')) +
  guides(color = 'none')
```
![](https://github.com/dbenjamin9/R-studio-test1/blob/main/Above%20Threshold%20black2.png)
```{r}
sum(abs(modelResults$.std.resid) > 3)
subset(modelResults$index, abs(modelResults$.std.resid) > 3)
Observations 148 (Botswana) and 152 (Rwanda) are above the desired threshold
```
## Predictions

```{r}
Predictions <- read.csv('Case2.csv')
Predictions <- select(Predictions, -Overall.rank, -Country.or.region, -Generosity)
```

```{r}
n <- length(Predictions$Happiness)
n1 <- 100
```

```{r}
set.seed(42)
```

```{r}
trainobs <- sample(1:n, n1)
```

```{r}
train <- Predictions[trainobs,]
test <- Predictions[-trainobs,]
```

```{r}
lmTrain <- lm(Happiness~., data = train)
summary(lmTrain) ##Multiple R-squared:  0.8141,	Adjusted R-squared:  0.8042
```

```{r}
predictions2 <- predict(lmTrain, newdata = test)
head(predictions2)
```

```{r}
mse(predictions2, test$Happiness) ##0.3706376 -- better than previous models -- model squared errors are lower
```
```{r}
cordata <- data.frame(cbind(test$Happiness, predictions2))
cor(cordata) ##0.827324 -- good (pretty good correlation -- 1 is perfect correlation)
```
