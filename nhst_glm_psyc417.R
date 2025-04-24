# NHST and GLM code
# PSYC417 

# load the required packages
library(ggplot2) # visualizations
library(jtools) # APA graphics
library(dplyr) # data wrangling

# read in injury data 
injuries <-read.csv("~/PSYC417-SP25/Data/injuries.csv")

# explore the data
summary(injuries) # descriptive statistics
str(injuries) # structure summary similar to global environment

# plot exhaustion by injuries
ggplot(data = injuries, aes(x = exhaust, y = injury)) + 
  geom_point(color = "red") +
  geom_smooth(method = "lm", se = FALSE) + # add line of best fit
  jtools::theme_apa() 
# looks to be small positive relationship 

# fit simple regression model with injury regressed on exhaustion
simp_reg_model <- glm(injury ~ exhaust, data = injuries)
summary(simp_reg_model) # obtain model estimates
anova(simp_reg_model) # obtain SSE information (ancillary)
# effect of exhaustion is statistically significant
# b0: when exhaustion was 0, injuries were expected to be at 1.535. Since exhaustion ranges from 1-7, this is not a meaningful interpretation
# b1: for each one-unit increase in exhaustion injuries increase by 0.130 units

# manually conduct hypothesis test
test_stat = 0.13001/0.05626 # generate test statistic 
p_val = (1-pt(test_stat, 298))*2 # for two-tailed, remove *2 for one-tailed

##### check assumptions
# normality of residuals
hist(resid(simp_reg_model)) # looks normal, good! 
qqnorm(resid(simp_reg_model)) # looks like a straight enough line, good! 

# linearity
plot(simp_reg_model, 1)

# homogeneity of variance
plot(simp_reg_model, 3)

##### end check assumptions

# plot exhaustion by injuries
ggplot(data = injuries, aes(x = exhaust, y = injury)) + 
  geom_point(color = "navy") + # make scatterplot
  geom_abline(intercept = 1.535, slope = 0.130) + # add line of best fit manually
  geom_vline(xintercept = 3, color = "firebrick", linetype = "dashed") + # one unit increase in x
  geom_vline(xintercept = 4, color = "firebrick", linetype = "dashed") + # one unit increase in x
  geom_hline(yintercept = 1.925, color = "firebrick", linetype = "dashed") + # predicted value of y at x = 3
  geom_hline(yintercept = 2.095, color = "firebrick", linetype = "dashed") + # predicted value of y at x = 4
  geom_hline(yintercept = 1.535, color = "forestgreen") + # line for y-intercept
  geom_vline(xintercept = 0, color = "forestgreen") + # line for y-intercept
  xlim(c(0,6.5)) + # start graph at 0 
  jtools::theme_apa() # make APA format

# if X is dichotomous and coded as 0 and 1, the estimates have a nice interpretation
summary(glm(injury~sex, data = injuries))

# show that all models are the same with the same data
set.seed(40000) # to reproduce results
x = c(rep(0,100), rep(1,100)) # x dichotomous
y = x*.3 + rnorm(200) # y correlated with x
data = data.frame(x = x, y =y) # put x and y into a data frame

# GLM models 
t.test(data$y ~ as.factor(x), var.equal = TRUE) # t test
summary(aov(data$y ~ as.factor(x))) # as an ANOVA
summary(glm(y ~ x, data = data)) # simple regression
summary(glm(scale(y) ~ scale(x), data = data)) # standardized simple regression
cor.test(data$y, data$x) # correlation model

mult_reg_model <- glm(injury ~ exhaust + safety, data = injuries)
summary(mult_reg_model) # obtain model estimates
anova(mult_reg_model) # obtain SSE information (ancillary)
# effect of exhaustion is no longer significant but the effect of safety shortcuts is. 
# b0: when exhaustion and safety shortcuts were 0, injuries were expected to be at 0.78. Since exhaustion and safety both range from 1-7, this is not a meaningful interpretation
# b1: for each one-unit increase in exhastion, injuries increased by 0.055 units assuming they took the same safety shortcuts. 
# b2: For each one-unit increase in safety shortcuts, injuries increase by 0.246 units holding exhaustion constant. 
# safety was either confounding or mediating the relatinoship between exhaustion and injuries

# you can make these models quite complex
big_model <- glm(injury ~ exhaust + safety + sex + tenure + injuryb, data = injuries)
summary(big_model)
anova(big_model)
# b0: expected value of Y when all predictors are 0
# b(k where k is kth predictor) - expected change in Y with a one-unit increase in x_k controlling for the other k-1 variables in the model



### end of script