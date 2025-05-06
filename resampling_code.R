# resampling methods code
# PSYC417

# load required packages
library(ggplot2) # create visualizations 
library(jtools) # make figures APA format
library(pwr) # calculate power

# set parameters
set.seed(1000) # makes results reproducible since we're generating random numbers
alpha = .05 # set significance threshold
reps = 1000 # number of reps
n = 175 # sample size
res <- rep(NA, reps) # empty results vector
eff_size = .2

# show Type I error, Power, and other ideas
for(i in 1:reps){
  x = rnorm(n) # generate predictor variable
  y = x*eff_size + rnorm(n) # generate outcome variable that IS correlated to predictor, change eff_size to 0 to illustrate Type I error (since X and Y would not be related)
  p_val = summary(lm(y ~ x))$coefficients[2,4] # obtain p-values
  res[i] = as.numeric(p_val <= alpha) # count number of significant p-values
}
sum(res)/reps # type I error or power depending on effect size, power if nonzero, Type I error if 0 
pwr.r.test(n = n, r = eff_size, sig.level = alpha) # actual power, should be closer to above number

# manually calculate t- and p-values
est = 0.15361 # estimate IF you generate everything the same way 
se = 0.07870 # SE IF you generate everything the same way 
t_val = est/se; t_val # t-value IF you generate everything the same way 
p_val = 2*pt(t_val,(n-2), lower = FALSE); p_val # p-value IF you generate everything the same way 
t_crit = qt((alpha/2), (n-2), lower.tail = FALSE); t_crit # critical t-value for n and alpha levels

# create confidence interval
lb = est-se*t_crit # lower bound
ub = est+se*t_crit # upper bound
# show the confidence interval
paste0("The 95% confidence interval is [", round(lb,3), ", ", round(ub,3), "].")
# slightly different from the normal theory CI but very similar

# automatically obtain confidence interval in R
confint(lm(y ~ x))

### bootstrapping
n = 1000
x = rnorm(n)
effect_size = .5
y = x*effect_size + rnorm(n) # ensure correlation around .5 between X and Y
dat = data.frame(x = x, y = y) # put data into data frame
confint(lm(y ~ x, data = dat))
B = 5000 # number of bootstrap resamples
boot_res = rep(NA, B) # empty results vector to store results
alpha = .05 # set significance threshold

# start bootstrapping
for(i in 1:B){
  boot_dat = dat[sample(1:n, replace = TRUE), ]
  boot_res[i] = coef(lm(y ~ x, data = boot_dat))[2]
} # end bootstrapping

# construct boot CI 
bootLB = quantile(boot_res, alpha/2) # lower bound of CI
bootUB = quantile(boot_res, 1-(alpha/2)) # upper bound of CI

# print boot CI
paste0("The 95% percentile bootstrap confidence interval is [", round(bootLB,3), ", ", round(bootUB,3), "].")

# put results into data frame
boot_res = data.frame(boot_res = boot_res)

# visualize bootstrap CI
ggplot(boot_res) + 
  geom_histogram(aes(x = boot_res), color = "darkblue", fill = "lightblue") + 
  jtools::theme_apa() + 
  geom_vline(xintercept = bootLB, color = "black", linetype = "dashed") + # lower bound
  geom_vline(xintercept = bootUB, color = "black", linetype = "dashed") + # upper bound
  geom_vline(xintercept = effect_size, color = "forestgreen", linetype = "dashed") + # population effect size
  geom_vline(xintercept = 0, color = "red", linetype = "dotted") # 0



### end of script