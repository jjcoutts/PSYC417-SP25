# ggplot2 script PSYC417

# load required packages
library(ggplot2)
library(jtools)

# read in data
data(mtcars)

# create a histogram
ggplot(mtcars,aes(x=mpg)) + # refer to data frame and variable
  geom_histogram() + # creates a histogram
  jtools::theme_apa() + # converts to APA format
  labs(x = "Miles Per Gallon", y = "Frequency", title = "Histogram of Miles per Gallon")

# create a density plot
ggplot(mtcars,aes(x=mpg)) + # makes y-axis proportions
  geom_density() + # creates a density plot
  theme_bw() + # converts to black and white theme
  xlab("Miles Per Gallon") + # changes x-axis 
  ylab("Density") + # changes y-axis
  ggtitle("Density Plot of Miles Per Gallon") # adds plot title

# map the histogram and the density plot
ggplot(mtcars,aes(x=mpg, y = stat(density))) + # makes y-axis proportions
  geom_histogram() + # creates a histogram
  geom_density() + # creates a density plot
  theme_classic() # converts to APA format

# modify a density plot
ggplot(mtcars,aes(x=mpg)) + # makes y-axis proportions
  geom_density(size=10, color = "darkgoldenrod", fill = "#708090",linetype = "dotted") + # creates a density plot
  jtools::theme_apa() + # converts to apa theme
  labs(x = "Miles Per Gallon", y = "Frequency", title = "Histogram of Miles per Gallon")

# modify a histogram
ggplot(mtcars,aes(x=mpg)) + # refer to data frame and variable
  geom_histogram(bins=18, color = "red", fill = "green", linewidth=2) + # creates a histogram
  jtools::theme_apa() + # converts to APA format
  geom_vline(aes(xintercept=mean(mpg)), color = "purple", linewidth = 4) + # add mean line
  geom_hline(aes(yintercept=3), color = "deeppink4", linewidth = 4) + 
  labs(x = "Miles Per Gallon", y = "Frequency", title = "Histogram of Miles per Gallon")

# create and modify a scatterplot
ggplot(mtcars, aes(x = mpg, y = wt)) + 
  geom_point(color = "cadetblue", size = 5, shape = 14) + 
  jtools::theme_apa() + # change theme
  geom_smooth(method = lm, color = "pink", se = FALSE) + # add regression line
  labs(x = "Miles Per Gallon", y = "Car Weight" , title = "Plot of Weight by Miles Per Gallon")
  
# create and modify a bar graph 
ggplot(mtcars, aes(x=as.factor(cyl))) + 
  geom_bar(fill = "lightblue", color = "pink") + 
  jtools::theme_apa()

# stacked bar graphs 
ggplot(mtcars, aes(x = as.factor(cyl), fill = as.factor(gear))) + 
  geom_bar(position ="dodge") + 
  jtools::theme_apa()

# facet plot of scatterplots
ggplot(mtcars, aes(x = mpg, y = wt)) + 
  geom_point() + 
  jtools::theme_apa() + 
  facet_grid(~as.factor(cyl))

# color points in scatterplots
ggplot(mtcars, aes(x = mpg, y = wt)) + 
  geom_point(aes(color = as.factor(cyl))) + 
  jtools::theme_apa() 

# change color scheme manually
ggplot(mtcars, aes(x = wt, fill = as.factor(cyl), color = as.factor(cyl))) + 
  geom_histogram(bins = 10, aes(alpha = .7)) + 
  scale_fill_manual(values = c("orange", "purple", "beige"))+ 
  scale_color_manual(values = c("green","turquoise", "lightyellow2"))+
  jtools::theme_apa()



### end of script