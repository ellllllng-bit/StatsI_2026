#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c("ggplot2", "GGally"),  pkgTest)

#####################
# Problem 1
#####################

y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)

#Calculate 90% confidence interval
#Calculate mean, standard deviation then standard error
mean_iq <- mean(y)
mean_iq
sd_iq <- sd(y)
sd_iq
se_iq <- sd_iq / sqrt(length(y))
se_iq
#mean: 98.44. standard deviation: 13.09. standard error: 2.62.

#Calculate critical value from t-score (n<30)
t_score <- qt(0.95, df = length(y) - 1)
#t_score: 1.71

#calculate CI
ci_90_lower <- mean_iq - t_score * se_iq

ci_90_upper <- mean_iq + t_score * se_iq
#confidence interval 93.96; 102.92

#is sample iq higher than 100, one sample t-test with a=0.05
t.test(y, mu = 100, alternative = "greater")

#####################
# Problem 2
#####################

expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_2026/main/datasets/expenditure.txt", header=T)

head(expenditure)

#ggpairs function citations
#https://www.geeksforgeeks.org/r-language/create-a-matrix-of-scatterplots-pairs-equivalent-in-ggplot2/
#https://www.datanovia.com/learn/data-visualization/r-base/scatterplot-matrix

#scatterplots and correlation coefficient to investigate relartionships between variables
ggpairs(expenditure[, c("Y", "X1", "X2", "X3")],
        columns = 1:4, columnLabels = c("Homelessness Expenditure (Y)", "Income (X1)", "Financial Insecurity (X2)","Urban Areas (X3)"),
        title = "Scatterplots",
        upper = list(continuous = "cor"),
        lower = list(continuous = "smooth")) 

#graphing money spent per region
table(expenditure$Region)

#citation for adding mean to box-plot
#https://www.geeksforgeeks.org/r-language/draw-boxplot-with-mean-in-r/ 

ggplot(expenditure,
       aes(x=as.factor(Region), y = Y)) +
  geom_boxplot(color="red", fill="white", alpha=0.5) +
  labs(title = "Money Spent Addressing Homelessness by Region",
       x = "Region",
       y = "Per Capita Expenditure on Addressing Homelessness") +
  scale_x_discrete(labels=c("1" = "North East",
                            "2" = "North Central",
                            "3" = "South",
                            "4" = "West")) +
  stat_summary(fun=mean, geom="point", colour = "black") +
  stat_summary(fun=mean, geom="text", colour = "black",    
               vjust = -0.75, aes(label = paste("Mean:", round(..y.., digits = 2))))

#graph relationship between Y and X1, scatterplot with line of best fit
ggplot(data = expenditure,
       mapping = aes(x = X1, y = Y)) +
  geom_point() +
  geom_smooth(method = "lm") + 
  labs(title = "Expenditure on Addressing Homelessness and Income",
       x = "Per Capita Personal Income",
       y = "Per Capita Expenditure on Addressing Homelessness")

#adding region to scatterplot
ggplot(data = expenditure,
       mapping = aes(x = X1, y = Y, color = factor(Region), shape = factor(Region))) +
  geom_point(size=2) +
  scale_shape_discrete(labels=c("1" = "North East",
                                "2" = "North Central",
                                "3" = "South",
                                "4" = "West")) +
  scale_color_discrete(labels=c("1" = "North East",
                                "2" = "North Central",
                                "3" = "South",
                                "4" = "West")) +
  labs(title = "Expenditure on Addressing Homelessness and Income by Region",
       x = "Per Capita Personal Income",
       y = "Per Capita Expenditure on Addressing Homelessness",
       color = "Region",
       shape = "Region")

#adding regression lines for each region
ggplot(data = expenditure,
       mapping = aes(x = X1, y = Y, color = factor(Region), shape = factor(Region))) +
  geom_point(size=2) +
  geom_smooth(method = "lm") + 
  scale_shape_discrete(labels=c("1" = "North East",
                                "2" = "North Central",
                                "3" = "South",
                                "4" = "West")) +
  scale_color_discrete(labels=c("1" = "North East",
                                "2" = "North Central",
                                "3" = "South",
                                "4" = "West")) +
  labs(title = "Expenditure on Addressing Homelessness and Income by Region",
       x = "Per Capita Personal Income",
       y = "Per Capita Expenditure on Addressing Homelessness",
       color = "Region",
       shape = "Region")


