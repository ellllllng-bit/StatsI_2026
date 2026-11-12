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

# Load Necessary Packages
library(ggplot2)
library(stargazer)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# read in data
inc.sub <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_2026/main/datasets/incumbents_subset.csv")

# Check Variables
summary(inc.sub$voteshare)
summary(inc.sub$difflog)
summary(inc.sub$presvote)

# Question 1.1 Run Regression

inc.submodel <- lm(voteshare ~ difflog, data=inc.sub)
stargazer(inc.submodel)

# 1.2 Scatterplot

ggplot(data = inc.sub,
       mapping = aes(x = difflog, y = voteshare)) +
  geom_point(size=0.5) +
  geom_smooth(method = "lm") +
  labs(title = "Incumbents Vote Share by Campaign Spending",
       x = "Log Difference in Incumbent vs. Challenger Campaign Spending",
       y = "Incumbent Vote Share")

# 1.3 Save Residuals

q1residuals <- residuals(inc.submodel)

# 2.1 Linear Regression

inc.submodelpv <- lm(presvote ~ difflog, data=inc.sub)
stargazer(inc.submodelpv)

# 2.2 Scatterplot

ggplot(data = inc.sub,
       mapping = aes(x = difflog, y = presvote)) +
  geom_point(size=0.5) +
  geom_smooth(method = "lm") +
  labs(title = "Presidential Vote Share by Campaign Spending",
       x = "Log Difference in Incumbent vs. Challenger Campaign Spending",
       y = "Vote Share of Presidential Candidate in Incumbent's Party")

# 2.3 Save Residuals

q2residuals <- residuals(inc.submodelpv)

# 3.1 Linear Regression

inc.submodel3 <- lm(voteshare ~ presvote, data=inc.sub)
stargazer(inc.submodel3)

# 3.2 Scatterplot

ggplot(data = inc.sub,
       mapping = aes(x = presvote, y = voteshare)) +
  geom_point(size=0.5) +
  geom_smooth(method = "lm") +
  labs(title = "Incumbent Vote Share by Presidential Vote Share",
       x = "Vote Share of Presidential Candidate in Incumbent's Party",
       y = "Incumbent Vote Share")

# 4.1 Linear Regression with Residuals

inc.submodelresid <- lm(q1residuals ~ q2residuals, data=inc.sub)
stargazer(inc.submodelresid)

# 4.2 Scatterplot of Residuals

ggplot(data = inc.sub,
       mapping = aes(x = q2residuals, y = q1residuals)) +
  geom_point(size=0.5) +
  geom_smooth(method = "lm") +
  labs(title = "Residuals Scatterplot",
       x = "Q2 Residuals",
       y = "Q1 Residuals")

# 5.1 Multiple Linear Regression

inc.submodelmul <- lm(voteshare ~ difflog + presvote, data=inc.sub)
stargazer(inc.submodelmul)

# 5.3

stargazer(inc.submodelmul, inc.submodelresid)
