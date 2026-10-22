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
library(tidyverse)
library(modelsummary)
library(webshot2)

# Question 1
# Re-create matrix
illegalturns <- matrix(c(14, 7, 6, 7, 7, 1), nrow = 2)

# Add total row and total column
illegalturns1 <- rbind(illegalturns, colSums(illegalturns))
illegalturns1 <- cbind(illegalturns1, rowSums(illegalturns))
illegalturns1
illegalturns

# Create matrix of expected frequencies
expectedfrequency <- matrix(c(((21*27)/42), ((21*15)/42), ((13*27)/42), ((13*15)/42), ((8*27)/42), ((8*15)/42)), nrow =2)
expectedfrequency

# Calculate X squared
xsquared <- sum(((illegalturns - expectedfrequency)^2)/expectedfrequency)
xsquared

# Calculate degrees of freedom
df <- ((2-1)*(3-1))
df

# Calculate p-value
pchisq(xsquared, 2, lower.tail = FALSE)

# Calculate residuals
illegalturnsdf=as.data.frame(illegalturns)

print(illegalturnsdf)

chi <- chisq.test(illegalturnsdf)
print(chi)

chi$residual

#Question 2

# Import Data
dataq2 <- read.csv("women.csv")

# Check variables
class(dataq2$reserved)
table(dataq2$water)
# Factorize reserved variable 
dataq2 <- dataq2 |>
  mutate(
    reserved_factor = factor(reserved,
                             levels = c(0, 1),
                             labels = c("GP not reserved for women", "GP reserved for women"))
  )

# Check factorization worked
table(dataq2$reserved_factor)
levels(dataq2$reserved_factor)

# Run linear regression with reserved as predictor and water as outcome
model <- lm(water ~ reserved_factor, data=dataq2)
summary(model)

modelsummary(model, 
stars = TRUE,
coef_rename = c(
  "(Intercept)" = "Intercept", 
  "reserved_factorGP reserved for women" = "GP Reserved for Women"), 
gof_map = c("nobs", "r.squared", "adj.r.squared"),
title = "Predicting Water Facilities with Reservation Policy",
output = "regression output.png")

