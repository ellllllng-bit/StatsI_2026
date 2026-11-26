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
lapply(c("car"),  pkgTest)
install.packages(car)
library(car)
library(dplyr)
library(stargazer)


# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Load and Check Data
data(Prestige)
help(Prestige)

#Q1A

#Check type variable

Prestige$type
levels(Prestige$type)
class(Prestige$type)
table(Prestige$type)

Prestige$professional <- ifelse(Prestige$type == "prof", 1, 0)
table(Prestige$professional)

#Q1B Regression
#interaction with dummy and continuous

profmodel <- lm(prestige ~ income * professional, data = Prestige)
stargazer(profmodel)

#Q1F Calculate manually then check with marginal effects

0.0031709*1000 - 0.0023257*1000*1
(0.0031709 - 0.0023257)*1000


(21.1422589 + 0.0031709*1000 + 37.7812800*1 - 0.0023257*1000*1) - (21.1422589 + 0.0031709*0 + 37.7812800*1 - 0.0023257*0*1)

#1G calculate then double check

37.7812800*1 - 0.0023257*6000*1
37.7812800 - 0.0023257*6000

(21.1422589 + 0.0031709*6000 + 37.7812800*1 - 0.0023257*6000*1) - (21.1422589 + 0.0031709*6000 + 37.7812800*0 - 0.0023257*6000*0)

#Q2A
#Hypothesis test, alpha 0.05

TS <- (0.042-0) / 0.016
TS

p_val <- 2*pt(abs(TS), 131-3, lower.tail = FALSE)
p_val

#2B

TS2 <- (0.042-0) / 0.013
TS2

p_val2 <- 2*pt(abs(TS2), 131-3, lower.tail = FALSE)
p_val2
