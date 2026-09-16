# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
  package.list <- setdiff(package.list,basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
}
detachAllPackages()

#############
# Basic stats
#############

# create vector y
y <- c(0, 4, 4, 5, 7, 10)

# (1) find sum of y using the built-in R function

sum(y)

# (2) find mean of y using your "own" function
# now do the same thing, but faster using the built-in R function

30/6

sum(y)/length(y)

#5

# (3) find sum of demeaned values

demeaned_y <- y - mean(y)

sum(demeaned_y)

# (4) calculate sum of squared error

sum(demeaned_y)

ssq <- demeaned_y^2

sum(ssq)

###########
# Quantiles
###########

# create vector
quantilesVec <- c(55, 84, 65, 54, 61, 67, 80, 59, 81, 82)

# (1) calculate median 

median(quantilesVec)

# (2) calculate quantiles

# (3) make a histogram of state median income
state.x77[,2]

# remember to save your plot as a pdf