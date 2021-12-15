library(sf)
library(spdep)
library(tigris)
library(acs)
library(tidyverse)
library(tmap)
library(tidycensus)

states <- read.csv("States.csv", header=TRUE)
cont.states <- read.csv("ContiguousStates.csv", header=TRUE)
data <- read.csv("pres16results.csv")
ue <- read.csv("Unemployment.csv")
a <- states$Abbreviation
b <- cont.states$Abbreviation

api="fd79d3eb4ce2c69f2ca5d2ac69199b3930b798ed"
api.key.install(key=api)
acs.tables.install()
set.seed(1234) # because we are randomizing part of the process

# Access the shapefile
s <- get_acs(key = api, geography = "county", variables = "B19013_001", 
             state = b, geometry = TRUE)
# remove NA values is any
s <- na.omit(s)
# select column to work with
s <- subset(s, select=c("estimate"))
s2<- subset(s2, select=c("pct"))

# check data skewness
hist(s$estimate, main=NULL)
# check for outliers
boxplot(s$estimate, horizontal = TRUE)
# plot variable
tm_shape(s) + tm_fill(col="estimate", style="quantile", n=5, palette="Greens") +
  tm_legend(outside=TRUE)

# check data skewness
hist(s2$pct, main=NULL)
# check for outliers
boxplot(s2$pct, horizontal = TRUE)
# plot variable
tm_shape(s2) + tm_fill(col="pct", style="quantile", n=5, palette="Greens") +
  tm_legend(outside=TRUE)

# define neighbor
nb <- poly2nb(s, queen=TRUE) # here nb list all ID numbers of neighbors;
# assign weights to neighbors
lw <- nb2listw(nb, style="W", zero.policy=TRUE) # equal weights
# compute neighbor average
inc.lag <- lag.listw(lw, s$estimate)
# plot polygons vs lags
plot(inc.lag ~ s$estimate, pch=16, asp=1)
M1 <- lm(inc.lag ~ s$estimate)
abline(M1, col="blue")
# access Moran's coeff
coef(M1)[2]
# calculating Moran coeff with one line
I <- moran(s$estimate, lw, length(nb), Szero(lw))[1]
