library(sf)
library(spdep)
library(tigris)
library(acs)
library(tidyverse)
library(tmap)
library(tidycensus)

"https://towardsdatascience.com/spatial-autocorrelation-neighbors-affecting-neighbors-ed4fab8a4aac"
"https://www.kaggle.com/stevepalley/2016uspresidentialvotebycounty"
"https://www.ers.usda.gov/data-products/county-level-data-sets/download-data/"
"https://api.census.gov/data/2016/acs/acs1/variables.html"


states <- read.csv("States.csv", header=TRUE)
cont.states <- read.csv("ContiguousStates.csv", header=TRUE)
data <- read.csv("pres16results.csv")
a <- states$Abbreviation
b <- cont.states$Abbreviation

api="fd79d3eb4ce2c69f2ca5d2ac69199b3930b798ed"
api.key.install(key=api)
acs.tables.install()
set.seed(1234) # because we are randomizing part of the process

# Access the shapefile
s <- get_acs(key = api, geography = "tract", variables = c("B19013_001", "B19083_001E"), 
             state = b, geometry = TRUE) %>% tibble()
# remove NA values is any
s <- na.omit(s)
s$NAME
# select column to work with
<<<<<<< HEAD
s2 <- s %>% separate(NAME, c("Tract","county", "State"), sep = "[,]")
s2 <- full_join(s2, data, by = "county")
s2$pct
=======
s2 <- s %>% separate(NAME, c("Tract", "County", "State"), sep = "[,]") %>%
  separate(County, c(NA, "County"), sep = "[ ]")
data <- data %>% separate(county, c("County", NA), sep = "[ ]")
s2 <- inner_join(s2, data, by = "County")

>>>>>>> 4c0b7f09478cd6cb3b96de6c075371dd3564b61a
s3 <- subset(s, select=c("estimate"))
# check data skewness
hist(s$estimate, main=NULL)
# check for outliers
boxplot(s$estimate, horizontal = TRUE)
# plot variable
tm_shape(s) + tm_fill(col="estimate", style="quantile", n=5, palette="Greens") +
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

# hypothesis test with moran.test function
moran.test(s$estimate,lw, alternative="greater")
# using Monte-Carlo simulation
MC<- moran.mc(s$estimate, lw, nsim=999, alternative="greater")
# View results (including p-value)
MC
# plot Null distribution
plot(MC)