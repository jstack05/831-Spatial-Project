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

#For distress
"https://www.statsamerica.org/downloads/default.aspx"

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
s <- get_acs(key = api, geography = "county", variables = c("B19013_001", "B19083_001E"), 
<<<<<<< HEAD
             state = b, geometry = TRUE) 
=======
             state = b, geometry = TRUE)
>>>>>>> ec48ce25ebd25142d736b3935f25ef821f7d7898

# remove NA values is any
s <- na.omit(s)
backup.s <- s
backup.s
x <- backup.s[-seq(2, NROW(backup.s), by = 2),]
#reshape to wide format
#remove columns var1 and var3

#Just en example dataset
nc <- sf::st_read(system.file("shape/nc.shp", package="sf"))


s <- subset(s, select = -c(moe))
s <- spread(s, key = variable, value = estimate)
colnames(s)[4] <- "Median.Income"
colnames(s)[5] <- "Gini.Index"


#An attempt
s$geometry = as_Spatial("sfc_MULTIPOLYGON", cast = FALSE, IDs = counties)
spd <- sf::as_Spatial(st_geometry(s), cast = FALSE, IDs = county.ID)
sf::st_as_sf(s, coords = "geometry", crs = 4326)

#work with unemployment data
ue <- read.csv("Unemployment.csv")
ue <-ue[(ue['Month']==12 & ue['Year']==2016 & ue['Countyfips']!= 0),]
#ue$county <- sub(",.*", "", ue$Description) 

#work with presidential results data
data <- read.csv("pres16results.csv")
data= data[(data['cand'] == 'Donald Trump'),]
data <- na.omit(data)
data$county <- sub(" County.*", " County,", data$county) 
data$Description <- paste(data$county, data$st)

#inner join with presidential results
data <- inner_join(data, ue, by = "Description")
data
data$county <- sub(",.*", "", data$county) 

# select column to work with

s2 <- x %>% separate(NAME, c("county", "State"), sep = "[,]")
s2$State <- sub(" ", "", s2$State) 
s2$State <- state.abb[match(s2$State, state.name)]
s2 <- inner_join(s2, data, by = c("county", "State" = "st"))
s2

s3 <- inner_join(s, s2, by = "GEOID")
s3 <- subset(s3, select=c("pct", "geometry"))
colnames(s3)



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

#Linear predictor of Trump's pct of votes
#Use 70% of dataset as training set and remaining 30% as testing set
sample <- sample(c(TRUE, FALSE), nrow(s2), replace=TRUE, prob=c(0.7,0.3))
train <- s2[sample, ]
test <- s2[!sample, ]  

#fit linear model
model <- lm(pct~Median.Income*Gini.Index*X24.Month.Average.Unemployment.Rate*(Employed..Sum.of.Last.24.Months./Unemployed..Sum.of.Last.24.Months.), data=train)

#view model summary
summary(model)


