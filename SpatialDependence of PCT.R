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

#Inner join with geo data
s2 <- s %>% separate(NAME, c("county", "State"), sep = "[,]")
s2$State <- sub(" ", "", s2$State) 
s2$State <- state.abb[match(s2$State, state.name)]
s2 <- inner_join(s2, data, by = c("county", "State" = "st"))
s2
s2 <- subset(s2, select=c("GEOID", "pct"))
#Pare down to make join work in later steps
s <- subset(s, select=c("GEOID", "geometry"))

#Inner join with geometry data
s3 <- st_join(s, s2, by = "GEOID")
s3 <- subset(s3, select=c("pct", "geometry"))
colnames(s3)

# check data skewness
hist(s3$pct, main=NULL)
# check for outliers
boxplot(s3$pct, horizontal = TRUE)
# plot variable
tm_shape(s3) + tm_fill(col="pct", style="quantile", n=5, palette="Reds") +
  tm_legend(outside=TRUE)

# define neighbor
nb <- poly2nb(s3, queen=TRUE) # here nb list all ID numbers of neighbors;
# assign weights to neighbors
lw <- nb2listw(nb, style="W", zero.policy=TRUE) # equal weights
# compute neighbor average
inc.lag <- lag.listw(lw, s3$pct)
# plot polygons vs lags
plot(inc.lag ~ s3$pct, pch=16, asp=1)
M1 <- lm(inc.lag ~ s3$pct)
abline(M1, col="blue")
# access Moran's coeff
coef(M1)[2]
# calculating Moran coeff with one line
I <- moran(s3$pct, lw, length(nb), Szero(lw))[1]