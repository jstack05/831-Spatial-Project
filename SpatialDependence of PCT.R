library(sf)
library(spdep)
library(tigris)
library(acs)
library(tidyverse)
library(tmap)
library(tidycensus)
library(spatialreg)


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
#s2 <- subset(s2, select=c("pct", "GEOID", "geometry"))

#Get Linear Predictor Variables
x <- get_acs(key = api, geography = "county", variables = c("B19013_001", "B19083_001E"), 
             state = b, geometry = TRUE) %>% tibble()

# remove NA values is any
x <- na.omit(x)

#reshape to wide format
#remove columns var1 and var3
x <- subset(x, select = -c(moe, geometry))
x <- spread(x, key = variable, value = estimate)
colnames(x)[3] <- "Median.Income"
colnames(x)[4] <- "Gini.Index"

#Join Census median/gini data with csv's from around the internet
y <- inner_join(x,s2, by="GEOID")
y<-st_as_sf(y)

# check data skewness
hist(y$pct, main=NULL)
# check for outliers
boxplot(y$pct, horizontal = TRUE)
# plot variable
tm_shape(y) + tm_fill(col="pct", style="quantile", n=5, palette="Reds") +
  tm_legend(outside=TRUE)

# define neighbor
nb <- poly2nb(y, queen=TRUE) # here nb list all ID numbers of neighbors;
# assign weights to neighbors
lw <- nb2listw(nb, style="B", zero.policy=TRUE) # equal weights

#Linear predictor of Trump's pct of votes
#Use 70% of dataset as training set and remaining 30% as testing set
sample <- sample(c(TRUE, FALSE), nrow(y), replace=TRUE, prob=c(0.7,0.3))
train <- y[sample, ]
test <- y[!sample, ]  

#fit linear model
model <- lm(pct~Median.Income*Gini.Index*X24.Month.Average.Unemployment.Rate*Employed..Sum.of.Last.24.Months.*Unemployed..Sum.of.Last.24.Months., data=train)

#view model summary
summary(model)

##CAR model regressing pct on our predictors
#This code copied directly and adapted from the spatial regression notes
nc.sids.car.out = spautolm(pct~Unemployed..Sum.of.Last.24.Months., data=y, family="CAR", 
                           listw=lw, zero.policy=TRUE)
nc.sids.car.fitted = fitted(nc.sids.car.out)
nc.sids$fitted.car = nc.sids.car.fitted
summary(nc.sids.car.out)

#If I include too many variables, the model above does not run, so I cannot run the full model I had above^^
