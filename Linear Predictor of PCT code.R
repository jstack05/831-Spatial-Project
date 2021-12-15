library(sf)
library(spdep)
library(tigris)
library(acs)
library(tidyverse)
library(tmap)
library(tidycensus)
library(lmvar)




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
             state = b, geometry = TRUE) %>% tibble()

# remove NA values is any
s <- na.omit(s)

#reshape to wide format
#remove columns var1 and var3
s <- subset(s, select = -c(moe, geometry))
s <- spread(s, key = variable, value = estimate)
colnames(s)[3] <- "Median.Income"
colnames(s)[4] <- "Gini.Index"

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

#inner join with presidential results and unemployment results
data <- inner_join(data, ue, by = "Description")
data$county <- sub(",.*", "", data$county) 
data

# Inner join the presidential / unemployment results with Median Income and Gini Index
s2 <- s %>% separate(NAME, c("county", "State"), sep = "[,]")
s2$State <- sub(" ", "", s2$State) 
s2$State <- state.abb[match(s2$State, state.name)]
s2 <- inner_join(s2, data, by = c("county", "State" = "st"))
s2

#Linear predictor of Trump's pct of votes
#Use 70% of dataset as training set and remaining 30% as testing set
sample <- sample(c(TRUE, FALSE), nrow(s2), replace=TRUE, prob=c(0.7,0.3))
train <- s2[sample, ]
test <- s2[!sample, ]  

#fit linear model
model <- lm(pct~Median.Income*Gini.Index*X24.Month.Average.Unemployment.Rate*(Employed..Sum.of.Last.24.Months./Unemployed..Sum.of.Last.24.Months.), data=train, x=TRUE, y=TRUE)

cv.model<- cv.lm(model, k=100)
summary(cv.model)

#view model summary
summary(model)


