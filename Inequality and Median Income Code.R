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
             state = b, geometry = TRUE) %>% tibble()

# remove NA values is any
s <- na.omit(s)

#reshape to wide format
#remove columns var1 and var3
s <- subset(s, select = -c(moe, geometry))
s <- spread(s.1, key = variable, value = estimate)
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

#inner join with presidential results
data <- inner_join(data, ue, by = "Description")
data
data$county <- sub(",.*", "", data$county) 

# select column to work with
s2$State <- sub(" ", "", s2$State) 
s2 <- s %>% separate(NAME, c("County", "State"), sep = "[,]") #%>%
 # separate(County, c(NA, "county"), sep = "[ ]") %>%
  #separate(State, c(NA, "State"), sep = "[ ]")
s2$State <- state.abb[match(s2$State, state.name)]
data <- data %>% separate(county, c("county", NA), sep = "[ ]")
s2 <- inner_join(s2, data, by = c("county", "State" = "st"))
s2


s3 <- subset(s, select=c("estimate"))
RepPred <- subset(s2, select = c("county", "State", "geometry", "estimate", "moe", "cand", "pct")) %>% 
  filter(cand == "Donald Trump" & county == "Dallas")
RepPred <- RepPred %>% unique()

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

#Logistic Model
#make this example reproducible
set.seed(1)

#Use 70% of dataset as training set and remaining 30% as testing set
sample <- sample(c(TRUE, FALSE), nrow(s2), replace=TRUE, prob=c(0.7,0.3))
train <- s2[sample, ]
test <- s2[!sample, ]  

#fit logistic regression model
model <- glm(pct~estimate+log(moe) + estimate*log(moe), family="binomial", data=train)

#disable scientific notation for model summary
options(scipen=999)

#view model summary
summary(model)

pscl::pR2(model)["McFadden"]

