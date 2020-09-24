################################################
# Intro to data science, Final Project
#
# Student name: Rashika Pramod Singh
# Date due:  08/15/2020
# Run these three functions to get a clean test of homework code
dev.off() # Clear the graph window
cat('\014')  # Clear the console
rm(list=ls()) # Clear user objects from the environment

# Set working directory 
setwd("/Users/rashikasingh/Desktop/syr/687/Homework")


#---Data loading and cleaning------------------------------------------------------------------------

#Installing packages
install.packages("jsonlite")
library(jsonlite)
install.packages("ggplot2")
library(ggplot2)
install.packages("dplyr")
install.packages("dplyr")
library(dplyr)
install.packages("carat")
library(carat)
install.packages("kernlab")
library(kernlab)
install.packages("maps")
library(maps)
install.packages("ggmap")
library(ggmap)
install.packages("arules")
library(arules)
install.packages("arulesViz")
library(arulesViz)
install.packages("corrplot")
library(corrplot)


#Data loading and cleaning
airData<-fromJSON("project.json")
df<-data.frame(airData)
View(df)
str(df)
summary(df)
#contains 5000 rows and 31 columns

sapply(airData,function(x)sum(is.na(x))) #check for null values
#columns with null values
#Departure.Delay.in.Minutes
#airData$Arrival.Delay.in.Minutes
#Flight.time.in.minutes

#Removing null values
airData$Arrival.Delay.in.Minutes[is.na(airData$Arrival.Delay.in.Minutes)]=round(mean(airData$Arrival.Delay.in.Minutes, na.rm=TRUE))
airData$Flight.time.in.minutes[is.na(airData$Flight.time.in.minutes)]=round(mean(airData$Flight.time.in.minutes, na.rm=TRUE))
airData$Departure.Delay.in.Minutes[is.na(airData$Departure.Delay.in.Minutes)]=round(mean(airData$Departure.Delay.in.Minutes, na.rm=TRUE))
View(df)

#Adding attributes to the data 
df1<-df
for (i in 1:length(df1$Arrival.Delay.in.Minutes)) #calculating arrival delay if greater than 5 minutes
{
  if (df1$Arrival.Delay.in.Minutes[i] > 5) 
  {
    df1$ArrivalDelay[i] <- "Yes"
  } 
  else 
  {
    df1$ArrivalDelay[i] <- "No"
  }
}

#calculating if departure delay is greater than 5 minutes
for (i in 1:length(df1$Departure.Delay.in.Minutes))
{
  if (df1$Departure.Delay.in.Minutes[i] > 5) 
  {
    df1$DepartDelay[i] <- "Yes"
  } 
  else 
  {
    df1$DepartDelay[i] <- "No"
  }
}

#Converting likelihood into categorical values
for (i in 1:length(df1$Likelihood.to.recommend))
{
  if (df1$Likelihood.to.recommend[i] < 7) 
  {
    df1$likelihood[i] <- "Detractor"
  } 
  else if (df$Likelihood.to.recommend[i] >= 7 & df$Likelihood.to.recommend[i] <= 8)
  {
    df1$likelihood[i] <- "Passive"
  }
  else 
  {
    df1$likelihood[i] <- "Promoter"
  }
}

#calculating NPS
count <- table(df1$Partner.Name,df1$likelihood)
dim(count)
npsdata <- data.frame(Airlines = unique(rownames(count)),Detractor = count[,1],Passive = count[,2],Promoters = count[,3])
npsdata$NPS <- ((npsdata$Promoters - npsdata$Detractor)/(npsdata$Promoters + npsdata$Detractor + npsdata$Passive))*100


#---Exploratory data analysis------------------------------------------------------------

#Creating histograms of numeric variables
hist(df1$Age) 
hist(df1$Flights.Per.Year)

#Creating tables of categorical variables
table(df1$Gender) #There are 2820 female travelers and 2180 male travelers
table(df1$Airline.Status) #There are 4 categories of Airline status
table(df1$Type.of.Travel)

#examining relation between likelihood to recommend and price sensitivity
price<-ggplot(df1,aes(y=Likelihood.to.recommend,x=Price.Sensitivity,fill=Flight.cancelled))+geom_boxplot() #create a boxplot
price<-price+ ggtitle("Likelihood to recommend based on price sensitivity")
price

#examining relation between type of travel and likelihood to recomend
travel <- ggplot(df1,aes(x=Type.of.Travel, y=Likelihood.to.recommend))
travel <- travel + geom_col()
travel <- travel + theme(axis.text.x = element_text(angle = 90, hjust = 1))
travel

#create histogram to examine relation between gender and likelihood
ggplot(df1,aes(x=Gender)) + geom_histogram(stat="count",color="black",aes(fill=likelihood))

#create maps to examine likelihood by state
us <- map_data("state")
df1$Origin.State <- tolower(df1$Origin.State)   
map1<- ggplot(df1, aes(map_id = Origin.State, label = Origin.State)) #ggplot for adding the data and map_id for specifying the data for map
map1<- map1 + geom_map(map = us, aes(fill=likelihood, x=dlong, y=dlat)) #adding the map as us
map1

#Calculating NPS
count <- table(df1$Partner.Name,df1$likelihood)
dim(count)
npsdata <- data.frame(Airlines = unique(rownames(count)),Detractor = count[,1],Passive = count[,2],Promoters = count[,3])
npsdata$NPS <- ((npsdata$Promoters - npsdata$Detractor)/(npsdata$Promoters + npsdata$Detractor + npsdata$Passive))*100
ggplot(npsdata,aes(x=reorder(Airlines,NPS),y=NPS)) + geom_col(aes(fill=NPS))+ theme(axis.text.x = element_text(angle = 90, hjust = 1))

#creating promoter data
promoterdata <- subset(df1, likelihood=="Promoter")
View(promoterdata)

#create detractor data
detractordata <- subset(df1, likelihood=="Detractor")
View(detractordata)

#plot for promoter data visualizing age with Gender relation
ggplot(promoterdata,aes(x=Age)) + geom_histogram(stat="count",color="black",aes(fill=Gender))

#plot for detractor data visualizing age with Gender relation
ggplot(detractordata,aes(x=Age)) + geom_histogram(stat="count",color="black",aes(fill=Gender))

#---Association rule mining------------------------------------------------------------
dfnew<-df1

#Removing  columns not necessary for analysis
dfnew <- subset(dfnew, select = -Origin.State)
dfnew <- subset(dfnew, select = -Destination.State)
dfnew <- subset(dfnew, select = -Destination.City)
dfnew <- subset(dfnew, select = -Orgin.City)
dfnew <- subset(dfnew, select = -Day.of.Month)
dfnew <- subset(dfnew, select = -olong)
dfnew <- subset(dfnew, select = -olat)
dfnew <- subset(dfnew, select = -dlong)
dfnew <- subset(dfnew, select = -dlat)
dfnew <- subset(dfnew, select = -Partner.Name)
dfnew <- subset(dfnew, select = -Airline.Status)
View(dfnew)

#create groups with the columns as average, low and high
creategroups<-function(columns)
{
  parts=quantile(columns,c(0.45,0.55))
  groups<-rep("Average",length(columns)) 
  groups[columns<=parts[1]]<-"Low"
  groups[columns>=parts[2]]<-"High"
  return(as.factor(groups))
}

#using the function to create subgroups in the columns
dfnew$Loyalty<-creategroups(dfnew$Loyalty)
dfnew$Eating.and.Drinking.at.Airport<-creategroups(dfnew$Eating.and.Drinking.at.Airport)
dfnew$Flight.Distance<-creategroups(dfnew$Flight.Distance)
dfnew$Total.Freq.Flyer.Accts<-creategroups(dfnew$Total.Freq.Flyer.Accts)
dfnew$Price.Sensitivity<-creategroups(dfnew$Price.Sensitivity)
dfnew$Flights.Per.Year<-creategroups(dfnew$Flights.Per.Year)

#converting dataframe into factors
dfnew<-mutate_all(dfnew,as.factor) 
#converting to transaction matrix
dfnew_x<-as(dfnew,"transactions")
summary(dfnew_x)
#inspect the matrix
inspect(dfnew_x) 

itemFrequency(dfnew_x)

#apriori for association rule where the lhs is default and rhs is likehood to recommend for promoters
ruleset<-apriori(dfnew_x,parameter=list(support=0.04,maxlen=20, confidence=0.5),appearance = list(default="lhs",rhs=("likelihood=Promoter")))

#apriori for association rule where the lhs is default and rhs is likehood to recommend for detractors
rule<-apriori(dfnew_x,parameter=list(support=0.04,maxlen=20, confidence=0.5),appearance = list(default="lhs",rhs=("likelihood=Detractor")))

#inspect promoters in rhs
inspectDT(ruleset)

#inspect detractors in rhs
inspectDT(rule)

#---Linear modeling--------------------------------------------------------------------
df4<-df1

#converting categorical into numeric values 
df4$Type.of.Travel <- gsub('Business travel', 0, df4$Type.of.Travel)
df4$Type.of.Travel <- (gsub('Mileage tickets', 1, df4$Type.of.Travel))
df4$Type.of.Travel <- (gsub('Personal Travel', 2, df4$Type.of.Travel))


#converting categorical into numeric values 
df4$Gender <- (gsub('Male', 0, df4$Gender))
df4$Gender <- (gsub('Female', 1, df4n$Gender))


#regression model for predicting likelihood for recomendation by age
model1<-lm(formula =Likelihood.to.recommend~Age, data=df4) 
summary(model1)

#regression model for predicting likelihood for recomendation by age, flights per year
model2<-lm(formula =Likelihood.to.recommend~Age+Flights.Per.Year, data=df4) 
summary(model2)

#regression model for predicting likelihood for recomendation by age, flights per year, price sensitivity
model3<-lm(formula =Likelihood.to.recommend~Age+Flights.Per.Year+Gender+Price.Sensitivity, data=df4)
summary(model3)

#regression model for predicting likelihood for recomendation by age, flights per year, price sensitivity, type of travel, departure delay
model4<-lm(formula =Likelihood.to.recommend~Age+Flights.Per.Year+Gender+Price.Sensitivity+Type.of.Travel+Flight.Distance+Departure.Delay.in.Minutes, data=df4) 
summary(model4)


#---Support vector machine--------------------------------------------------------------------
df3 <- df1

#creating groups for age
for (i in 1:length(df3$Age))
{
  if (df3$Age[i] >=15 & df3$Age[i] <= 29) 
  {
    df3$Age_group[i] <- "Age between 15 and 29"
  } 
  else if (df3$Age[i] >=30 & df3$Age[i] <= 54)
  {
    df3$Age_group[i] <- "Age between 30 and 54"
  }
  else 
  {
    df3$Age_group[i] <- "Age above 54"
  }
}

#removing unwanted columns
df3<- subset(df3, select = -Origin.State)
df3 <- subset(df3, select = -Destination.State)
df3 <- subset(df3, select = -Destination.City)
df3 <- subset(df3, select = -Orgin.City)
df3 <- subset(df3, select = -Day.of.Month)
df3 <- subset(df3, select = -olong)
df3 <- subset(df3, select = -olat)
df3 <- subset(df3, select = -dlong)
df3 <- subset(df3, select = -dlat)
df3 <- subset(df3, select = -Partner.Name)
df3 <- subset(df3, select = -Airline.Status)

df3$Gender <- (gsub('Male', 0, df3$Gender))
df3$Gender <- (gsub('Female', 1, df3$Gender))

#converting categorical into numeric values 
df3$Type.of.Travel <- gsub('Business travel', 0, df3$Type.of.Travel)
df3$Type.of.Travel <- (gsub('Mileage tickets', 1, df3$Type.of.Travel))
df3$Type.of.Travel <- (gsub('Personal Travel', 2, df3$Type.of.Travel))

#removing null values
df3$Likelihood.to.recommend[is.na(df3$Likelihood.to.recommend)]=round(mean(df3$Likelihood.to.recommend, na.rm=TRUE))
df3$Age[is.na(df3$Age)]=round(mean(df3$Age, na.rm=TRUE))
df3$Flight.Distance[is.na(df3$Flight.Distance)]=round(mean(df3$Flight.Distance, na.rm=TRUE))
df3$Departure.Delay.in.Minutes[is.na(df3$Departure.Delay.in.Minutes)]=round(mean(df3$Departure.Delay.in.Minutes, na.rm=TRUE))

colnames(df3)
df3 <- df3[,c(-4,-6,-10,-11,-12,-13,-17,-21,-22)] #removing unwanted columns
View(df3)

# creating random sample for training and test dataset
randIndex1 <- sample(1:dim(df3)[1])
cutPoint2_3 <- floor(2 * dim(df3)[1]/3)
trainData <- df3[randIndex1[1:cutPoint2_3],] 
testData <- df3[randIndex1[(cutPoint2_3+1):dim(df3)[1]],]

dim(trainData) 
#It contains 3333 rows and 23 columns
dim(testData) 
#It contains 1667 rows and 23 columns
View(df3)
# running support vector machine
svmOutput <- ksvm(Likelihood.to.recommend~Gender+ Age + Type.of.Travel+Flight.Distance+Departure.Delay.in.Minutes, data = trainData, kernel="rbfdot", kpar="automatic", C=5, cross=2, prob.model=TRUE)
svmOutput

#performing prediction
svmPred<-predict(svmOutput, testData) 
pred <- data.frame(svmPred)

comparison <- data.frame(testData$Likelihood.to.recommend,svmPred) #comparing the predicted values with actual values
table1<-table(comparison) 
View(table1)


