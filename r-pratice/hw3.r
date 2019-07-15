###################################################################### 
# Use the R code to calculate the sample summary statistics, histogram and boxplot. 
# Can copy and paste directly to R script.
IQ.data = c(78,	90,	99,	102,	103,	103,	104,	105,	105,	105,	106,	106,
            109,	109,	109,	109,	111,	111,	111,	112,	113,	113,	118,	118,
            122,	122,	123,	124,	127,	132,	136,	140,	140,	147)

summary(IQ.data) 
sd(IQ.data) 
hist(IQ.data) 
boxplot(IQ.data, horizontal = TRUE) 
###################################################################### 

##########################################################################################
# Use R to create a stemplot 
# Copy the data and code below 
# Run the code in R
time=c(	380,	350,	356,	361,	378,	420,	322,	397,	404,	373,	376,	371,	362,
        368,	365,	328,	337,	393,	393,	368,	377,	358,	353,	406,	332,	399)
stem(time,1)
summary(time)
hist(time)
mins = time/60
summary(mins)
#Note: If your stems from R are not the same as below. Change 1 in the stem function to 2.
#######################################################################################

##########################################################
#R Code for question 10
data = c(116.7, 115.6, 114.6, 115.1, 115.7)
newData = data -50
summary(data)
sd(data)
devation = c()

for(item in data){
  numsd = item-mean(data)
  devation = c(devation, numsd)
  print(numsd)
}

variance = sum(devation^2)/5
print(variance)
sqrt(variance)
sd(devation)
print(devation)

summary(newData)
mean = sum(data)/5
print(mean)

