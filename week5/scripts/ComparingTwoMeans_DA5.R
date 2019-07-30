# Upload the epa data set 
# call the dataset cardata

cardata = read.csv(file="/Users/engineeringuser/Documents/ST314-Summer-2019/week5/scripts/EpaFE2019Data.csv.xls", header = TRUE)

View(cardata)

# Create a side by side boxplot
boxplot(EstimatedFuelCostin5Years~Guzzler, data = cardata, 
        main = "Guzlers Vs Non Guzlers", 
        xlab = "cost compared to average",
        col = c("red", "green"), ### add different color names
        horizontal = TRUE)

# Calculate the mean, sd and sample sizes for 5 year fuel cost split among Guzzler and no Guzzler
# the command aggregate() will perform the given function on the specified groups
# aggregate(response~treatment, data = datasetname, function)
# Make a table of values with the results. 

aggregate(EstimatedFuelCostin5Years~Guzzler, data = cardata, mean)
aggregate(EstimatedFuelCostin5Years~Guzzler, data = cardata, sd)
aggregate(EstimatedFuelCostin5Years~Guzzler, data = cardata, length)

# What type of vehicles are Guzzlers? Find out by subsetting the dataset to only include Guzzlers. 


GuzzlersOnly = subset(cardata, Guzzler == "Guzzler")
View(GuzzlersOnly)


NonGuzzler = subset(cardata, Guzzler == "No")

t.test(EEstimatedFuelCostin5Years~Guzzler, data = cardata, conf.level = 0.99, alternative ="two.sided")




# Perform a two sample t test 
# t.test(response~treatment, data = datasetname, conf.level = enterconfidencelevel, alternative = "two.sided" ))


