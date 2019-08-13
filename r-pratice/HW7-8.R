
x=c(	12,	18,	39,	43,	45)
y=c(	280,	350,	480,	500,	560)
# Construct a scatterplot using R. 
plot(x,y)

# Calculate the least squares regression line 
mod = lm(y~x)
summary(mod)

# Correlation 
cor(x,y)


k.sdevs = c(0.202,	0.313,	0.093,	0.183,	0.231,	0.211,	0.320,	0.284,
            0.145,	0.208,	0.050,	0.148,	0.275,	0.354,	0.162,	0.216,
            0.390,	0.184,	0.152,	0.232,	0.275,	0.117,	0.091,	0.059)
mean(k.sdevs)

# Construct your own Control Chart. 
LCL = 0.03768 ##### change this value to be the same as your calculated LCL
UCL = 0.3703 ##### change this value to be the same as your calculated UCL
plot(k.sdevs)
abline(h = mean(k.sdevs))
abline(h = LCL, lty = 2) 
abline(h = UCL, lty = 2)


non.conforming = c(13, 19, 23, 17, 34, 18, 4, 28, 11, 26, 31, 13, 15, 24, 15, 20, 13, 25, 15, 18, 18, 21, 11, 22, 30, 15, 14, 17, 15, 28) 
#Construct a2 p chart by using the following code. You will need to enter your values for pbar, LCL and UCL. 
pbar = 1 - (sum(non.conforming/200)/30)
pbar
  LCL = 0.03315
  UCL = 0.1578
  plot(non.conforming/200, ylim = c(0,.5))
abline(h = pbar, lty = 2)
abline(h = LCL, lty = 3)
abline(h = UCL, lty = 3)



defects = c(2, 6, 6, 4, 5, 2, 7, 4, 3, 3, 5, 6, 3, 2, 2, 3, 6, 3, 1, 5, 3, 1, 6, 5, 5) 
#Construct a c chart by using the following code. You will need to enter your values for xbar, LCL and UCL. 
xbar = mean(defects)
xbar
  LCL = 
  UCL =
  plot(defects, ylim = c(0,12))
abline(h = xbar, lty = 2)
abline(h = LCL, lty = 3)
abline(h = UCL, lty = 3)



#Sample No.	Moisture-Content Observations
subsamp1=c(	  12.3  ,	  12.1  ,	  13.2  ,	  13.0  ,	  13.0  )
subsamp2=c(	  12.5  ,	  13.3  ,	  12.7  ,	  12.6  ,	  12.9  )
subsamp3=c(	  12.9  ,	  12.7  ,	  14.2  ,	  12.5  ,	  12.9  )
subsamp4=c(	  13.2  ,	  13.0  ,	  13.0  ,	  12.6  ,	  13.6  )
subsamp5=c(	  12.8  ,	  12.3  ,	  12.4  ,	  13.3  ,	  12.0  )
subsamp6=c(	  13.7  ,	  13.4  ,	  13.1  ,	  12.4  ,	  13.2  )
subsamp7=c(	  12.2  ,	  14.4  ,	  12.4  ,	  12.4  ,	  12.5  )
subsamp8=c(	  12.6  ,	  12.8  ,	  13.5  ,	  13.9  ,	  13.1  )
subsamp9=c(	  14.6  ,	  13.4  ,	  12.2  ,	  13.7  ,	  12.4  )
subsamp10=c(	  12.8  ,	  12.3  ,	  12.6  ,	  13.2  ,	  12.8  )
subsamp11=c(	  12.6  ,	  13.1  ,	  12.7  ,	  13.2  ,	  12.3  )
subsamp12=c(	  13.5  ,	  12.5  ,	  12.8  ,	  13.1  ,	  12.9  )
subsamp13=c(	  13.4  ,	  13.3  ,	  12.0  ,	  12.9  ,	  13.1  )
subsamp14=c(	  13.5  ,	  12.4  ,	  13.0  ,	  13.6  ,	  13.4  )
subsamp15=c(	  12.3  ,	  12.8  ,	  13.0  ,	  12.8  ,	  13.5  )
subsamp16=c(	  12.6  ,	  13.4  ,	  12.1  ,	  13.2  ,	  13.1  )
subsamp17=c(	  12.1  ,	  12.7  ,	  13.4  ,	  13.0  ,	  13.9  )
subsamp18=c(	  13.0  ,	  12.8  ,	  13.0  ,	  13.3  ,	  13.1  )
subsamp19=c(	  12.4  ,	  13.2  ,	  13.0  ,	  14.0  ,	  13.1  )
subsamp20=c(	  12.7  ,	  12.4  ,	  12.4  ,	  13.9  ,	  12.8  )
subsamp21=c(	  12.6  ,	  12.8  ,	  12.7  ,	  13.4  ,	  13.0  )
subsamp22=c(	  12.7  ,	  13.4  ,	  12.1  ,	  13.2  ,	  13.1  )


# Combine the rows of sub samples into a data frame. 
# This structures data appropriately for control chart functions. 
moisture_content = data.frame(rbind(subsamp1,subsamp2,subsamp3,subsamp4,subsamp5,subsamp6, 
                                    subsamp7,subsamp8,subsamp9,subsamp10,subsamp11,subsamp12,subsamp13,subsamp14,
                                    subsamp15,subsamp16,subsamp17,subsamp18,subsamp19,subsamp20,subsamp21,subsamp22))

# Check out the data frame 
moisture_content 

# Check subgroup means 
rows = rowMeans(moisture_content)
mean(rowMeans(moisture_content))


# Open the library of commands from the qcc package. 
library("qcc")

# Create x-bar - s chart 
qcc(moisture_content, type = "xbar") 