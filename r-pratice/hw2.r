### R Code For problem 2
a = 42 ; b = 5 ; ### You must plug in values for a and b. 
r = seq(0,1,0.01) # Defines range of X from 0 to 1

pdf = function(x) {a*x^b*(1-x)} # Creates the pdf function 
plot(r, pdf(r), type='l', xlab = "",main = "Continuous VAR", lwd = 2)

### part c, d, e, f
cdf = function(x) {42*(((x^6)/6)-((x^7)/7))}
plot(r, cdf(r), type='l', xlab="", main="Cumulative VAR", lwd = 2)

print(mean(pdf))
print(cdf(0.7))
print(cdf(0.7) - cdf(0.45))
print(cdf(0.65) - cdf(0.45))

print(cdf(0.8943) - cdf(0.6057))


### Code for problem 3
cdf2 = function(x) {(x^2)/25}

print(cdf2(3))
print(cdf2(3) - cdf2(2.5))
print(1 - cdf2(3.5))

### Code for problem 5
n = seq(0,2, 0.1)

bankTime = function(x) {pexp(x, rate=1.5, log=FALSE)}

print(bankTime(1))
print(bankTime(3) - bankTime(1))
print(1 - bankTime(2.49))


plot(n, bankTime(n), type="l", xlab="", main = "Bank Wait Time", lwd=2)


### Code for problem 6

d = function(x) {pexp(x, rate=0.01377, log=FALSE)}

print(d(100))
print(d(200))
print(d(200) - d(100))
print(1-d(217.86492))


### Code for problem 8
### Example R code
n=seq(0, 10, 0.1)
mu = 0; sigma = 1; x1 = 2.2; x2 = -1 # Note you will have to change the value of x.
print(pnorm(x1,mu,sigma)-pnorm(x2,mu,sigma))


mu = 0; sigma = 1; p = 0.02; # Note you will have to change the value of p.
qnorm(p,mu,sigma)


### End Code



