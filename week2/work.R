salary = function(x) {dnorm(x, mean = 75, sd=10)}
r = seq(40, 110, 0.1)
lab = seq(45, 105, 10)
plot(r, salary(r), xlab="", main="Salary", lwd = 2, xaxt="n")
axis(1, at = lab, las=2)

salaryProp = function(x) {pnorm(x, mean = 75, sd = 10)}
salaryProp(59)

salaryPrec = function(x) {}

salary = function(x) {qnorm(x, mean = 75, sd=10)}
salary(0.85)

dgamma(x.range,alpha,1/beta)
gamma = function(x) {pgamma(x,2, (1/7))}

gamma(4)