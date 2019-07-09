salary = function(x) {dnorm(x, mean = 75, sd=10)+75}
r = seq(40, 110, 0.1)
lab = seq(45, 105, 10)
plot(r, salary(r), xlab="", main="Salary", lwd = 2, xaxt="n")
axis(1, at = lab, las=2)