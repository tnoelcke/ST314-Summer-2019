
data = c(418,
         421,
         422,
         422,
         426,
         427,
         430,
         435,
         437,
         438,
         446,
         446,
         450,
         452,
         457,
         462,
         464)
boxplot(data, horizontal = TRUE)
mean(data)
sd(data)

mean(data) + dt(0.95, 17) *(sd(data)/sqrt(17))
mean(data) - dt(0.95, 17) *(sd(data)/sqrt(17))

tstat = (mean(data) - 450)/(sd(data)/sqrt(17))
tstat
dt(tstat, 17)

data2 = c(2761, 2909, 3015, 2847, 2888)

mean(data2)
sd(data2)
tstat2 = (mean(data2) - 3000)/(sd(data2)/sqrt(5))
tstat2

p = dt(tstat2, 5) / 2
t.test(data2, mu = 3000, conf.level = 0.95)


Percent = c(	5.7,	4.1,	3.4,	2.2,	5.9,	2.6,	1.9,	2.7,	3.4,	0.8,	3.6,	4.5,	3.4,	2.8,	4.2,	4.2,	3.8,	0.0,	3.9,	4.3,	7.1,	8.7,	3.9,	8.8,	6.8,	5.0,	3.7,	2.6)		
hist(Percent)
mean(Percent)
sd(Percent)
t.test(Percent, mu = 5.0, conf.level = 0.90)


x1=24.76
s1=0.55
n1=6
x2=20.87
s2=0.550
n2=4

df=3
tails=2

c1=0

sderr=sqrt((s1^2/n1)+(s2^2/n2))
sderr

min1=(x1 - x2) + qt(0.975,df)*sderr
max1=(x1 - x2) - qt(0.975,df)*sderr


min1
max1

tstat = ((x1 - x2) - c1)/sderr
tstat
(1 - pt(tstat, df))*tails



YF = c(	29,	34,	31,	27,	28,	32,	31,	36,	32,	27,	30,	36,	29,	34,	33,	27,	28,	32,	31,	34)
OF = c(	24,	20,	21,	23,	22,	19,	15,	23,	19,	14,	22,	17)
boxplot(YF, OF)
t.test(YF, OF, alternative = "greater")
t.test(YF, OF)$conf.int

U = c(
  36.5,  
  55.0,  
  51.3,  
  38.9,  
  43.2,  
  48.8,  
  25.6,  
  49.9)  
A = c(
  28.5,  
  20.0,  
  46.0,  
  34.0,  
  36.5,  
  52.5,  
  26.5,  
  46.5)  

diff = U - A
mean(diff)
sd(diff)

t.test(U, A, paired = TRUE, mu = 0, conf.level = 0.99)

1-pt(1.7528, 7)





