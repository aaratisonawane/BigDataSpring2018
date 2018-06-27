# CS5620 Big Data - Spring 2018
# Name: Aarati D. Sonawane
# 700#: 700675608

# -----------------------------------------------------------------------
# Notes:
# 1. Include your asnwers in this file as comments.
# 2. Also include the R commands that you used to reach your answers.
# 3. Remember you can run execute a line in this file that has R commands
#    by 1st placing the cursor at the line then pressing Ctrl+Enter. You
#    might try it on the line below:
ls()

# -----------------------------------------------------------------------
# Exercise 8: College dataset
# 8.(a)
college <- read.csv("C:/Users/Arti/Desktop/Big Data/College.csv", header=TRUE, na.strings = "?")
college

# 8.(b)
fix(college)

rownames (college )=college [,1]
fix(college)

college =college [,-1]
fix(college)

# 8.(c) i.
summary(college)

#Private        Apps           Accept          Enroll       Top10perc       Top25perc      F.Undergrad     P.Undergrad         Outstate    
#No :212   Min.   :   81   Min.   :   72   Min.   :  35   Min.   : 1.00   Min.   :  9.0   Min.   :  139   Min.   :    1.0   Min.   : 2340  
#Yes:565   1st Qu.:  776   1st Qu.:  604   1st Qu.: 242   1st Qu.:15.00   1st Qu.: 41.0   1st Qu.:  992   1st Qu.:   95.0   1st Qu.: 7320  
#          Median : 1558   Median : 1110   Median : 434   Median :23.00   Median : 54.0   Median : 1707   Median :  353.0   Median : 9990  
#          Mean   : 3002   Mean   : 2019   Mean   : 780   Mean   :27.56   Mean   : 55.8   Mean   : 3700   Mean   :  855.3   Mean   :10441  
#          3rd Qu.: 3624   3rd Qu.: 2424   3rd Qu.: 902   3rd Qu.:35.00   3rd Qu.: 69.0   3rd Qu.: 4005   3rd Qu.:  967.0   3rd Qu.:12925  
#          Max.   :48094   Max.   :26330   Max.   :6392   Max.   :96.00   Max.   :100.0   Max.   :31643   Max.   :21836.0   Max.   :21700  

# Room.Board       Books           Personal         PhD            Terminal       S.F.Ratio      perc.alumni        Expend        Grad.Rate     
#Min.   :1780   Min.   :  96.0   Min.   : 250   Min.   :  8.00   Min.   : 24.0   Min.   : 2.50   Min.   : 0.00   Min.   : 3186   Min.   : 10.00  
#1st Qu.:3597   1st Qu.: 470.0   1st Qu.: 850   1st Qu.: 62.00   1st Qu.: 71.0   1st Qu.:11.50   1st Qu.:13.00   1st Qu.: 6751   1st Qu.: 53.00  
#Median :4200   Median : 500.0   Median :1200   Median : 75.00   Median : 82.0   Median :13.60   Median :21.00   Median : 8377   Median : 65.00  
#Mean   :4358   Mean   : 549.4   Mean   :1341   Mean   : 72.66   Mean   : 79.7   Mean   :14.09   Mean   :22.74   Mean   : 9660   Mean   : 65.46  
#3rd Qu.:5050   3rd Qu.: 600.0   3rd Qu.:1700   3rd Qu.: 85.00   3rd Qu.: 92.0   3rd Qu.:16.50   3rd Qu.:31.00   3rd Qu.:10830   3rd Qu.: 78.00  
#Max.   :8124   Max.   :2340.0   Max.   :6800   Max.   :103.00   Max.   :100.0   Max.   :39.80   Max.   :64.00   Max.   :56233   Max.   :118.00  

# 8.(c) ii.
pairs(college[,1:10])


# 8.(c) iii.
plot(college$Private, college$Outstate,  main = "Outstate Versus Private", xlab = "Private", ylab = "Outstate")


# 8.(c) iv.
Elite =rep ("No",nrow(college ))
Elite [college$Top10perc >50]=" Yes"
Elite =as.factor (Elite)
college =data.frame(college ,Elite)

summary(college$Elite)
# Yes   No 
# 78  699

plot(college$Outstate ~ college$Elite, main = "Outstate versus Elite", xlab = "Elite", ylab = "Outstate")

# 8.(c) v.
par(mfrow=c(2,2))
hist(college$Apps, main = "Number of applications received", breaks = 10)
hist(college$Enroll, main = "Number of new students enrolled", breaks = 15)
hist(college$PhD, main = "Percent of faculty with Ph.D.'s", breaks = 20)
hist(college$Grad.Rate, main = "Graduation rate", breaks = 15)

# Exercise 9: Auto dataset
#       load the Auto dataset from Auto.data, and
#       make sure missing values are removed.

# 9.(a) Hint: use fix(), or summary() to figure out the answer.
# Your answer:
Auto <- read.table("C:/Users/Arti/Desktop/Big Data/Auto.txt", header = TRUE, na.strings = "?")
Auto <- na.omit(Auto)
fix(Auto)

# Quantitative predictors  : mpg, cylinders, displacement, horsepower, weight, acceleration
# Qualitative predictors   : year, origin, name



# 9.(b)
range(Auto$mpg)
# Range of mpg predictor is [9.0, 46.6]

range(Auto$cylinders)                
# Range of cylinders predictor is [3, 8]

range(Auto$displacement)              
# Range of Displacement predictor is [68, 455]

range(Auto$horsepower, na.rm = TRUE)  
# Range of HorsePower predictor is [46, 230]

range(Auto$weight)                    
# Range of Weight predictor is [1613, 5140]

range(Auto$acceleration)              
# Range of Acceleration predictor is [8.0, 24.8]

# 9.(c)
mean(Auto$mpg)
# Mean of mpg predictor = 23.44592
sd(Auto$mpg)
# Standard Deviation of mpg predictor = 7.805007


mean(Auto$cylinders)
# Mean of cylinders predictor = 5.471939
sd(Auto$cylinders)
# Standard Deviation of cylinders predictor =  1.705783


mean(Auto$displacement)
# Mean of Displacement predictor = 194.412
sd(Auto$displacement)
# Standard Deviation of Displacement predictor = 104.644 


mean(Auto$horsepower, na.rm = TRUE)
# Mean of HorsePower predictor = 104.4694
sd(Auto$horsepower, na.rm = TRUE)
# Standard Deviation of HorsePower predictor =  38.49116


mean(Auto$weight)
# Mean of Weight predictor = 2977.584
sd(Auto$weight) 
# Standard Deviation of Weight predictor = 849.4026


mean(Auto$acceleration)
# Mean of Acceleration predictor = 15.54133
sd(Auto$acceleration)
# Standard Deviation of Acceleration predictor = 2.758864


# 9.(d)
Auto_new = Auto[-seq(10,85),]

range(Auto_new$mpg)
# Range of mpg predictor = [11.0, 46.6]
mean(Auto_new$mpg) 
# mean of mpg predictor = 24.40443
sd(Auto_new$mpg)     
# Standard Deviation of mpg predictor = 7.867283

range(Auto_new$cylinders)
# Range of cylinders predictor = [3, 8]
mean(Auto_new$cylinders)  
# mean of cylinders predictor = 5.373418
sd(Auto_new$cylinders) 
# Standard Deviation of cylinders predictor = 1.654179

range(Auto_new$displacement) 
# Range of displacement predictor =  [68, 455]
mean(Auto_new$displacement) 
# mean of displacement predictor = 187.2405 
sd(Auto_new$displacement) 
# Standard Deviation of displacement predictor = 99.67837


range(Auto_new$horsepower, na.rm = TRUE) 
# Range of horsepower predictor = [46, 230]
mean(Auto_new$horsepower, na.rm = TRUE) 
# mean of horsepower predictor = 100.7215
sd(Auto_new$horsepower, na.rm = TRUE)     
# Standard Deviation of horsepower predictor = 35.70885


range(Auto_new$weight) 
# Range of weight predictor = [1649, 4997]
mean(Auto_new$weight)  
# mean of weight predictor = 2935.972
sd(Auto_new$weight)   
# Standard Deviation of weight predictor = 811.3002


range(Auto_new$acceleration) 
# Range of  acceleration predictor = [8.5, 24.8]
mean(Auto_new$acceleration) 
# mean of acceleration predictor = 15.7269
sd(Auto_new$acceleration)   
# Standard Deviation of acceleration predictor = 2.693721


range(Auto_new$year) 
# Range of year predictor = [70, 82]
mean(Auto_new$year)  
# mean of year predictor = 77.14557
sd(Auto_new$year)   
# Standard Deviation of year predictor = 3.106217


range(Auto_new$origin) 
# Range of origin predictor = [1, 3]
mean(Auto_new$origin)  
# mean of origin predictor = 1.601266
sd(Auto_new$origin)  
# Standard Deviation of origin predictor = 0.81991





