#####LAB 10 SCRIPT ######### 

crabs <- read.csv("/Users/PaigeBranam/Downloads/crabs.csv")

# Exercise 1 #
#Make a strip chart of the data. Give the plot informative axes labels and titles. Add a symbol to show the average of each group. The average should be shown by a distinct symbol, so that it can be distinguished from the data.

stripchart(rate~group,data=crabs, method="jitter", vertical=TRUE, xlab="Groups", main="Crabs")
avgs <- tapply(crabs$rate, crabs$group, mean)
points(avgs, pch=18, col="blue")


#Exercise 2 # 
#Do an ANOVA to test for any differences in mean rate of heat gain among the groups. Do not use the aov command. Instead, do all of the calculations yourself in R. Use your results to fill in an ANOVA table like the one below. In addition, make a clear statement of the null and alternative hypotheses that you tested and the results of your test.

#Step 1 
#Hypothesis : 
#H0 : The heat gain among the groups will be the same. (No difference)
#HA : The heat gain among the groups will not be the same (Different Heat gains)
#Alpha : 0.5 

#Step 2
#Test Statistic 

#F Statistic - MSG/MSE 

#Step 3 - Calculate test statistic 

#Annotations/key/formulas for calculating ANOVA by hand 

#g=individual groups 
#G= grand group 
#k= number of groups : (4)
#Xbar= average 
#n= total number of subjects : (40)
#N = Length of group : (all groups are the same in length) :  (10)

# MSG = sum of squares between groups 
#(Grand Sum of (gXbar(1-4)-GXbar^2)*N) / k-1 

# MSE = sum of squares within groups 
#(Grand sum of (gx(1-4)-gxbar(1-4)^2))/n-k



#Calculations - MSE  
n <- 40
k <- 4
N <- 10

g_Xbar <- tapply(crabs$rate, crabs$group, mean)
gXbar1 <- 1.51 
gXbar2 <- 1.16 
gXbar3 <- 1.08 
gXbar4 <- 1.441 

G_Xbar <- mean(g_Xbar) #1.29775

# Sum of (xg(1-4)-g_Xbar(1-4))^2  

#Female (x1-xbar1)^2
g1_x <-((1.3-1.51)^2)+((1.6-1.51)^2)+((1.4-1.51)^2)+((1.1-1.51)^2)+((1.6-1.51)^2)+((1.8-1.51)^2)+((1.3-1.51)^2)+((1.7-1.51)^2)+((1.5-1.51)^2)+((1.8-1.51)^2)

#Intact male (x2-xbar2)^2
g2_x <- ((1.3-1.16)^2)+((1.2-1.16)^2)+((1.0-1.16)^2)+((0.9-1.16)^2)+((1.4-1.16)^2)+((1.0-1.16)^2)+((1.3-1.16)^2)+((1.4-1.16)^2)+((1.1-1.16)^2)+((1.0-1.16)^2)

#No Minor (x3-xbar3)^3
g3_x <- ((1.2-1.08)^2)+((1.0-1.08)^2)+((.9-1.08)^2)+((.8-1.08)^2)+((1.2-1.08)^2)+((.9-1.08)^2)+((1.1-1.08)^2)+((1.1-1.08)^2)+((1.3-1.08)^2)+((1.3-1.08)^2)

#No Major (x4-xbar4)^2
g4_x <- ((1.2-1.44)^2)+((1.7-1.44)^2)+((1.4-1.44)^2)+((1.2-1.44)^2)+((1.21-1.44)^2)+((1.6-1.44)^2)+((1.9-1.44)^2)+((1.4-1.444)^2)+((1.4-1.44)^2)+((1.4-1.44)^2)

#SSE -  1.548636 
#Sum of Squares within = gx(1-4)/n-k 
MSE <- (g1_x + g2_x + g3_x + g4_x) / (n-k)

#MSE <- 0.04301767  (Denominator of F Statistic)


#Calculations MSG 
#  Grand Sum of N(xbar(1-4) - G_Xbar)^2
n1 <- ((1.51-1.29775)^2)*10
n2 <- ((1.16-1.29775)^2)*10
n3 <- ((1.08-1.29775)^2)*10
n4 <- ((1.441-1.29775)^2)*10

#Solve for Sums 
N_Sum <- n1+n2+n3+n4

#SSGroups : 1.319608
#Calculate SSG
MSG <- N_Sum/(k-1)

#MSG <- 0.4398692 (Numorator of F Statistic)

FStatistic <- MSG/MSE

#FStatistic - 10.22531

#Pvalue 
#Pvalue = 1-pf(F,vgroups,verror)

#Vgroups = (n-k) - 3
#Verror = (k-1) - 36 

p <- 1-pf(FStatistic, 3, 36)
#p = 5.190931e-05 

#P value is less than alpha value, reject null hypothesis 



#Question 3 - Repeat test using AOV command 

Crabs_AOV <- aov(rate~group, data=crabs)
summary(Crabs_AOV)


#Question 4 
# Part A 
#Groups mean rate of heat gain (Calculated above by hand as well as in ANOVA AOV)

#MSG - .4399

#MSE - .0430

#Part B 
#95% Confidence Interval 

tcrit <- qt(0.975, 36)
se <- sqrt(MSE/N)
CI <- se*tcrit 
plotCI(avgs, uiw=CI, xaxt="n", ylim=c(.8,1.7))



#Question 5 
#Tukey-Kramer by Hand 

#Group comparison should be k groups = k(k-1)2 
# 4(4-1)/2 = 6 
# 6 Group Comparisons 

#Formula : 
#  xbar1-xbar2/sqrt((MSE/n)  
#Calculations available from hand calculation of ANOVA above

#all groups have 10 for n and MSE is the same, so this value will not change
y <- sqrt(MSE/10)

#Group 1 : Female - Intact Male 
(gXbar1-gXbar2)/y = 5.336354

#pvalue -
1-ptukey(5.336354, 4, 36) 
#0.003120495


#Group 2 : Intact Male - No Minor 
(gXbar2-gXbar3)/y = 1.219738

#pvalue -
1-ptukey(1.219738, 4, 36) 
#0.8239841



#Group 3 : No Minor - No Major
(gXbar4-gXbar3)/y = 5.504068

#pvalue -
1-ptukey(5.504068, 4, 36) 
#0.002236915
#Female - Intact



#Group 4 : Intact - No major 
(gXbar4-gXbar2)/y = 4.28433

#pvalue -
1-ptukey(4.28433, 4, 36) 
#0.02237932
#Female- Intact / No Major - No minor 



#Group 5 : No Minor - Female 
(gXbar1-gXbar3)/y - 6.556092

#pvalue -
1-ptukey(6.556092, 4, 36) 
#0.0002561845 
#Female - Intact / No Major - No minor / Intact - No Major 



#Group 6 : Female - No Major 
(gXbar1-gXbar4)/y - 1.052024

#pvalue -
1-ptukey(1.052024, 4, 36) 
# 0.8786417
#Intact Male - No minor 





#Question 6 
#Tukey-Kramer Short Cut 

TukeyHSD(Crabs_AOV)


#Question 7 - Make an Error Bar Plot w/ Tukey-Kramer Data 

library(gplots)


plotCI(avgs, uiw=CI, xaxt="n", ylim=c(.8,1.7))

#adding back the x axis 
axis(1, at=1:4, labels=c("female","intact","no major", "no minor"))
text(x=1:4, y=avgs+CI+.03, labels=c("a,d","a,c","b,c","d,b"))








