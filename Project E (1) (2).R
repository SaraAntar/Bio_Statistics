install.packages("corrplot")
install.packages("RColorBrewer")
library(corrplot)
library(RColorBrewer)
library(car)
###########  Reading and Descriptive data ##################
#Data Reading 
load("AAD.RData")
#summarize your data using summary()
summary(AAD)

AAD$D1.Shannon.diversity=as.numeric(AAD$D1.Shannon.diversity)
AAD$D6.Shannon.diversity=as.numeric(AAD$D6.Shannon.diversity)
AAD$D1.Chao1.diversity=as.numeric(AAD$D1.Chao1.diversity)
AAD$D6.Chao1.diversity=as.numeric(AAD$D6.Chao1.diversity)
AAD$D1.D6.Jaccard.distance=as.numeric(AAD$D1.D6.Jaccard.distance)
str(AAD)
summary(AAD)

# Calculate mean, median, min, max, 1st and 3rd quartile for numeric variables

#mean
mean(AAD$D1.Shannon.diversity, na.rm=TRUE) #na.rm argument is set to TRUE, which instructs the mean() function to remove any missing or "NA" values from the calculation.
mean(AAD$D6.Shannon.diversity, na.rm=TRUE)
mean(AAD$D1.Chao1.diversity, na.rm=TRUE)
mean(AAD$D6.Chao1.diversity, na.rm=TRUE)
mean(AAD$D1.D6.Jaccard.distance, na.rm=TRUE)

#median
median(AAD$D1.Shannon.diversity, na.rm=TRUE)
median(AAD$D6.Shannon.diversity, na.rm=TRUE)
median(AAD$D1.Chao1.diversity, na.rm=TRUE)
median(AAD$D6.Chao1.diversity, na.rm=TRUE)
median(AAD$D1.D6.Jaccard.distance, na.rm=TRUE)

#Min
min(AAD$D1.Shannon.diversity, na.rm=TRUE)
min(AAD$D6.Shannon.diversity, na.rm=TRUE)
min(AAD$D1.Chao1.diversity, na.rm=TRUE)
min(AAD$D6.Chao1.diversity, na.rm=TRUE)
min(AAD$D1.D6.Jaccard.distance, na.rm=TRUE)

#Max
max(AAD$D1.Shannon.diversity, na.rm=TRUE)
max(AAD$D6.Shannon.diversity, na.rm=TRUE)
max(AAD$D1.Chao1.diversity, na.rm=TRUE)
max(AAD$D6.Chao1.diversity, na.rm=TRUE)
max(AAD$D1.D6.Jaccard.distance, na.rm=TRUE)

#1st quartile
quantile(AAD$D1.Shannon.diversity, 0.25, na.rm=TRUE)
quantile(AAD$D6.Shannon.diversity, 0.25, na.rm=TRUE)
quantile(AAD$D1.Chao1.diversity, 0.25, na.rm=TRUE)
quantile(AAD$D6.Chao1.diversity, 0.25, na.rm=TRUE)
quantile(AAD$D1.D6.Jaccard.distance, 0.25, na.rm=TRUE)

#3rd quartile 
quantile(AAD$D1.Shannon.diversity, 0.75, na.rm=TRUE)
quantile(AAD$D6.Shannon.diversity, 0.75, na.rm=TRUE)
quantile(AAD$D1.Chao1.diversity, 0.75, na.rm=TRUE)
quantile(AAD$D6.Chao1.diversity, 0.75, na.rm=TRUE)
quantile(AAD$D1.D6.Jaccard.distance, 0.75, na.rm=TRUE)

# categorical variables

# For a categorical variable a frequency table is more appropriate
table(AAD$Patient.ID)
table(AAD$Antibiotic.class)
table(AAD$Outcome)

# Convert "outcome" variable to ordered factor
AAD$Outcome <- factor(AAD$Outcome, ordered=TRUE, levels=c("CDI", "AAD", "ND"))

# Convert factor levels to numeric values
#AAD$outcome_numeric <- as.numeric(AAD$Outcome)

# Calculate mean and median of "outcome_numeric" variable
#mean_outcome <- mean(AAD$outcome_numeric)
#median_outcome <- median(AAD$outcome_numeric)

# Calculate first and third quartiles
#q1_Outcome <- quantile(AAD$outcome_numeric, 0.25)
#q3_Outcome <- quantile(AAD$outcome_numeric, 0.75)

# Calculate frequency distribution of "outcome" variable
table(AAD$outcome)

# Calculate frequency tables for all categorical variables
cat_vars <- c("Patient.ID", "Antibiotic.class", "Outcome")
for (var in cat_vars) {
  print(paste("Frequency table for", var, ":"))
  print(table(AAD[, var]))
}

#Calculate the correlation coefficient (D1 Shannon and D6 Shannon) and (D1 Chao D6 Chao)
cor(AAD$D1.Shannon.diversity, AAD$D6.Shannon.diversity)
cor(AAD$D1.Chao1.diversity, AAD$D6.Chao1.diversity)

###################   Graphics ###########################################

#1) Generate a bar chart of a categorical variable for the Outcome (AAD, CDI ,ND)
barplot(table(AAD$Outcome),ylab = "Frequency",xlab = "Antibiotic",main="barplot for the outcome ")
# THIS FIGURE shown most of the patient don't have Diarrhea & less than 50 have AAD( Antibiotics associated Diarrhea)

#2)Generate a bar chart graph with mean Outcome in BPL, FQ, OBL 
barplot(tapply(AAD$D1.Shannon.diversity,list(AAD$Antibiotic.class),mean,na.rm=T), xlab="antibiotics",ylab="mean outcome ")

# 3)Make a histogram of a continuous variable: “D1 Chao” as well as “D6 Chao”.
#par(mfrow=c(1,2))
hist(AAD$D1.Chao1.diversity,xlab="D1",main="•	Changes over time ") 
hist(AAD$D6.Chao1.diversity,xlab="D6",main="•	Changes over time ")

#4)Make a scatterplot of 2 continuous variables D1 Chao and D6 Chao, and add the regression lines for each antibiotics
plot(AAD$D6.Chao1.diversity[AAD$Antibiotic.class=="OBL"]~AAD$D1.Chao1.diversity[AAD$Antibiotic.class=="OBL"],data=AAD,main="D1.Chao1 & 6.Chao1.diversity scatterplot",
     xlab='D1.CHAO', ylab='D6 CHAO',col=1)
points(AAD$D6.Chao1.diversity[AAD$Antibiotic.class=="FQN"]~AAD$D1.Chao1.diversity[AAD$Antibiotic.class=="FQN"],data=AAD,col=2)
points(AAD$D6.Chao1.diversity[AAD$Antibiotic.class=="PBL"]~AAD$D1.Chao1.diversity[AAD$Antibiotic.class=="PBL"],data=AAD,col=4)

#Add the three regression lines for each of the 3 SES group
abline(lm(AAD$D1.Chao1.diversity[AAD$Antibiotic.class=="PBL"]~AAD$D6.Chao1.diversity[AAD$Antibiotic.class=="PBL"]),col=4)#blue
abline(lm(AAD$D1.Chao1.diversity[AAD$Antibiotic.class=="OBL"]~AAD$D6.Chao1.diversity[AAD$Antibiotic.class=="OBL"]),col=1)#black
abline(lm(AAD$D1.Chao1.diversity[AAD$Antibiotic.class=="FQN"]~AAD$D6.Chao1.diversity[AAD$Antibiotic.class=="FQN"]),col=2)#red

#5) Make a boxplot of Jacard distance   and a separate boxplots per Antbiotics (as.factors). 
boxplot(D1.D6.Jaccard.distance~Antibiotic.class,data=AAD,main="a boxplot of Jacard distance")

################### Outlier detection #########################
Outlier = boxplot(D1.Shannon.diversity~Antibiotic.class,data=AAD,plot=TRUE, main="Boxplot of D1 shannon per antibiotic")$out
#group OBL is less dispersed than the other two groups
#group FQN has no outliers , other two groups have outliers, all below the minimum value 
#medians of the three groups are close to each other 
#FQN is negatively skewed , OBL is symmetric and PBL is negatively skewed 

Outlier = boxplot(D6.Shannon.diversity~Antibiotic.class,data=AAD,plot=TRUE, main="Boxplot of D6 shannon per antibiotic")$out
#on day6 outliers appeared in group FQN, it became less dispersed than other two groups too
#all outliers below minimum value 
#on day6 FQN became symmetric , PBL is also symmetric but OBL is negatively skewed

Outlier = boxplot(D1.Chao1.diversity~Antibiotic.class,data=AAD,plot=TRUE, main="Boxplot of D1 Chao per antibiotic")$out
#groups FQN and PBL have less outliers than OBL, all outliers are above maximum value 
#FQN and OBL are symmetric , PBL is positively skewed

Outlier = boxplot(D6.Chao1.diversity~Antibiotic.class,data=AAD,plot=TRUE, main="Boxplot of D6 Chao per antibiotic")$out
#on day6 outliers disappeared from group FQN 
#outliers in OBL and PBL are all above maximum value 
#FQN is positively skewed , OBL is negatively skewed and PBL is symmetric 

#################### Testing for normality/homoscedasticity ######################


# Check the normality of variables using QQ plot
qqnorm(AAD$D1.Shannon.diversity)
qqline(AAD$D1.Shannon.diversity)

qqnorm(AAD$D6.Shannon.diversity)
qqline(AAD$D6.Shannon.diversity)

qqnorm(AAD$D1.Chao1.diversity)
qqline(AAD$D1.Chao1.diversity)

qqnorm(AAD$D6.Chao1.diversity)
qqline(AAD$D6.Chao1.diversity)

qqnorm(AAD$D1.D6.Jaccard.distance)
qqline(AAD$D1.D6.Jaccard.distance)

# Check the normality of variables using histogram
hist(AAD$D1.Shannon.diversity, xlab="D1 Shannon", main="D1 Shannon Histogram")
hist(AAD$D6.Shannon.diversity, xlab="D6 Shannon", main="D6 Shannon Histogram")
hist(AAD$D1.Chao1.diversity, xlab="D1 Chao1", main="D1 Chao1 Histogram")
hist(AAD$D6.Chao1.diversity, xlab="D6 Chao1", main="D6 Chao1 Histogram")
hist(AAD$D1.D6.Jaccard.distance, xlab="Jacard Distance", main="Jacard Distance Histogram")

# Check the normality of variables using Shapiro Test
shapiro.test(AAD$D1.Shannon.diversity)
shapiro.test(AAD$D6.Shannon.diversity)
shapiro.test(AAD$D1.Chao1.diversity)
shapiro.test(AAD$D6.Chao1.diversity)
shapiro.test(AAD$D1.D6.Jaccard.distance)

# According to the 3 tests done above on the numerical variables, data are found to 
# be not normal, and according to shapiro-test, the p-value of all of them are less than 0.05 so there is a significant difference and we have enough evidence
# to reject the null hypothesis (it is not normally distributed).

# Check homoscedasticity

# Bartlett test and F-test
bartlett.test(list(AAD$D1.Shannon.diversity,AAD$D6.Shannon.diversity))
var.test(AAD$D1.Shannon.diversity,AAD$D6.Shannon.diversity)
#p-value = 2.533e-06, so we reject the null hypothesis, where the variance of both of them are not similar.


bartlett.test(list(AAD$D1.Chao1.diversity,AAD$D6.Chao1.diversity))
var.test(AAD$D1.Chao1.diversity,AAD$D6.Chao1.diversity)
# p-value = 0.03624, so we reject the null hypothesis, where the variance of both of them are not similar.





###################################### confidence interval ###################################################################

means <- tapply(AAD$D1.D6.Jaccard.distance,list(Antibiotic.class=AAD$Antibiotic.class),mean,na.rm=T)
sd <- tapply(AAD$D1.D6.Jaccard.distance,list(Antibiotic.class=AAD$Antibiotic.class),sd,na.rm=T)
x = means
s = sd
n = 335
#90%
margin_error <- qt(0.90,df=n-1)*s/sqrt(n)
lowerinterval <- x - margin_error
upperinterval <- x + margin_error

lowerinterval 
upperinterval 

#95
#I pick 0.975 to get a two-sided confidence interval. 
#This gives 2.5% of the probability in the upper tail and 2.5% in the lower tail, .
margin_error <- qt(0.95,df=n-1)*s/sqrt(n) 
margin_error
lowerinterval <- x - margin_error
upperinterval <- x + margin_error
#using the qnorm function to calculate the value of the z-score that corresponds to a 95% confidence interval.
#This value is being multiplied by "s" and divided by the square root of "n" to calculate the margin of error
#for the confidence interval.
#margin error tells you how persentage point eill differ from the real population value.

lowerinterval 
upperinterval 

#99
margin_error <- qt(0.99,df=n-1)*s/sqrt(n)
lowerinterval <- x - margin_error
upperinterval <- x + margin_error

lowerinterval
upperinterval
# any index of jaccard distance between 0.63&0.67 belong to FQN class 
#and between 0.6543 &0.690 belong to OBL class 
#between 0.620 &0.6633 belong to PBL class 
####
####
#99% confidence interval
t.test(AAD[AAD$Antibiotic.class=="OBL",]$D6.Shannon.diversity, conf.level = 0.99)
t.test(AAD[AAD$Antibiotic.class=="FQN",]$D6.Shannon.diversity, conf.level = 0.99)
t.test(AAD[AAD$Antibiotic.class=="PBL",]$D6.Shannon.diversity, conf.level = 0.99)


#95% confidence interval
t.test(AAD[AAD$Antibiotic.class=="OBL",]$D6.Shannon.diversity, conf.level = 0.95)
t.test(AAD[AAD$Antibiotic.class=="FQN",]$D6.Shannon.diversity, conf.level = 0.95)
t.test(AAD[AAD$Antibiotic.class=="PBL",]$D6.Shannon.diversity, conf.level = 0.95)

#90% confidence interval
t.test(AAD[AAD$Antibiotic.class=="OBL",]$D6.Shannon.diversity, conf.level = 0.90)
t.test(AAD[AAD$Antibiotic.class=="FQN",]$D6.Shannon.diversity, conf.level = 0.90)
t.test(AAD[AAD$Antibiotic.class=="PBL",]$D6.Shannon.diversity, conf.level = 0.90)

#D6,chao
#99% confidence interval
t.test(AAD[AAD$Antibiotic.class=="OBL",]$D6.Chao1.diversity, conf.level = 0.99)
t.test(AAD[AAD$Antibiotic.class=="FQN",]$D6.Chao1.diversity, conf.level = 0.99)
t.test(AAD[AAD$Antibiotic.class=="PBL",]$D6.Chao1.diversity, conf.level = 0.99)


#95% confidence interval
t.test(AAD[AAD$Antibiotic.class=="OBL",]$D6.Chao1.diversity, conf.level = 0.95)
t.test(AAD[AAD$Antibiotic.class=="FQN",]$D6.Chao1.diversity, conf.level = 0.95)
t.test(AAD[AAD$Antibiotic.class=="PBL",]$D6.Chao1.diversity, conf.level = 0.95)

#90% confidence interval
t.test(AAD[AAD$Antibiotic.class=="OBL",]$D6.Chao1.diversity, conf.level = 0.90)
t.test(AAD[AAD$Antibiotic.class=="FQN",]$D6.Chao1.diversity, conf.level = 0.90)
t.test(AAD[AAD$Antibiotic.class=="PBL",]$D6.Chao1.diversity, conf.level = 0.90)


#################### Hypothesis testing ###############################

# H0: Chao/Shannon at day 6 is the same in day1 for CDI.
# Ha: Chao/Shannon at day 6 different from day1 for CDI.
# Assuming normality with equal variance: we can use two-sample t-test.

# D1.Chao1.diversity for CDI
t.test(AAD[AAD$Outcome=="CDI",]$D1.Chao1.diversity, var.equal = TRUE)
# p-value = 0.01209 which is smaller than the significance level of 0.05, thus we reject the null hypothesis

# D6.Chao1.diversity for CDI
t.test(AAD[AAD$Outcome=="CDI",]$D6.Chao1.diversity, var.equal = TRUE)
# p-value = 0.00578 which is smaller than the significance level of 0.05, thus we reject the null hypothesis
#then, Chao at day 6 different from day1 

# D1.Shannon.diversity for CDI
t.test(AAD[AAD$Outcome=="CDI",]$D1.Shannon.diversity, var.equal = TRUE)
# p-value = 0.009446 which is smaller than the significance level of 0.05, thus we reject the null hypothesis

# D6.Shannon.diversity for CDI
t.test(AAD[AAD$Outcome=="CDI",]$D6.Shannon.diversity, var.equal = TRUE)
# p-value = 4.106e-05(0.00004106) which is much smaller than the significance level of 0.05, thus we reject the null hypothesis
#In this case, a p-value of 0.00004106 suggests that the observed data is very unlikely to have occurred by chance alone, assuming that the null hypothesis is true.
#Therefore, the null hypothesis may be rejected in favor of the alternative hypothesis

#then, Shannon at day 6 different from day1 

# Assess whether the previous test assumptions have been met for the test.

# Test normality for D6 and D1 in chao
qqnorm(AAD[AAD$Outcome == "CDI",]$D1.Chao1.diversity, main='CDI D1.Chao')
qqline(AAD[AAD$Outcome == "CDI",]$D1.Chao1.diversity)
shapiro.test(AAD[AAD$Outcome=="CDI",]$D1.Chao1.diversity)
# D1.Chao1.diversity is normal in CDI

qqnorm(AAD[AAD$Outcome == "CDI",]$D6.Chao1.diversity, main='CDI D6.Chao')
qqline(AAD[AAD$Outcome == "CDI",]$D6.Chao1.diversity)
shapiro.test(AAD[AAD$Outcome=="CDI",]$D6.Chao1.diversity)
# D6.Chao1.diversity is not normal in CDI
#thus assume non-normality

# Then we will use Wilcoxon rank sum test
wilcox.test(AAD[AAD$Outcome=="CDI",]$D1.Chao1.diversity)
wilcox.test(AAD[AAD$Outcome=="CDI",]$D6.Chao1.diversity)
# The p-value = 0.0625 which is smaller than α(0.05), thus we reject the null hypothesis
# Chao at day 6 different from day1 for CDI.

# Test normality for D6 and D1 in Shannon
qqnorm(AAD[AAD$Outcome == "CDI",]$D1.Shannon.diversity, main='CDI D1.Shannon')
qqline(AAD[AAD$Outcome == "CDI",]$D1.Shannon.diversity)
shapiro.test(AAD[AAD$Outcome=="CDI",]$D1.Shannon.diversity)

qqnorm(AAD[AAD$Outcome == "CDI",]$D6.Shannon.diversity, main='CDI D6.Shannon')
qqline(AAD[AAD$Outcome == "CDI",]$D6.Shannon.diversity)
shapiro.test(AAD[AAD$Outcome=="CDI",]$D6.Shannon.diversity)
# D1.Shannon.diversity is normal and D6.Shannon.diversity is normal in CDI

# The previous test assumptions for normality have not been met for the test in Chao
#but in shannon The previous test assumptions for normality have been met

boxplot(D1.Chao1.diversity~AAD$Outcome=="CDI", data=AAD)
leveneTest(D1.Chao1.diversity~AAD$Outcome=="CDI", data=AAD)#better to use levene because it does not assume normality and more robust
var.test(D1.Chao1.diversity~AAD$Outcome=="CDI", data=AAD)

boxplot(D6.Chao1.diversity~AAD$Outcome=="CDI", data=AAD)
leveneTest(D6.Chao1.diversity~AAD$Outcome=="CDI", data=AAD)
var.test(D6.Chao1.diversity~AAD$Outcome=="CDI", data=AAD)

boxplot(D1.Shannon.diversity~AAD$Outcome=="CDI", data=AAD)
leveneTest(D1.Shannon.diversity~AAD$Outcome=="CDI", data=AAD)#better to use levene because it does not assume normality and more robust
var.test(D1.Shannon.diversity~AAD$Outcome=="CDI", data=AAD)

boxplot(D6.Shannon.diversity~AAD$Outcome=="CDI", data=AAD)
leveneTest(D6.Shannon.diversity~AAD$Outcome=="CDI", data=AAD)
var.test(D6.Shannon.diversity~AAD$Outcome=="CDI", data=AAD)

# We hypothesis that Chao “different” in the group receiving BPL Antibiotics  compared to the FQ antibiotics B. Can you test this hypothesis assuming heteroscedasiticy 
#we firstly test normality but here heteroscedasticity of variance is assumed so we gonna check only normality not variance to know what test we are going to use
par(mfrow=c(1,2))

#QQPLOT for FQN
qqnorm(AAD[AAD$Antibiotic.class=="FQN",]$D1.Chao1.diversity)
qqline(AAD[AAD$Antibiotic.class=="FQN",]$D1.Chao1.diversity)
#QQPLOT for PBL
qqnorm(AAD[AAD$Antibiotic.class=="PBL",]$D1.Chao1.diversity)
qqline(AAD[AAD$Antibiotic.class=="PBL",]$D1.Chao1.diversity)

shapiro.test(AAD[AAD$Antibiotic.class=="FQN",]$D1.Chao1.diversity)
shapiro.test(AAD[AAD$Antibiotic.class=="PBL",]$D1.Chao1.diversity)
# The p-values are smaller than the significance(0.05), thus we reject the null hypothesis
# so we assume non-normality
#so here we will use man whitny test (welcox test) 
wilcox.test(AAD[AAD$Antibiotic.class == "PBL",]$D1.Chao1.diversity, AAD[AAD$Antibiotic.class == "FQN",]$D1.Chao1.diversity)
# The p-value = 0.2818 which is bigger than α(0.05), thus we don't have enough evidence to reject the
# null hypothesis in support of alternative hypothesis and D1.Chao1.diversity is similar between PBL and FQN Antibiotics 

#Assess the previous test assumption.

library(car)
leveneTest(D1.Chao1.diversity~AAD$Antibiotic.class=="FQN", data=AAD)
leveneTest(D1.Chao1.diversity~AAD$Antibiotic.class=="PBL", data = AAD)
var.test(AAD[AAD$Antibiotic.class=="FQN",]$D1.Chao1.diversity,
         AAD[AAD$Antibiotic.class=="PBL",]$D1.Chao1.diversity)
##### There is no difference in variance in the 2 groups, so we will use 2-samples t-test
wilcox.test(AAD[AAD$Antibiotic.class=="FQN",]$D1.Chao1.diversity,AAD[AAD$Antibiotic.class=="PBL",]$D1.Chao1.diversity, paired = F, var.equal = F)
####### The previous test assumptions has not been met


# We hypothesis that Chao is different between the different Antibiotics overtime. Can you perform comparison between the different groups, after assessing the assumptions and performing post-hoc testing (assuming normality and homoscedasticity).
# here we have in our data the groups of antibiotics are categorical and the CHAO is continuous so we gonna use anova test

# Anova
library(car)
library(report)
AnovaModel = aov(D6.Chao1.diversity~Antibiotic.class, AAD)
summary(AnovaModel)
report(AnovaModel)# not significant
coef(AnovaModel)
#the results of anova test gives a p value equal to 0.507 which is much greater than the significance level alpha 0.05 so we do not have enough evidence to reject the null hypothesis in support of alternative hypothesis so Chao is not different between the different Antibiotics overtime 

# Posthoc using tukey test
#posthoc is performed after annova assuming normality and homoscedasticity (includes p value correction to be p adjusted) 
TukeyHSD(AnovaModel) 

#the results of posthoc shows the difference of means between antibiotics with each others(OBL-FQN,PBL-FQN,PBL-OBL)
#The first column (`diff`) shows the estimated difference in means between each pair of groups being compared. The second and third columns (`lwr` and `upr`) show the lower and upper bounds of the 95% confidence intervals for the estimated differences in means. The last column (`p adj`) shows the p-value for each pairwise comparison, adjusted for multiple comparisons using the Tukey method.
#after anova we perform posthoc(tukey test) , the results of posthoc of p adjusted value show that all antibiotics with each other gives a p adjusted value greater than the significance level alpha which means that we do not have enough evidence to reject the null so this means that Chao is not different 
plot(TukeyHSD(AnovaModel))

#test normality and homo variance to check assumptions are true or not
#QQPLOT for Antibiotic OBL
qqnorm(AAD[AAD$Antibiotic.class == "OBL",]$D1.Chao1.diversity, main='OBL Chao')
qqline(AAD[AAD$Antibiotic.class == "OBL",]$D1.Chao1.diversity)
#QQPLOT for Antibiotic PBL
qqnorm(AAD[AAD$Antibiotic.class == "PBL",]$D1.Chao1.diversity, main='PBL Chao')
qqline(AAD[AAD$Antibiotic.class == "PBL",]$D1.Chao1.diversity)
#QQPLOT for Antibiotic FQN
qqnorm(AAD[AAD$Antibiotic.class == "FQN",]$D1.Chao1.diversity, main='FQN Chao')
qqline(AAD[AAD$Antibiotic.class == "FQN",]$D1.Chao1.diversity)
#data is not normally distributed

#check normality within each group with histogram
hist(AAD[AAD$Antibiotic.class == "OBL",]$D1.Chao1.diversity, main='Antibiotic OBL')
hist(AAD[AAD$Antibiotic.class == "PBL",]$D1.Chao1.diversity, main='Antibiotic PBL')
hist(AAD[AAD$Antibiotic.class == "FQN",]$D1.Chao1.diversity, main='Antibiotic FQN')
#DATA is not normally distributed

# shapiro test for Antibiotic OBL
shapiro.test(AAD[AAD$Antibiotic.class == "OBL",]$D1.Chao1.diversity) #p value = 1.95e-06 which is much smaller than 0.05 then reject null hypothesis
#not normal

# shapiro test for Antibiotic PBL
shapiro.test(AAD[AAD$Antibiotic.class == "PBL",]$D1.Chao1.diversity) #p value = 0.000228 which is much smaller than 0.05 then reject null hypothesis
#not normal

# shapiro test for Antibiotic FQN
shapiro.test(AAD[AAD$Antibiotic.class == "FQN",]$D1.Chao1.diversity) #p value = 0.01034 which is smaller than 0.05 then reject null hypothesis
#not normal

#Hence, assumption have not been met
#so we gonna use kruskal wallis test ( non-parametric )

kruskal.test(D1.Chao1.diversity~Antibiotic.class, AAD) # p value equal to 0.3945 which is greater than 0.05 so we do not have evidence to reject the null so CHAO is not different between groups of Antibiotics. 

#check variance 
leveneTest(D1.Chao1.diversity~Antibiotic.class, data=AAD) #P-VALUE = 0.9278
#homoscedasticity 
##Hence, assumption have been met


#################### Linear Regression ###############################

AAD$Antibiotic_numeric <- as.numeric(AAD$Antibiotic.class)
AAD$Antibiotic.class=as.numeric(AAD$Antibiotic.class)
AAD$Antibiotic.class<-as.integer(AAD$Antibiotic.class)
plot(AAD$Antibiotic.class, AAD$D1.Chao1.diversity)

chao$Antibiotic.class <- as.factor(chao$Antibiotic)

#fit linear regression model
fit <- lm(Chao ~ Antibiotic + Time, data = chao)
summary(fit)


plot(AAD$Antibiotic, AAD$Chao, pch=16, cex=2)
plot(AAD$Time, AAD$Chao, pch=16, cex=2)
simple.regression <- lm(Antibiotic ~ Chao, data=AAD)


