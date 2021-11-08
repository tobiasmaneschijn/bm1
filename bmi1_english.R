
###########################################################################
## Set the working directory

## In RStudio the working directory is easily set via the menu
## "Session -> Set Working Directory -> To Source File Location" 
## Note: In R only "/" is used for separating in paths 
## (i.e. no backslash).


###########################################################################
## Read data into R

## Read data from bmi1_data.csv
D <- read.table("bmi1_data.csv", header=TRUE, sep=";", as.is=TRUE)


###########################################################################
## Simple overview of the data

## Dimensions of D (number of rows and columns)
dim(D)
##  Column/variable names
names(D)
## The first rows/observations
head(D)
## The last rows/observations
tail(D)
## Selected summary statistics
summary(D)
## Another type of summary of the dataset
str(D)

is.na(D)

###########################################################################
## Calculate BMI scores

## Calculate BMI scores and add new variable to D
D$bmi <- D$weight/(D$height/100)^2


###########################################################################
## Histogram (empirical density)

## Histogram describing the empirical density of the BMI scores
## (histogram of the BMI scores normalized to have an area of 1)
hist(D$bmi, xlab="BMI", prob=TRUE)


###########################################################################
## Taking subsets of the data using 'subset'

## Divide data into two subsets according to gender
Dfemale <- subset(D, gender == 0)
Dmale <- subset(D, gender == 1)


###########################################################################
## Density histograms by gender

## Density histograms describing the empirical density
## of the BMI scores of women and men, respectively.
hist(Dfemale$bmi, xlab="BMI (female)", prob=TRUE)
hist(Dmale$bmi, xlab="BMI (male)", prob=TRUE)


###########################################################################
## Box plot

## Box plot of BMI scores by gender
boxplot(Dfemale$bmi, Dmale$bmi, names=c("Female", "Male"), 
        xlab="Gender", ylab="BMI")


###########################################################################
## Summary statistics for BMI
plot.ecdf(Dfemale$bmi)
plot.ecdf(Dmale$bmi)


## Both Genders combined
## Total number of observations
## (doesn't include missing values if there are any)
sum(!is.na(D$bmi))

## Sample mean (both genders combined)
mean(D$bmi, na.rm=TRUE)
## Sample variance (both genders combined)
var(D$bmi, na.rm=TRUE)
## etc.
sd(D$bmi, na.rm=TRUE)

## Only Females

## Total number of observations
sum(!is.na(Dfemale$bmi))

## Sample mean (female)
mean(Dfemale$bmi, na.rm=TRUE)
## Sample variance (female)
var(Dfemale$bmi, na.rm=TRUE)
## etc.
sd(Dfemale$bmi, na.rm=TRUE)

## Only Males

## Total number of observations
sum(!is.na(Dmale$bmi))

## Sample mean (female)
mean(Dmale$bmi, na.rm=TRUE)
## Sample variance (female)
var(Dmale$bmi, na.rm=TRUE)
## etc.
sd(Dmale$bmi, na.rm=TRUE)

## Summary stats for all

summary(D$bmi, na.rm=TRUE)
summary(Dfemale$bmi, na.rm=TRUE)
summary(Dmale$bmi, na.rm=TRUE)
##
## The argument 'na.rm=TRUE' ensures that the statistic is
## computed even in cases where there are missing values.


###########################################################################
## qq-plot for model validation

## New variable 'logbmi' with log-transformed BMI
D$logbmi <- log(D$bmi)
## qq-plot of log-transformed BMI
qqnorm(D$logbmi)
qqline(D$logbmi)



Dfemale$logbmi <- log(Dfemale$bmi)
Dmale$logbmi <- log(Dmale$bmi)
## qq-plot of log-transformed BMI
qqnorm(Dfemale$logbmi)
qqline(Dfemale$logbmi)

qqnorm(Dmale$logbmi)
qqline(Dmale$logbmi)

3.217641 - 1.976575 * ((0.1488778) / (sqrt(145)))
3.217641 + 1.976575 * ((0.1488778) / (sqrt(145)))

qt(p=0.975, df=144)

qt(p=0.975, df=144)

mean(D$logbmi) + qt(p=0.975, df=144) * ((sd(D$logbmi))/(sqrt(145)))
mean(D$logbmi) - qt(p=0.975, df=144) * ((sd(D$logbmi))/(sqrt(145)))
###########################################################################
## One-sample t-test

n <- length(D$logbmi)
n
sd(D$logbmi)
mean(D$logbmi) 
t_obs <- (mean(D$logbmi) - log(25)) / (sd(D$logbmi) / sqrt(n))
t_obs


pvalue <- 2 * (1-pt(abs(t_obs), df=n-1))
pvalue

## Testing hypothesis mu=log(25) for log-transformed BMI
t.test(D$logbmi, mu=log(25))



###########################################################################
## CI's for the mean and median



## Compute CI for mean log-BMI score
KI <- t.test(D$logbmi, conf.level=0.95)$conf.int
KI
## "Back-transform" to get a CI for median BMI score
exp(KI)

## Consider data for women only
Dfemale <- subset(D, gender == 0)
## Compute CI for mean log-BMI score of a woman
KI <- t.test(Dfemale$logbmi, conf.level=0.95)$conf.int
KI
## "Back-transform" to get a CI for median BMI score of a woman
exp(KI)


###########################################################################
## Welch t-test for comparing two (independent) samples

## Comparison of mean logBMI for women and men
t.test(D$logbmi[D$gender == 0], D$logbmi[D$gender == 1])


###########################################################################
## Computing correlations

## Computing correlations between selected variables
cor(D[,c("weight","fastfood","bmi")], use="pairwise.complete.obs")


###########################################################################
## Subsets in R
  
## Optional extra remark about taking subsets in R
##
## A logical vector with a TRUE or FALSE for each value 
## of a column in D, e.g.: Find all women in the data
D$gender == 0
## Can be used to find all the data for women
D[D$gender == 0, ]
## Alternatively, use the 'subset' function
subset(D, gender == 0)
## More complex logical expressions can be made, e.g.:
## Find all women who weigh less than 55 kg
subset(D, gender == 0 & weight < 55)


###########################################################################
## More R tips

## Use a 'for'-loop to calculate the summary statistics
## and assign the result to a new data.frame
Tbl <- data.frame()
for(i in 0:1){
  Tbl[i+1, "mean"] <- mean(D$bmi[D$gender == i])
  Tbl[i+1, "var"] <- var(D$bmi[D$gender == i])
}
row.names(Tbl) <- c("Women","Men")
## View the contents of Tbl
Tbl

## In R there are also more condensed ways to do such calculations.
## For example,
aggregate(D$bmi, by=list(D$gender), function(x){ 
  c(mean=mean(x), var=var(x)) 
})
## See more useful functions with: ?apply, ?aggregate and ?lapply
## For extremely efficient data handling see, e.g., the packages: 
## dplyr, tidyr, reshape2 and ggplot2

## LaTeX tips:
##
## The R package "xtable" can generate LaTeX tables written to a file 
## and thereby they can automatically be included in a .tex document.
## 
## The R package "knitr" can be used very elegantly to generate .tex 
## documents with R code written directly in the document. This 
## document and the book were generated using knitr.
