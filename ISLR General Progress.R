# Clear plots and objects
dev.off()                # But only if there IS a plot
rm(list = ls())          # Clear environment remove all objects

######### Loading some libraries first ####
if (!require("ISLR")) install.packages("ISLR") # this is the package used to go along with the ISLR book
if (!require("GGally")) install.packages("GGally")

if (!require("ggplot2")) install.packages("ggplot2")
# once a lib with data is loaded, R knows the dataset and can work on it, e.g. when ISLR is loaded, the Auto dataset is available for use
library(GGally, include.only = 'ggpairs') # include only one function from GGally (in this case, the function needs ggplot2 to be loaded too)
lapply(c("MASS", "tidyverse", "ISLR", "car", "ggcorrplot", "psych", "class", "boot"), 
       require, character.only = T) # to load multiple packages at once

######### ISLR BOOK Specific Progress ########
AutoData = Auto # store the Auto dataset from ISLR package into a dataframe
attach(AutoData)
fix(AutoData) # to see the data in a spreadsheet form and can edit values in it
dim(AutoData)
colnames(AutoData) # to see the names of the columns
names(AutoData) # to see the names of the variables (same as columns)
AutoData <- na.omit(AutoData)  # removes any rows with missing data (NA data) even if it's only in one column
head(AutoData)
summary(AutoData)
summary(weight)
lapply(AutoData[,1:8], summary) # to apply a function over a list or vector or dataframe, etc.

plot(AutoData$cylinders, AutoData$mpg) # at this point, R considers Cylinders variable to be quantitative
# Attach () is used to make database attach to the R search path. R would search the database when evaluating a variable, so objects in the database can be accessed by simply giving their names.
# so once we use the attach() function, we are telling R to use the column names in the dataset as known variables so we can say plot(cylinders,mpg)
attach(AutoData)  
plot(mpg ~ as.factor(cylinders)) # considering cylinders as a factor variable than continuous
as.factor(cylinders) # if we look at something as a class/factor what would be the levels/classes/categories?
cylinders <- as.factor(cylinders) # create a stand alone variable for cylinders but with a Factor type. note, the same column inside the database is still continuous quantitative
AutoData$origin <- as.factor(AutoData$origin) # to make a column be known as a factor variable inside the dataset
plot(cylinders) # since now cylinders is a categorical variable, plot() will plot a bar plot
plot(cylinders, mpg) # since now cylinders (a standalone new variable) is categorical and mpg is continuous plot() will do a box plot (boxes of y values for every class of x)
plot(AutoData$origin, weight) # weight is a class variable but is a column in the original dataset
plot(cylinders, mpg, 
     col = "magenta",
     varwidth = T, # Set as true to draw width of the box proportionate to the sample size (sample of y for a given category)
     xlab = "Cylinders",
     ylab = "Miles Per Gallon",
     #     horizontal = T, # to do a horizontal boxplot but need to be careful with this one to not mess with the labels
    )
pairs(AutoData[,c(1,4,6)]) # scatter plot of every pair for the given variables
plot(AutoData[,c(1,4,6)], col=2) # scatter plot of every pair for the given variables

# if we want to identify a point on the graph, we use identify(), click on the point, and esc. the printed output number is the row number for that observation
plot(mpg ~ horsepower, col = 2)
identify(mpg ~ horsepower, 
         #label = weight, # once the point is clicked and identified, weight for that point will be shown on the graph
         labels = paste0("(", horsepower, ", ", mpg, ")"),
         plot = T, 
         atpen = T, 
         offset = 0.0025) # my wasteful attempt to get the label next to the identified point!

# adding regression lines to chart - linear model (linear relationship)
library(ggplot2)
linereg <- lm(mpg ~ horsepower, data = AutoData)  # simple linear regression using liear function for mpg vs horsepower
coeff = coefficients(linereg) 
eq = paste0("y = ", round(coeff[2], 1), "*x ", round(coeff[1], 1))  # putting equation into characters for printing
plot(mpg ~ horsepower, col = 2, main = eq)
abline(linereg, col = "darkgreen", lwd = 2)  # adding the regression line to the plot
abline(h = mean(mpg), col='blue', lty=2, lwd=2)  # adding the mean of response to the plot

# examining the residuals
linereg.results = data.frame(horsepower.x = AutoData$horsepower # creating a table of reg output results for each data point 
                             , mpg.y = AutoData$mpg
                             , mpg.yhat = linereg$fitted.values
                             , linereg.res = linereg$residuals
                             , res.manual = linereg$fitted.values - AutoData$mpg
                             )

head(linereg.results)
ggplot(linereg.results, aes(x = linereg.results$horsepower.x,  # plot of residuals vs. x
                            y = linereg.results$res.manual )) +
              geom_point() +
              xlab("Horsepower") +
              ylab("Residuals")
library(plotly) ; ggplotly() # Convert the recent graph to interactive plot so info can be seen by cursor on points


ggplot(linereg.results, aes(x = linereg.results$mpg.yhat,  # plot of residuals vs. fitted values
                            y = linereg.results$linereg.res)) +
                            geom_point() +
                            xlab("Horsepower") +
                            ylab("Residuals on simple linear regression mpg vs horsepower ") +
                            stat_smooth(method = "lm",
                                        formula = y ~ poly(x, 2, raw = T), # have to use y and x (define them first in aes)
                                        se = F,
                                        col = "darkblue")

summary(lm(linereg.res ~ poly(mpg.yhat, 2, raw = T), data = linereg.results)) # fitting a quad model to residuals vs. fitted

# adding regression lines to chart - linear model (non-linear relationship / polynomial regression)
nonlinereg <- lm(mpg ~ poly(horsepower, 2, raw = T), data = AutoData)  # linear regression using non-linear function for mpg vs horsepower, if RAW is true, use raw and not orthogonal polynomials (we want this as true)
summary(nonlinereg)
coeff2 = coefficients(nonlinereg) 
eq2 = paste0("y = ", round(coeff2[3], 1), "*x^2 ", round(coeff2[2], 1), "*x +", round(coeff2[1], 1))  # putting equation into characters for printing
plot(mpg ~ horsepower, col = 2, main = eq2)
abline(h = mean(mpg), col='blue', lty=2, lwd=2)  # adding the mean of response to the plot

y <- mpg # redefining them due to variables conflicts with functions and crap from libraries!
x <- horsepower
ggp <- ggplot(AutoData, aes(x, y)) + 
        geom_point() +
        xlab("Horsepower") +
        ylab("Miles per Gallon")

ggp +                           # Add polynomial regression curve to the existing scatterplot made as ggp
  stat_smooth(method = "lm",
              formula = y ~ poly(x, 1, raw = T),  # using poly(dependent variable, polynomial degree, use the raw data)
              se = T,           # to display confidence interval around smooth
              color="purple",
              linetype  =2,
              size = 1.5) +
  stat_smooth(method = "lm",
              formula = y ~ poly(x, 2, raw = T),
              se = FALSE,
              col = "darkgreen") +
  stat_smooth(method = "lm",
              formula = y ~ poly(x, 5, raw = T),
              se = FALSE)




lapply(AutoData, class) # to apply a function over a list or vector or dataframe, etc. in this case, I need to know the type of variables (to show this in a line use t())
AutoData$cylinders <- as.factor(AutoData$cylinders) # changed the cylinders column from numeric to factor
unique(AutoData[,2])





####@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
####@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
######### Work with Credit data ########@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
library(ISLR)
library(GGally, include.only = 'ggpairs')
head(Credit)
Credit <- Credit
attach(Credit) # so R will consider the columns as known variables

pairs(Credit[ , c(2:7, 12)],
      col = "dark red",
      pch = 19, # Change shape of points
      cex = 0.5, # Change size of points
      #labels = c("var1", "var2", "var3"), # Change labels of diagonal/ goes with the columns order
      main = "Pairs scatter plot for quantitative variables in Credit dataset") 

ggpairs(Credit, # a much better looking pairs graph with additional features (using GGally AND ggplot2 packages)
        columns = c(2:4,7, 12), # choosing specific columns of the data
        ) 

### examining regression with interaction term between quant and qual variables
plot(Student, Balance) # just to have a quick look
plot(Credit$Balance ~ Credit$Income, 
     col = c("red", "blue")[Credit$Student], # choose colors by levels of factor in the third variable (blue for student:Yes)
     pch = c(1, 2)[Credit$Student] # choose point shape by levels of factor in the third variable
     )
identify(Credit$Balance ~ Credit$Income,  # to double check on some of the data points in the chart!
         labels = paste0("(", round(Credit$Income, 2), ", ", round(Credit$Balance, 2), ")")
         )

# add some regression line to the existing chart generated above (this is very handy)
abline(lm((Credit$Balance[Credit$Student == "No"] ~ Credit$Income[Credit$Student == "No"])), col = "red", lwd = 3)
abline(lm((Credit$Balance[Credit$Student == "Yes"] ~ Credit$Income[Credit$Student == "Yes"])), col = "blue", lwd = 2)

# conduct multiple regression with interaction term
BalVSIncomeStudent.lm <- lm(Balance ~ Income + Student + Income*Student, data = Credit)
summary(BalVSIncomeStudent.lm)

# changing student to a dummy 0/1 variable instead of default factor yes/No variable. Nothing changes!
Student <- as.character(Credit$Student)
Student[Credit$Student == "No"] <- 0
Student[Credit$Student == "Yes"] <- 1
Student <- as.factor(Student)
plot(Student, Balance) # just to have a quick look
BalVSIncomeStudent.lm <- lm(Balance ~ Income + Student + Income*Student, data = Credit)
summary(BalVSIncomeStudent.lm)



####### Grouping data and use it in graphing #######
# create grouping categories
group <- NA
group[Ethnicity == "Caucasian"] <- 1 # could do this using strings of their name too
group[Ethnicity == "African American"] <- 2
group[Ethnicity == "Asian"] <-3
table(group)

pairs(Credit[ , 2:4],
      col = c("red", "Dark blue", "purple")[group],
      pch = c(15, 17, 19)[group], # Change shape of points
      cex = 0.9, # Change size of points
      main = "Pairs scatter plot for quantitative variables in Credit dataset") 


###### Checking some specific relationships of interest ######
summary(Balance)
plot(Gender, Balance)
x = tapply(Balance, Gender, summary) # to get summary stat 
cbind(x$Female, x$` Male`, deparse.level = 2)


#---
########## Regression balance onto gender (categorical or factor variable with 2 levels) ######
# default way to do it (automated dummy variable creation - R does it by sorting the variable, first one gets the 0 (here male is coded as " male")
balgen.lm = lm(formula = Balance ~ Gender, data = Credit) 
summary(balgen.lm)

# creating a dummy for the factor variable (Gender): call it Female and say it's a 1 (Yes) or 0 (No)
Credit[, 13] = NA
head(Credit)
names(Credit)[names(Credit) == 'V13'] <- "Female" # changing the column name from the created dummy variable name to something sensible
Credit[, 13][Gender == "Female"] <- 1
Credit$Female[Gender == " Male"] <- 0

balgen.lm = lm(formula = Balance ~ Credit$Female, data = Credit) 
summary(balgen.lm)

# creating a dummy for the factor variable (Gender): call it 1 for Female and -1 for Male
Credit[, 13] = NA
head(Credit)
names(Credit)[names(Credit) == 'Female'] <- "GenderDummy"
Credit[, 13][Gender == "Female"] <- 1
Credit[, 13][Gender == " Male"] <- -1

balgen.lm = lm(formula = Balance ~ Credit$GenderDummy, data = Credit) 
summary(balgen.lm)


########## Regression balance onto ethnicity (categorical or factor variable with >2 levels) ######
# default way to do it
baleth.lm = lm(formula = Balance ~ Ethnicity, data = Credit) 
summary(baleth.lm)


# creating 2 dummy variables for the factor variable (ethnicity) (mimicking the automated dummies as R does it by sorting it): 
# First one, call it Asian, 1 if Asian 0 otherwise
# Second one, call it Caucasian, 1 if Asian 0 otherwise
# We will have three values for  
Credit[, 14] <- 0
names(Credit)[names(Credit) == 'V14'] <- "Asian" # changing the column name from the created dummy variable name to something sensible
Credit[, 14][Ethnicity == "Asian"] <- 1
Credit[, 15] <- 0
names(Credit)[names(Credit) == 'V15'] <- "Caucasian" # changing the column name from the created dummy variable name to something sensible
Credit[, 15][Ethnicity == "Caucasian"] <- 1
head(Credit, 10)

baleth.lm = lm(Balance ~ Credit$Asian + Credit$Caucasian, data = Credit) 
summary(baleth.lm)


####@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
######### Ch 2 - Ex 9  - Additional work with auto data ########
range(AutoData[,1]) # range of a numerical continuous (quantitative) variable
lapply(AutoData[,c(1,3:7)], range) # getting the range of values for only quantitative variables (lapply error if factors are included)
sapply(AutoData[,c(1,3:7)], range) # does the same as lapply but in a vector form than as a list form (in lapply)

# find mean and sd of the quantitative variables
range_values = t(sapply(AutoData[,c(1,3:7)], range))
colnames(range_values) <- c("low Range", "high range")
mean_values = sapply(AutoData[,c(1,3:7)], mean)
SD_values = sapply(AutoData[,c(1,3:7)], sd)
Mean_SD_Range = data.frame(mean_values, SD_values, range_values) ; rm(mean_values, SD_values, range_values) 
print(Mean_SD_Range)

# find mean and sd if some observations are removed
AutoData_rev <- AutoData[c(1:9, 85:nrow(AutoData)), ] # removing observations 10th through 85th
range_values = t(sapply(AutoData_rev[,c(1,3:7)], range))
colnames(range_values) <- c("low Range", "high range")
mean_values = sapply(AutoData_rev[,c(1,3:7)], mean)
SD_values = sapply(AutoData_rev[,c(1,3:7)], sd)
Mean_SD_Range = data.frame(mean_values, SD_values, range_values) ; rm(mean_values, SD_values, range_values) 
print(Mean_SD_Range)

# find relationship between mpg and other variables
pairs(AutoData[, 1:7]) # overview using pairs() and then a nice looking set of plots
par(mfrow = c(2, 3))
plot(cylinders, mpg, xlab = "cylinders", ylab = "mpg")
plot(weight, mpg)
plot(horsepower, mpg)
plot(displacement, mpg)
plot(acceleration, mpg)
par(mfrow = c(1, 1))
dev.off()


####@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
######### Ch 2 - Ex 8  - work with college data ########
library(ISLR) 
?College # see what the variables are and get to know about the dataset
collegedata <- College
head(collegedata)
names(collegedata)
summary(collegedata)
fix(collegedata)
pairs(collegedata[,1:10])

attach(collegedata)
plot(Private, Outstate, 
     xlab = "Is School private?",
     ylab = "Out of State Tuition",
     )

# let's divide the schools based on their number of top10percent being more or less than 50%
Elite = rep("No", nrow(collegedata)) # create a vector called Elite with the same size as the dataset with "No" as all the values
Elite[Top10perc > 50] = "Yes" # Top10per > 50 creates a vector with logical true/false values, we say make Elite vector be equal to yes where condition is met (i.e. is tur)
Elite <- as.factor(Elite) # making the vector as a factor data (having only two classes)
collegedata <- data.frame(collegedata, Elite) # adding it to the end of the main dataset
summary(Elite)
plot(Private, Elite)
table(Private, Elite) # a frequency table that shows the numbers of observation in each combination

par(mfrow=c(2,2)) # divide screen into a 2 by 2 window for graphs
hist(Enroll, breaks = 50)
par(mfrow=c(1,1)) # restore the plot window to a single view
dev.off()


######### ISLR Ch2 - Ex10 - Work with Boston housing data ##############
library(MASS)
?Boston
attach(Boston)
pairs(Boston, pch=",")
fix(Boston)
sapply(Boston, class)
sapply(Boston, range)
summary(Boston)
nrow(Boston[crim > 5,])
z <- Boston[crim > 5,]
plot(z$tax, z$crim)
nrow(Boston[chas == 1,])
median(ptratio)
plot(medv)
plot(Boston$medv[medv < 10]) # plot medv by its index for only were it is less than 10
identify(Boston$medv[medv < 10], # identify the points with low medv from the plot
         label=crim,
         offset = 10)
Boston[medv == min(medv),] # show the data for the lowest median home value
c(HomesW7pluroom = nrow(Boston[rm > 7, ]), HomesW8pluroom = nrow(Boston[rm > 8, ])) # find number of towns with more than 7 or 8 rooms in the building



#### ADVERTISING DATA ####
#### Mainly following https://rstudio-pubs-static.s3.amazonaws.com/249959_d71491a56f8242909331dfee0e25b813.html
library(MASS)
library(ISLR)
wd <- getwd()
file = paste0(wd, "/ISLR Advertising Data.csv")
ads <- read.csv(
  file,
  header = T, # file contains the names of the variables as its first line
  check.names = T, # to check the names of the variables to ensure syntactical validity; If necessary they are adjusted and to ensure that there are no duplicates. 
  blank.lines.skip = T  # to ignore the blank lines in the input
)[,-1] # without the first column (telling R to remove the 1st column - it's just index)
attach(ads)
head(ads)


########### Initial Work - Exploration
summary(ads)
pairs(ads, pch = ".")

plot(ads$TV, ads$Sales, 
     xlab = "TV", 
     ylab = "Sales (in thousands)")

########### Is there a relationship between advertising and budget?
# trying linear model: lm(formula, dataframe) e.g. lm(y ~ x^2, mydata) where my data has a column named x and one named y
# Tilde (~) is used to separate the left- and right-hand sides in a model formula. 
# linear models of Sales on individual explanatory variables
SalesTV.lm <- lm(formula = Sales ~ TV, data=ads) ; summary(SalesTV.lm)
SalesRadio.lm <- lm(Sales ~ Radio, data=ads) ; summary(SalesRadio.lm)
SalesNews.lm <- lm(Sales ~ Newspaper, data=ads) ; summary(SalesNews.lm)

####### multiple regression model of sales onto TV, radio, and newspaper
# The dot (.) is to follow no specific model; so this is saying to model sales by all in the dataset (same as TV + Radio + Newspaper)
# Test hypothesis H0 : beta_TV = beta_radio = beta_newspaper = 0 (to test none of the explanatory variable has any effect on Sales)
# F-test on the multiple regression model indicates clear evidence of a relationship between advertising and sales (small p-value)
SalesTVRadio.lm <- lm(Sales ~ TV + Radio, data=ads)
SalesAll.lm <- lm(Sales ~ ., data=ads); summary(SalesAll.lm)

########### How strong is the relationship in the model?
# Let's examine this using residual standard error (RSE) and R-squared (RSQ) values. Here is a very good read: https://statisticsbyjim.com/regression/standard-error-regression-vs-r-squared/
# Good notes for RSS, RSE, MSE, RMSE here: https://stats.stackexchange.com/questions/110999/r-confused-on-residual-terminology
# the summary of the model is a list of many model summaries, one of them is sigma which is the RSE (not accessible from the model properties)
# notice the RSE is not the same as the residual sd. They are two different things in nature.
SalesAll.lm.residuals = SalesAll.lm$residuals # to store the model residuals into a list (once the model is run, it creates a list object with many properties/columns)
sd(SalesAll.lm.residuals)
RSE = summary(SalesAll.lm)$sigma
RSE / mean(Sales) * 100  # this provides an idea on the residual variation to overall response mean
RSQ = summary(SalesAll.lm)$r.sq 
cat("Multiple regression model RSE = ", round(RSE, 3), "\nMultiple regression model R-squared = ", round(RSQ, 3)) # print a concatenation without showing in character form (\n is for next line)

### calculating the R-squared using its formula manually
# yhat = SalesAll.lm$fitted.values
# y = Sales
# ybar = mean(y)
# rsq=1-sum((y-yhat)^2)/sum((y-ybar)^2)
# var(yhat)/var(y) # this is also equal to rsq in linear regression
# cor(yhat, y)^2 # this is also equal to rsq in linear regression

########### Which media contribute to sales?
# let's look closer to the coefficient estimates of the model to see their values and significance
(coeffs = summary(SalesAll.lm)$coefficients) # using () runs and print the results in console 

# plotting a tornado plot to visualize the various coefficients. More options in here: https://cran.r-project.org/web/packages/tornado/vignettes/tornadoVignette.html
# types can be percentchange, ranges, percentiles
install.packages("tornado")
library(tornado)
torn1 <- tornado::tornado(SalesAll.lm, type = "percentiles", alpha = 0.05)
plot(torn1, 
     xlabel = "Sales", 
     geom_bar_control = list(width = 0.4),
     sensitivity_colors = c(2, "#69BE28"), # choosing two colors to express the effects of each variable
)

########### How large is the effect of each medium on sales?
confint(SalesAll.lm, level = 0.95) # this is the easy and quick way for a 95% confidence level (alpha = 5%)

# could collinearity be the reason for the wide CI for newspaper? (can use vif() function from library "car")
# if no collinearity found, separate simple linear regressions can help see which one is stronger, etc.
install.packages("car")
require(car)
vif(SalesAll.lm) # VIF values for each model predictors - In practice, a VIF > 5 or 10 indicates a problematic collinearity

# or to do it manually to understand:
# CI = estimate +- critical standard normal z-score for the CL * standard error of the estimate
# a 95% CI means that 95% certainty that the estimate between value of estimate +- 1.96 standard deviation on each side. 
# The area under the curve covers 95% of the probability that the true value falls in this interval
# here is a good reference: https://bookdown.org/curleyjp0/psy317l_guides5/confidence-intervals.html
# also, here are additional useful functions for finding probability related values in distributions: https://sphweb.bumc.bu.edu/otlt/MPH-Modules/BS/R/R-Manual/R-Manual10.html
alpha = 0.05
critZ = qnorm(1 - alpha/2, mean = 0, sd = 1) # To find a critical Z-value from standard normal (z) distribution (mean=0, sd = 1) for a two-tailed 95% confidence interval
CI.LowerLimit = coeffs[, 1] - critZ*coeffs[, 2]
CI.UpperLimit = coeffs[, 1] + critZ*coeffs[, 2]
CI <- cbind(CI.LowerLimit, CI.UpperLimit)  # to bind them in column form (so the values are next to each other) rbind can be used to bind in rows
rm(alpha, critZ, CI.LowerLimit, CI.UpperLimit)

grid(nx=20, ny=20)
boxplot(t(CI[-1,]), # putting CI into graphs to visually see in a box chart (removed intercept from plot)
        beside = T, # to keep them beside each other - most useful in bar charts
        col = "light green",
        main = "Confidence Interval for the predictor coefficients",
) 

########### How accurately can we predict future sales?
# the answer to how accurately we can estimate the response depends on whether we want to predict individual responses (use prediction interval Y = f(X) + e), 
# or if we want to predict the average response, f(X) (use confidence interval)
# Confidence interval is used for average value of response over a group of observations with similar characteristics and prediction interval is used for a single observation

predict(SalesAll.lm, 
        newdata = data.frame(TV=149, Radio=22, Newspaper=25), # any new data point
        interval = "confidence") # for the average  response f(X)

predict(SalesAll.lm, 
        newdata = data.frame(TV=149,Radio=22,Newspaper=25),
        interval = "prediction") # for individual response

########### Is the relationship linear?
plot(TV, SalesAll.lm.residuals)  # simple plot of residuals vs TV
plot(SalesAll.lm) #diagnostic plot (Hit return to see next plot: click on console and press enter)


########### Is there synergy among the advertising media?
# Investigating the Interaction Effect between TV and Radio
SalesTVRadioInt.lm <- lm(Sales ~ TV + Radio + TV*Radio, data=ads) ; summary(SalesTVRadioInt.lm)

# Investigating every possible interaction in the model
SalesAll.lm2 <- lm(Sales~.^2, data = ads) ; summary(SalesAll.lm2)
cat("R^2 for model with no Interaction terms =", summary(SalesAll.lm)$r.sq, 
    "\nR^2 for model with all Interaction terms ", summary(SalesAll.lm2)$r.sq)


######### Sec 3.6.2 Simple Linear Regression ################################
options(show.signif.stars=T)      # if False, removes the stars from regression output showing significance
lm(medv~lstat, data = Boston)
lm.fit = lm(medv~lstat)                 # if we attach the dataset first
summary(lm.fit)
names(lm.fit)                           # to see what other information is stored in the fit variable after regression
lm.fit$coefficients                     # extracting quantities from the fit by their name
coef(lm.fit)                            # safer way of extracting quantities from the fit using extractor functions
confint(lm.fit)                         # confidence interval for the estimates in the fit

# to use the model for predicting response on new data points
predict(lm.fit, 
        data.frame(lstat=c(5, 10, 15)), # new data points
        se.fit = F,                     # to require the stdev for each prediction point
        interval = "confidence",        # to ask for confidence or prediction intervals on predicted responses
        level = 0.95)

plot(lstat, medv, pch = "+")
abline(lm.fit, col = "blue", lwd=2)     # to add the fit line onto the current plot (used for straight lines) (lwd to make it twice as thick)
plot(lstat, medv, pch = 1:20)
abline(lm.fit, col = "red", lwd=3)

# diagnostics plots - plot(lm.fit) does 4 of them at once needing to Enter for each. 
plot(lm.fit)
par(mfrow = c(2,2)); plot(lm.fit); par(mfrow = c(1,1))  # to see all 4 diagnostics plots in the same chart
residuals(lm.fit)
rstudent(lm.fit)                        # to get the studentized residuals 
plot(predict(lm.fit), rstudent(lm.fit)) # to plot any diagnostics manually using the data (here: studentized residuals vs. fitted values)
hatvalues(lm.fit)                       # to get the leverage statistics for the model (min is 1/n, average is (p+1)/n)
hist(hatvalues(lm.fit), breaks = 40)
which.max(hatvalues(lm.fit))            # to find the index of the observation with maximum leverage
Boston[which.max(hatvalues(lm.fit)), ]   # see the observation with max leverage
boxplot(lstat, horizontal = T)
library(car); Boxplot(lstat, id = list(n=2), method = "identify") # to see the leverage point in a boxplot for predictor


######### Sec 3.6.3 Multiple Linear Regression ################################
library(MASS)
library(tidyverse)
library(car)
attach(Boston)
pairs(Boston)
names(Boston)

options(show.signif.stars=FALSE)      # removes the stars from regression output showing significance
Boston %>%
      lm(medv ~ lstat + age, data = .) %>% 
      summary()

?summary.lm    # see what is available in a summary object of a linear fit to grab them individually e.g. summary.lm$sigma for RSE

# to use all variables in the model
lm.fit = lm(medv ~ ., data = Boston) ; summary(lm.fit) ; message("VIF values are:") ; vif(lm.fit)

# to use all predictors in the model except one
lm.fit = lm(medv ~ .-age, data = Boston) ; summary(lm.fit)

# Z ~ A * B will include both A and B and their interaction term A:B (A:B can be separately used to model against too)
lm.fit = lm(medv ~ lstat * age, data = Boston) ; summary(lm.fit)

# I() is used for non-linear transformation of predictors (which is used in a lienar regression)
lm.fit = lm(medv ~ lstat + I(lstat^2)) ; summary(lm.fit)

# How much is better than the linear fit the quadratic function is (to compare the two models)
# let's use ANOVA(): Null: both model are the same, Alt: full model is superior
lm.fit1 = lm(medv ~ lstat)  ; summary(lm.fit1)
lm.fit2 = lm(medv ~ lstat + I(lstat^2)) ; summary(lm.fit2)
anova(lm.fit1, lm.fit2)

par(mfrow = c(2,2))
plot(lm.fit1)

lm.fit = lm(medv ~ poly(lstat, 5)) # to fit a 5th order polynomial function
summary(lm.fit)
plot(lm.fit)

lm.fit = lm(medv ~ log(rm)) # to fit to a log transformation of x in a linear reg
summary(lm.fit)
plot(lm.fit)

lm.fit = lm(medv ~ black*crim + lstat + poly(age, 2) + log(rm)) # a mix multiple reg
summary(lm.fit)
plot(lm.fit)
par(mfrow = c(1,1))

####@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
######### Sec 3.6.6 Qualitative Predictors (Carseats Dataset) #############
?Carseats
attach(Carseats)
Carseats %>% 
      mutate(ShelveLoc = factor(ShelveLoc,
                                levels = c("Bad", "Medium", "Good"))) %>% 
      boxplot(Sales ~ ShelveLoc, data = ., col = "gold")

# regression of sales onto all variables and a couple interaction terms
# for categorical variables, R automatically creates dummy variables and does the job
lm.fit = lm(Sales ~ . + Income:Advertising + Price:Age, data = Carseats)
summary(lm.fit)

# to see the coding that R uses to create dummy variables for categorical variables:
contrasts(ShelveLoc)


####@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
######### Ch 3 - Ex 8  - Additional work with auto data ########
str(Auto)
attach(Auto)
plot(Auto$mpg ~ horsepower)
lm.fit = lm(Auto$mpg ~ horsepower, data = Auto); summary(lm.fit)
abline(lm.fit)
predict(lm.fit, 
        newdata = data.frame(horsepower = 98),
        interval = "prediction",
        level = 0.95)
points(x = 98, y=24.47, pch = "*", cex = 5, col = "red")
par(mfrow=c(2,2)) ; plot(lm.fit) ; par(mfrow=c(1,1))

######### Ch 3 - Ex 9  - Continue on 8 ########
Auto %>% 
      select(-name, -cylinders, -origin, -year) %>% 
      pairs()

# correlation between variables
library("ggcorrplot")
Auto %>% 
      dplyr::select(-name) %>% 
      cor() %>% 
      round(2) %>% 
      ggcorrplot(hc.order = TRUE, 
                 type = "lower", 
                 lab = TRUE, 
                 lab_size = 3, 
                 method="square",     # can use circle instead
                 colors = c("tomato", "white", "darkgreen"), 
                 title="Correlogram of mtcars", 
                 ggtheme=theme_bw)

Auto %>% 
      select(name)
# or
library(psych)
Auto %>% 
      dplyr::select(-name, -year, -origin) %>% 
      pairs.panels(
             smooth = TRUE,      # If TRUE, draws loess smooths
             scale = FALSE,      # If TRUE, scales the correlation text font
             density = TRUE,     # If TRUE, adds density plots and histograms
             ellipses = TRUE,    # If TRUE, draws ellipses
             method = "pearson", # Correlation method (also "spearman" or "kendall")
             pch = 21,           # pch symbol
             lm = FALSE,         # If TRUE, plots linear fit rather than the LOESS (smoothed) fit
             cor = TRUE,         # If TRUE, reports correlations
             jiggle = FALSE,     # If TRUE, data points are jittered
             factor = 2,         # Jittering factor
             hist.col = 4,       # Histograms color
             stars = TRUE,       # If TRUE, adds significance level with stars
             ci = TRUE)          # If TRUE, adds confidence intervals

# multiple linear regression: mpg on all but name
lm.fit2 <- lm(mpg ~ . -name, data = Auto); summary(lm.fit2)
par(mfrow=c(2,2)) ; plot(lm.fit) ; par(mfrow=c(1,1))

# checking on some interaction effects and transformation of predictors
lm.fit3 <- lm(mpg ~ . -name + 
                   weight:acceleration +
                   weight:origin +
                   cylinders:weight +
                   I(displacement^2) +
                   I(horsepower^2) +
                   log(horsepower) + 
                   sqrt(acceleration),
                   data = Auto); summary(lm.fit3)


######### Ch 3 - Ex 10  - Carseats Regression ########
names(Carseats)
attach(Carseats)
# visualizing price vs binaries of Urban and US
ggplot(Carseats, 
       aes(x=Urban, 
           y=Price, 
           fill=US)) + 
      geom_boxplot()

# Visualizing Sales vs Price and binaries of US and Urban
ggplot(Carseats, 
       aes(x = Price, 
           y = Sales)) + 
      geom_point(aes(col = US,
                     size = Advertising)) + 
      geom_smooth(method = "lm", 
                  se = F) + 
      # xlim(c(0, 0.1)) +
      # ylim(c(0, 500000)) +
      labs(subtitle="Sales Vs Price", 
           y="Sales",
           x="Price",
           title="Scatterplot",
           caption = "ISLR: Ch3-Ex9") +
      facet_wrap(~ Urban)

# fit multiple regression 
lm.fit = lm(Sales ~ Price + Urban + US, data = Carseats); summary(lm.fit)

# adjust the fit based on the results
lm.fit = lm(Sales ~ Price + US, data = Carseats); summary(lm.fit)
par(mfrow=c(2,2)) ; plot(lm.fit) ; par(mfrow=c(1,1))

# confidence intervals for the coefficients
confint(lm.fit, level = 0.95)

######### Ch 3 - Ex 11  - t-Statistics Investigation ########
set.seed(1)
x = rnorm(100)
y = 2*x + rnorm(100)

# check t-statistics in the followings and compare and think
# simple linear reg y on x without intercept
lm.fit = lm(y ~ x + 0) ; summary(lm.fit)
plot(x = x, y = y) ; abline(lm.fit) ; points(x = 0, y = 0, pch = 19, col  ="Red", cex = 1.5)

# simple linear reg x on y without intercept
lm.fit = lm(x ~ y + 0) ; summary(lm.fit)
plot(x = y, y = x) ; abline(lm.fit) ; points(x = 0, y = 0, pch = 19, col  ="Red", cex = 1.5)

# simple linear reg y on x wit intercept
lm.fit = lm(y ~ x) ; summary(lm.fit)
plot(x = x, y = y) ; abline(lm.fit) ; points(x = 0, y = summary(lm.fit)$coefficients[1,1], pch = 19, col  ="Red", cex = 1.5)

# simple linear reg x on y with intercept
lm.fit = lm(x ~ y) ; summary(lm.fit)
plot(x = y, y = x) ; abline(lm.fit) ; points(x = 0, y = summary(lm.fit)$coefficients[1,1], pch = 19, col  ="Red", cex = 1.5)

######### Ch 3 - Ex 12.c ########
# for the y~x and x~y to have the same coefficients, one should be a permutation of the other one 
# this is to make sure the SSX and SSY won't change (in coeff calculations) as the model order changes
set.seed(15) ; x = rnorm(10) 
y <- x[order(rnorm(10))] # order(rnorm(100)) # a random list of numbers to cover 1 to 10
df <- data.frame(x, y)
lm.fit = lm(y~x) ; summary(lm.fit) ; plot(x = x, y = y)
lm.fit = lm(x~y) ; summary(lm.fit) ; plot(x = y, y = x)

######### Ch 3 - Ex 13 ########
set.seed(1)
x <- rnorm(100)
eps <- rnorm(100, mean = 0, sd = 0.25)
y = -1 + 0.5* x + eps
length(y)
lm.fit = lm(y ~ x) ; summary(lm.fit)
plot(x, y)
abline(lm.fit, col = "blue") # Regression fit line
abline(a = -1, b = 0.5 , col = "red") # Population line
legend(x=-2, y=0, c("Linear Rregression", "Population Line"), cex=0.9, fill = c(c("blue","red")))
lm.poly = lm(y ~ poly(x, 2)) ; summary(lm.poly)
midnoiseconfint = confint(lm.fit)

# more noise in the data
set.seed(1)
x <- rnorm(100)
eps <- rnorm(100, mean = 0, sd = 1)
y = -1 + 0.5* x + eps
lm.fit = lm(y ~ x) ; summary(lm.fit)
plot(x, y)
abline(lm.fit, col = "blue") # Regression fit line
abline(a = -1, b = 0.5 , col = "red") # Population line
legend(x=-2, y=0, c("Linear Rregression", "Population Line"), cex=0.9, fill = c(c("blue","red")))
morenoiseconfint = confint(lm.fit)

# less noise in the data
set.seed(1)
x <- rnorm(100)
eps <- rnorm(100, mean = 0, sd = 0.05)
y = -1 + 0.5* x + eps
lm.fit = lm(y ~ x) ; summary(lm.fit)
plot(x, y)
abline(lm.fit, col = "blue") # Regression fit line
abline(a = -1, b = 0.5 , col = "red") # Population line
legend(x=-2, y=0, c("Linear Rregression", "Population Line"), cex=0.9, fill = c(c("blue","red")))
lessnoiseconfint = confint(lm.fit)

#
confintcomparison = cbind(lessnoiseconfint, midnoiseconfint, morenoiseconfint)
colnames(confintcomparison) = c("LL @ Low noise", "UL @ Low Noise", "LL @ Mid noise", "UL @ Mid Noise", "LL @ High noise", "UL @ High Noise")

######### Ch 3 - Ex 14 ########
set.seed(1)
x1 = runif(100)
x2 = 0.5*x1 + rnorm(100)/10
y = 2 + 2*x1 + 0.3*x2 + rnorm(100)
plot(x1, x2)
cor(x1, x2)
lm.fit = lm(y ~ x1 + x2) ; summary(lm.fit)
lm.fitx1 = lm(y ~ x1) ; summary(lm.fitx1)
lm.fitx2 = lm(y ~ x2) ; summary(lm.fitx2)
x1 = c(x1, 0.1)
x2 = c(x2, 0.8)
y = c(y, 6)
par(mfrow=c(2,2)) ; plot(lm.fit) ; par(mfrow=c(1,1))

######### Ch 3 - Ex 15 ########
# part a
y <- Boston$crim                  # store response into y variable
CoeffData = data.frame()          # create an empty dataframe to store regression coefficients and their significance levels
for (i in 2:length(Boston)) {     # indexes for assigning values to x from dataframe columns
  x <- unlist(Boston[i])          # store predictor into x
  lm.fit = lm(y ~ x)              # regression of y onto x
  plot(x, y, ylab = "Crime", xlab = colnames(Boston[i])) ; abline(lm.fit, col = i) # generating scatter plots and fits over
  CoeffData = rbind(CoeffData, c(coef(lm.fit), summary(lm.fit)$coefficients[,4]))  # compiling the dataframe to see/compare results in the end
}
rownames(CoeffData) = colnames(Boston[2:length(Boston)]) # naking the rows and columns (below) of the results dataframe for clarity
colnames(CoeffData) = c("Intercept (Crime ~ x)", "Slope (Crime ~ x)", "Intercept p-Value", "Slope P-value")
view(CoeffData)

# part b
lm.fit.poly = lm(Boston$crim ~ ., data = Boston); summary(lm.fit.poly)

# part c
multicoeff <- coef(lm.fit.poly)[2:14]      # slope coefficients from multiple regression into a variable for y
singlecoeff<- CoeffData[,1]                # slope coefficients from individual single regressions into another variable for x
z <- cbind(singlecoeff, multicoeff)        # having a nice matrix to see coefficients next to each other
plot(singlecoeff, multicoeff, pch = 19, col = "red")

# part d
options(scipen=999)               # to remove scientific notation
y <- Boston$crim                  # store response into y variable
CoeffData = data.frame()          # create an empty dataframe to store regression coefficients and their significance levels
for (i in c(2, 3, 5:length(Boston))) {     # indexes for assigning values to x from dataframe columns
  x <- unlist(Boston[i])          # store predictor into x
  lm.fit = lm(y ~ poly(x, 3))     # regression of y onto x / polynomial of degree 3
  summary(lm.fit)
  # plot(x, y, ylab = "Crime", xlab = colnames(Boston[i])) ; abline(lm.fit, col = i) # generating scatter plots and fits over
  CoeffData = rbind(CoeffData, c(coef(lm.fit), summary(lm.fit)$coefficients[,4]))  # compiling the dataframe to see/compare results in the end
}
rownames(CoeffData) = colnames(Boston[c(2, 3, 5:length(Boston))]) # naking the rows and columns (below) of the results dataframe for clarity
colnames(CoeffData) = c("Intercept (Crime ~ x)", "Slope (Crime ~ x)", "Slope (Crime ~ x^2)", "Slope (Crime ~ x^3)", 
                        "Intercept p-Value", "Slope x P-value", "Slope x^2 P-value", "Slope x^3 P-value")
view(round(CoeffData, 6))
# trying an example plot for crim vs Nox
lm.fit = lm(Boston$crim ~ poly(Boston$nox, 3))
par(mfrow=c(2,2)) ; plot(lm.fit) ; par(mfrow=c(1,1))
summary(lm.fit)
plot(Boston$nox,Boston$crim)
abline(lm(Boston$crim ~ Boston$nox), col = "red")
curve(3.6135 + 81.3720*x -28.8286*x^2, from=min(Boston$nox), to=max(Boston$nox), col = "blue", add = T)
curve(3.6135 + 81.3720*x -28.8286*x^2 -60.3619*x^3, from=min(Boston$nox), to=max(Boston$nox), col = "green", add = T)
legend(x=0.4, y=80, c("Linear Rregression", "Multiple Regression Deg 2", "Multiple Regression Deg 3"), cex=0.9, fill = c(c("red","blue", "green")))

######### Ch 4 - Logistic Regression ##############
# Using Default Data to work on
str(Default)

# evaluate visually if balance has a relationship with income, being default, or being a student
boxplot(Default$balance ~ Default$default)    # quick comparison of balance data for default yes and no
Default %>%
  ggplot(aes(x = income,
             y = balance,
             color = default)) +
  geom_point() +
  facet_wrap( ~ student, ncol = 2) +
  theme_classic()

# logistic regression for default vs binary of being student - create dummy variable first (although this is not necessary)
# Default <- Default %>% 
# mutate(binarystudent = if_else(student == "Yes", 1, 0)) 

# run the regression model via generalized linear model, binomial is used for logit function
lm.fit = glm(Default$default ~ Default$student, family = "binomial")
summary(lm.fit)

# Ch 4 - Multiple Logistic Regression
lm.fit = glm(Default$default ~ Default$student + Default$income + Default$balance, family = "binomial")
summary(lm.fit)

######### Sec 4.6.1 - Stock Market Data ##############
glimpse(Smarket)
head(Smarket)
summary(Smarket)
cor(Smarket[,-9]) # all columns but remove the 9th col which is a factor
library("ggcorrplot")
Smarket[,-9] %>%
     cor() %>% 
     round(2) %>% 
     ggcorrplot(hc.order = TRUE, 
                type = "lower", 
                lab = TRUE, 
                lab_size = 3, 
                method="square",     # can use circle instead
                colors = c("tomato", "white", "darkgreen"), 
                title="Correlogram", 
                ggtheme=theme_bw)

boxplot(Smarket$Volume ~ Smarket$Year) # since there was a 0.5 cor for volume and year
plot(Smarket$Volume)

######### Sec 4.6.2 - Logistic on Stock Market ##############
logism.fit <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, 
                  data = Smarket, 
                  family = "binomial") # binomial is used for logit function
summary(logism.fit)
coef(logism.fit)                   # Extracting coefficients of the model only
summary(logism.fit)$coef[2,4]      # Extracting a specific p-value of the model - lag1 in this case

# to predict the probability of market going up, given values of the predictors. 
# type = "response" is sued to get the predicted probabilities P(Y=1|X) as opposed to other outputs (e.g. logit)
glm.probs = predict(logism.fit, 
                    type = "response") # predicting probabilities for the training set (since no test set is provided)
head(glm.probs)
contrasts(Smarket$Direction)  # to see what R will use as 0 and 1, in this case Up is 1, so the probabilities are for market going up.

glm.preds = rep("Down", 1250)           # a vector of all "Down" of size of data
glm.preds[glm.probs > 0.5] = "Up"       # where predicted probability is more than 0.5 change it to up 
table(glm.preds, Smarket$Direction)     # a confusion table that shows how many are predicted truly or falsely (see the two types of error)
mean(glm.preds == Smarket$Direction)    # the fraction of correct predictions out of total, 1 - which = training error rate

# Trying to train on train set and then test on test set
TrainIndex <- (Smarket$Year < 2005)     # taking years 2001-2004 as training set
SmarketTrain <- Smarket[TrainIndex, ]   # for modeling this is not needed due to using Train Index as subset in the model command
SmarketTest = Smarket[!TrainIndex, ]    
dim(SmarketTrain)
dim(SmarketTest)

logism.fit = glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, 
                 data = Smarket, 
                 subset = TrainIndex,
                 family = "binomial")

glm.probs = predict(logism.fit, 
                    newdata = SmarketTest,
                    type = "response")
length(glm.probs)
glm.preds = rep("Down", 252)
glm.preds[glm.probs > 0.5] <- "Up"
table(glm.preds, SmarketTest$Direction)
mean(glm.preds != SmarketTest$Direction)     # test error rate


# trying it with those variables with lowest p-value from original model
logism.fit = glm(Direction ~ Lag1 + Lag2, 
                data = Smarket, 
                subset = TrainIndex,
                family = "binomial")

glm.probs = predict(logism.fit, 
                    newdata = SmarketTest,
                    type = "response")

contrasts(Smarket$Direction)  # to see the dummy variables for up and down
glm.preds = rep("Down", 252)
glm.preds[glm.probs > 0.5] <- "Up"
table(glm.preds, SmarketTest$Direction)
mean(glm.preds != SmarketTest$Direction)

# to predict a particular case where there is a specific lag1 and lag2 values
summary(logism.fit)
predict(logism.fit, 
        newdata = data.frame(
             Lag1 = c(1.2, 1.5),
             Lag2 = c(1.1, -0.8)
             ),
        type = "response")

######### Sec 4.6.3 - LDA on Stock Market ##############
# using the same train and test data from before
lda.fit = lda(Direction ~ Lag1 + Lag2, 
              data = Smarket, 
              subset = TrainIndex)
lda.fit

# "The LDA output indicates that ˆπ1 = 0.492 and ˆπ2 = 0.508; in other words,
# 49.2 % of the training observations correspond to days during which the market went down. 
# It also provides the group means; these are the average of each predictor within each class, and are used by LDA as estimates of μk."

# "The coefficients of linear discriminants output provides the linear
# combination of Lag1 and Lag2 that are used to form the LDA decision rule.
# If −0.642×Lag1 − 0.514×Lag2 is large, then the LDA classifier will
# predict a market increase, and if it is small, 
# then the LDA classifier will predict a market decline."

plot(lda.fit)

lda.pred = predict(lda.fit, SmarketTest) 
names(lda.pred)
# class is the prediction, 
# posterior is a matrix with k columns (classes of response), each column contains the probability that the observation belongs to that class.
# x has the linear discriminants
head(lda.pred$posterior)
head(lda.pred$x)
table(lda.pred$class, SmarketTest$Direction)      # when compared with logistic reg output, they're almost the same.
sum(lda.pred$posterior[,2] > 0.5)                 # total number of predictions for up, which should be seen in the class element of prediction dataframe
sum(lda.pred$posterior[,2] > 0.9)                 # total number of predictions for up when our threshold is based on 90% probability that observation falls in Up class


######### Sec 4.6.4 - QDA on Stock Market ##############
# using the same train and test data from before
options(digits = 4)
TrainIndex <- (Smarket$Year < 2005)          # taking years 2001-2004 as training st
SmarketTest = Smarket[!TrainIndex, ]
qda.fit = qda(Direction ~ Lag1 + Lag2, 
              data = Smarket, 
              subset = TrainIndex)

qda.fit
qda.pred = predict(qda.fit, SmarketTest)     # predicting values from test sample
names(qda.pred)
head(qda.pred$class, 20)
t(head(qda.pred$posterior, 20))
# class is the prediction, 
# posterior is a matrix with k columns (classes of response), each column contains the probability that the observation belongs to that class.

table(qda.pred$class, SmarketTest$Direction)      # when compared with logistic reg output, they're almost the same.
mean(qda.pred$class == SmarketTest$Direction)     # percent of true positive prediction


######### Sec 4.6.5 - KNN on Stock Market ##############
# Needs library(class) and 4 inputs: training data, test data, responses for train data, and value of K
library(class)
TrainIndex <- (Smarket$Year < 2005)          # taking years 2001-2004 as indices for extracting training set
SmarketTrain = Smarket[TrainIndex, 2:3]      # making the training set with only the first two variables
SmarketTest = Smarket[!TrainIndex, 2:3]      # making the test set with only the first two variables
SmarketTrainResponse = Smarket$Direction[TrainIndex]   # making the response vector for the training set

# if several observations are tied as nearest neighbors, R randomly breaks the tie - so let's use a seed for reproducibility purposes
set.seed(1)
knn.pred = knn(SmarketTrain, SmarketTest, SmarketTrainResponse, k = 3) # using various K values, we will see what works best
SmarketTestDirection <- Smarket$Direction[!TrainIndex]
table(knn.pred, SmarketTestDirection)
mean(knn.pred == SmarketTestDirection)


######### Sec 4.6.6 - KNN on Caravan Insurance ##############
glimpse(Caravan)
attach(Caravan)
summary(Purchase)
# because the KNN is influenced by the scale of the variables, we first need to standardize the predictors we have so they'll all have a mean of 0 and sd of 1.
Caravan.St = scale(Caravan[ , -86])          # standardize all predictors and leave the response off (dataframe to matrix)
view(Caravan.St, n=10)

# let's compare some variance and means
library(psych)      # to use the describe function for descriptive stats
describe(Caravan[, 1:4])
describe(Caravan.St[, 1:4])
t(lapply(Caravan[, 1:4], var))
t(lapply(data.frame(Caravan.St[, 1:4]), var))

# creating a train and test set
TestIndex = 1:1000
Caravan.Train = Caravan.St[-TestIndex,]
Caravan.Test = Caravan.St[TestIndex,]
Caravan.Train.Response = Purchase[-TestIndex]
Caravan.Test.Response = Purchase[TestIndex]

dim(Caravan.Test)
dim(Caravan.Train)
length(Purchase)
Purchase[1:1000,]

# fitting the KNN fit to train set and testing it on the test set
set.seed(1)
knn.pred = knn(Caravan.Train, Caravan.Test, Caravan.Train.Response, k=1)
table(knn.pred, Caravan.Test.Response)
mean(Caravan.Test.Response != knn.pred)      # error rate = percentage predicted incorrectly 
mean(Caravan.Test.Response == "Yes")         # percent of yes in the test response

# 11.8% overall error rate too high, if all were predicted No, error would have been just 5.9% - but is this of interest in this analysis?
# Interest here is the percent of those who are truly predicted as a Yes (to spend money/effort to sell them insurance), which is 9/(9+68) = 11.7%
# the 11.7% true out of total positive prediction is the interest here. Model predicted 68 (false positive) + 9 (true positive). This is double the random guessing rate
# This page was interesting on this subject: https://classeval.wordpress.com/introduction/basic-evaluation-measures/

# Lets try k = 3 and higher (with k = 5, the success of positive response prediction is a lot higher than random)
set.seed(1)
knn.pred = knn(Caravan.Train, Caravan.Test, Caravan.Train.Response, k=5)
table(knn.pred, Caravan.Test.Response)
mean(Caravan.Test.Response != knn.pred)      # error rate = percentage predicted incorrectly 
mean(Caravan.Test.Response == "Yes")         # percent of yes in the test response

# for comparison, let's fit a logistic model to this data
logism.fit = glm(Purchase ~ ., 
                 data = Caravan, 
                 subset = -TestIndex,
                 family = binomial)

glm.probs = predict(logism.fit, 
                    newdata = Caravan[TestIndex,],
                    type = "response")
length(glm.probs)
contrasts(Purchase)
glm.preds = rep("No", 1000)
glm.preds[glm.probs > 0.5] <- "Yes"
table(glm.preds, Purchase[TestIndex])
mean(glm.preds != Purchase[TestIndex])     # total test error rate better than KNN but doesn't matter much really! although, still higher than random

# what of we change the probability of Yes to higher than 0.25 instead of 0.5 - it makes a big difference 11/33 as compared to random: 5.9%
glm.preds = rep("No", 1000)
glm.preds[glm.probs > 0.25] <- "Yes"
table(glm.preds, Purchase[TestIndex])

######### Ch 4 - Ex 10 ########
view(Weekly)
attach(Weekly)
glimpse(Weekly)
summary(Weekly)

# Part a
# see how Lag1 to Lag5 changed over time
par(mfrow = c(2,3))
for (i in 2:6) {
     plot(Weekly[,i])
}
par(mfrow = c(1,1))

# see pairwise scatter plot of lag1 to lag 5
pairs(Weekly[, 2:6])

# see correlation between all predictor variables
library(ggplot2)
Weekly[,-9] %>%
     cor() %>% 
     round(3) %>% 
     ggcorrplot(hc.order = TRUE, 
                type = "lower", 
                lab = TRUE, 
                lab_size = 3, 
                method="square",     # can use circle instead
                colors = c("tomato", "white", "darkgreen"), 
                title="Correlogram", 
                ggtheme=theme_bw)

# another approach to see all the above in the same graph
library(psych)
pairs.panels(Weekly[, 2:8],
             smooth = TRUE,      # If TRUE, draws less smooths
             scale = FALSE,      # If TRUE, scales the correlation text font
             density = TRUE,     # If TRUE, adds density plots and histograms
             ellipses = TRUE,    # If TRUE, draws ellipses
             method = "pearson", # Correlation method (also "spearman" or "kendall")
             pch = 21,           # pch symbol
             lm = FALSE,         # If TRUE, plots linear fit rather than the LOESS (smoothed) fit
             cor = TRUE,         # If TRUE, reports correlations
             jiggle = FALSE,     # If TRUE, data points are jittered
             factor = 2,         # Jittering factor
             hist.col = 4,       # Histograms color
             stars = TRUE,       # If TRUE, adds significance level with stars
             ci = TRUE)          # If TRUE, adds confidence intervals

# Part b - Logistic model using all data
logistic.fit = glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, 
                   data = Weekly,
                   family = binomial)
summary(logistic.fit)
logistic.prob = predict(logistic.fit,             # Output of logistic predictions are probabilities of outcome being a yes or a 1
                        newdata = Weekly,
                        type = "response")
# Part c
contrasts(Direction)                              # this shows up is 1, so a prediction with probability of Direction > 0.5 is an "Up" value
Direction.pred = rep("Down", nrow(Weekly))
Direction.pred[logistic.prob > 0.5] <- "Up"
table(Direction.pred, Direction)
mean(Direction.pred == Direction)


# Part d - Logistic model using training and test data @@@@@@@@@@@@@@@@
unique(Weekly$Year)
TrainIndex = Year <= 2008
logistic.fit = glm(Direction ~ Lag2,
                   data = Weekly,
                   subset = TrainIndex,           # index is used to define the subset from the data
                   family = binomial)
summary(logistic.fit)

logistic.prob = predict(logistic.fit, 
                    newdata = Weekly[!TrainIndex, ],
                    type = "response")

Direction.pred.logistic = rep("Down", nrow(Weekly[!TrainIndex, ]))
Direction.pred.logistic[logistic.prob > 0.5] <- "Up"
table(Direction.pred.logistic, Direction[!TrainIndex])
mean(Direction.pred.logistic == Direction[!TrainIndex])
(logistic.error.rate = mean(Direction.pred.logistic != Direction[!TrainIndex]))   # total error rate


# Part e - LDA model @@@@@@@@@@@@@@@@
LDA.fit = lda(Direction ~ Lag2,
              data = Weekly,
              subset = TrainIndex)
LDA.fit
Direction.pred.LDA = predict(LDA.fit, 
                             newdata = Weekly[!TrainIndex, ]) 
table(Direction.pred.LDA$class)
table(Direction.pred.LDA$class, Direction[!TrainIndex])
mean(Direction.pred.LDA$class == Direction[!TrainIndex])
(LDA.error.rate = mean(Direction.pred.LDA$class != Direction[!TrainIndex]))   # total error rate


# Part f - QDA model @@@@@@@@@@@@@@@@
QDA.fit = qda(Direction ~ Lag2,
              data = Weekly,
              subset = TrainIndex)
QDA.fit
Direction.pred.QDA = predict(QDA.fit, 
                             newdata = Weekly[!TrainIndex, ]) 
table(Direction.pred.QDA$class)
table(Direction.pred.QDA$class, Direction[!TrainIndex])
mean(Direction.pred.QDA$class == Direction[!TrainIndex])
(QDA.error.rate = mean(Direction.pred.QDA$class != Direction[!TrainIndex]))   # total error rate

# Part g - KNN model @@@@@@@@@@@@@@@@
library(class)
Weekly.Train.Set = matrix(Weekly[TrainIndex, 3])      # making the training set with only the Lag2 variable (KNN only takes dataframe or matrix as input)
Weekly.Test.Set = matrix(Weekly[!TrainIndex, 3])      # making the test set with only the Lag2 variable
Weekly.Train.Response = Weekly$Direction[TrainIndex]   # making the response vector for the training set
Weekly.Test.Response = Weekly$Direction[!TrainIndex]

set.seed(100)
Direction.Pred.KNN = knn(train =  Weekly.Train.Set, 
                         test = Weekly.Test.Set, 
                         cl = Weekly.Train.Response, 
                         k = 1)

table(Direction.Pred.KNN, Weekly.Test.Response)
mean(Direction.Pred.KNN == Weekly.Test.Response)
(KNN.error.rate = mean(Direction.Pred.KNN != Weekly.Test.Response))

# Part h
Error.rate =data.frame('Logistic Error Rate' = logistic.error.rate,
                       'LDA Error Rate' = LDA.error.rate,
                       'QDA Error Rate' = QDA.error.rate,
                       'KNN-1 Error Rate' = KNN.error.rate)

# Part i - Skipped them

######### Ch 4 - Ex 11 ########
# Part a
AutoData <- Auto %>% 
     mutate(mpg01 = if_else(mpg > median(Auto$mpg), 1, 0))

# or another way to do the above
Auto$mpg01[Auto$mpg > median(Auto$mpg)] = 1
Auto$mpg01[Auto$mpg <= median(Auto$mpg)] = 0

# Part b
library(psych)
pairs.panels(AutoData[,],
             smooth = TRUE,      # If TRUE, draws less smooths
             scale = FALSE,      # If TRUE, scales the correlation text font
             density = TRUE,     # If TRUE, adds density plots and histograms
             ellipses = TRUE,    # If TRUE, draws ellipses
             method = "pearson", # Correlation method (also "spearman" or "kendall")
             pch = 21,           # pch symbol
             lm = FALSE,         # If TRUE, plots linear fit rather than the LOESS (smoothed) fit
             cor = TRUE,         # If TRUE, reports correlations
             jiggle = FALSE,     # If TRUE, data points are jittered
             factor = 2,         # Jittering factor
             hist.col = 4,       # Histograms color
             stars = TRUE,       # If TRUE, adds significance level with stars
             ci = TRUE)          # If TRUE, adds confidence intervals

par(mfrow = c(2, 4))
plot(AutoData$horsepower, AutoData$mpg01)
plot(AutoData$weight, AutoData$mpg01)
plot(AutoData$displacement, AutoData$mpg01)
plot(AutoData$acceleration, AutoData$mpg01)

boxplot(AutoData$horsepower ~ AutoData$mpg01)
boxplot(AutoData$weight~ AutoData$mpg01)
boxplot(AutoData$displacement~ AutoData$mpg01)
boxplot(AutoData$acceleration~ AutoData$mpg01)
par(mfrow = c(1, 1))

# Part c - Create sample data
# create a vector with the size of number of records in the dataframe, 90% of values are True and 10% False randomly assigned, to be used as index
TrainIndex = sample(c(TRUE, FALSE), nrow(AutoData), replace = T, prob = c(0.9, 0.1))      
TrainData = AutoData[TrainIndex,]       # just for the heck of it. subset command is used in the modeling
TestData = AutoData[!TrainIndex,]

# Part d
LDA.fit = lda(mpg01 ~ horsepower + weight + displacement + acceleration, 
              data = AutoData, 
              subset = TrainIndex)

LDA.fit
lda.pred = predict(LDA.fit, TestData) 
lda.pred$class
table('LDA prediction' =lda.pred$class, observation = TestData$mpg01)
LDA.error.rate <- mean(TestData$mpg01 !=lda.pred$class)

# Part e
QDA.fit = qda(mpg01 ~ horsepower + weight + displacement + acceleration, 
              data = AutoData, 
              subset = TrainIndex)
QDA.fit
qda.pred = predict(QDA.fit, 
                   newdata = TestData) 
qda.pred$class
table('QDA prediction' =qda.pred$class, observation = TestData$mpg01)
QDA.error.rate <- mean(TestData$mpg01 !=qda.pred$class)


# Part f
logistic.fit = glm(mpg01 ~ horsepower + weight + displacement + acceleration, 
                   data = AutoData,
                   subset = TrainIndex,
                   family = binomial)
summary(logistic.fit)

par(mfrow = c(2,2))
plot(logistic.fit)
par(mfrow = c(1,1))

logistic.prob = predict(logistic.fit, 
                        newdata = TestData,
                        type = "response")

logistic.pred = rep(0, times = nrow(TestData))
logistic.pred[logistic.prob > 0.5] = 1
table('logistic prediction'= logistic.pred, 'Observation' = TestData$mpg01)
(logistic.error.rate = mean(logistic.pred != TestData$mpg01))

# Part g
TrainAuto = AutoData[TrainIndex, 3:6]
TrainResponce = AutoData[TrainIndex, 10]
TestAuto = AutoData[!TrainIndex, 3:6]

KNN1.fit = knn(TrainAuto, TestAuto, TrainResponce, k = 1)
table('KNN prediction'= KNN1.fit, 'Observation' = TestData$mpg01)
(KNN1.error.rate = mean(KNN1.fit != TestData$mpg01))

KNN2.fit = knn(TrainAuto, TestAuto, TrainResponce, k = 2)
(KNN2.error.rate = mean(KNN2.fit != TestData$mpg01))

KNN3.fit = knn(TrainAuto, TestAuto, TrainResponce, k = 3)
(KNN3.error.rate = mean(KNN3.fit != TestData$mpg01))

KNN4.fit = knn(TrainAuto, TestAuto, TrainResponce, k = 4)
(KNN4.error.rate = mean(KNN4.fit != TestData$mpg01))

options(digits = 4)
Error.rate =data.frame('Logistic Error Rate' = logistic.error.rate * 100,
                       'LDA Error Rate' = LDA.error.rate * 100,
                       'QDA Error Rate' = QDA.error.rate * 100,
                       'KNN-1 Error Rate' = KNN1.error.rate * 100,
                       'KNN-2 Error Rate' = KNN2.error.rate * 100,
                       'KNN-3 Error Rate' = KNN3.error.rate * 100,
                       'KNN-4 Error Rate' = KNN4.error.rate * 100)

t(Error.rate)

######### Ch 4 - Ex 12 ########
# # Part a
# Power = function() {
#      show(2^3)
# }
# Power()
# 
# # Part b
# Power2 = function(a, b){
#      show(a^b)
# }
# Power2(4, 5)
# 
# # Part c
# Power2(10, 3)
# 
# # Part d
# Power3 = function(a, b){
#      result = a ^ b
#      return(result)
# }
# Power3(10, 3)
# 
# # Part e
# x = 1:10
# y = Power3(x, 2)
# 
# par(mfrow = c(2,2))
# plot(x, y, main = "Plot of Y = x^2")
# plot(x, y, main = "Plot of Y = x^2 on log-x axis", log = 'x')
# plot(x, y, main = "Plot of Y = x^2 on log-y axis", log = 'y')
# plot(x, y, main = "Plot of Y = x^2 on log-x and log-y axes", log = 'xy')
# par(mfrow = c(1,1))
# 
# # Part f
# PlotPower = function(a, b){
#      result = x^b
#      return(plot(x, result))
# }
# 
# x=-10:10
# PlotPower(x, 3)

######### Ch 4 - Ex 13 ########
# taking a look around the data
Boston = Boston
attach(Boston)
summary(crim)
options(scipen = 100)
boxplot(crim, log = 'y')
view(sort(crim))

# create a binary crim variable calling it crim01
Boston$crim01[crim > median(crim)] = 1
Boston$crim01[crim < median(crim)] = 0
glimpse(Boston)
attach(Boston)

# looking at correlation of variables and crim01
Boston %>% 
     cor() %>% 
     round(2) %>% 
     ggcorrplot(hc.order = TRUE, 
                type = "lower", 
                lab = TRUE, 
                lab_size = 3, 
                method="square",     # can use circle instead
                colors = c("tomato", "white", "darkgreen"), 
                title="Correlogram of mtcars", 
                ggtheme=theme_bw)

# looking at those with 0.6+ correlation only
cor(data.frame(crim01, rad, tax, age, indus, nox, dis))

# tax and rad have a high correlation. A scatter plot of those two shows it's not a real correlation and something is going on!
# seems about 26% of records are standing farther away from the rest with much higher tax rate, near or far from radial highways
# after removing those seemingly outliers, correlation between rad and tax drops to 19% - both will be included in the analaysis
plot(tax, rad)
ggplot(Boston, aes(tax, rad)) +
     geom_jitter(width = 2, 
                 size=0.5,
                 color = "blue") +
     labs(title="Jittered plot of tax and rad",
          y="Vicinity to Radial Highways", 
          x="Tax rate per $10,000")

test <- data.frame(cbind(tax, rad))
test <- test[test$rad < 10 | test$tax < 500, ]
cor(test)
rm(test)
Boston <- data.frame(crim01, rad, tax, age, indus, nox, dis)
attach(Boston)







# Create sample data
# create a vector with the size of number of records in the dataframe, 90% of values are True and 10% False randomly assigned, to be used as index
TrainIndex = sample(c(TRUE, FALSE), nrow(Boston), replace = T, prob = c(0.9, 0.1))      
TestBoston = Boston[!TrainIndex,]

# Logistic
logistic.fit = glm(crim01 ~ rad + tax + age + indus + nox + dis, 
                   data = Boston,
                   subset = TrainIndex,
                   family = binomial)
summary(logistic.fit)

par(mfrow = c(2,2))
plot(logistic.fit)
par(mfrow = c(1,1))

logistic.prob = predict(logistic.fit, 
                        newdata = TestBoston,
                        type = "response")

logistic.pred = rep(0, times = nrow(TestBoston))
logistic.pred[logistic.prob > 0.5] = 1
table('logistic prediction'= logistic.pred, 'Observation' = TestBoston$crim01)
(logistic.error.rate = mean(logistic.pred != TestBoston$crim01))


# LDA
LDA.fit = lda(crim01 ~ rad + tax + age + indus + nox + dis, 
              data = Boston,
              subset = TrainIndex)
LDA.pred = predict(LDA.fit, 
                   newdata = TestBoston) 
table('LDA prediction' =LDA.pred$class, 
      Observation = TestBoston$crim01)
(LDA.error.rate <- mean(TestBoston$crim01 != LDA.pred$class))

# QDA
QDA.fit = qda(crim01 ~ rad + tax + age + indus + nox + dis, 
              data = Boston,
              subset = TrainIndex)
QDA.pred = predict(QDA.fit, 
                   newdata = TestBoston) 
table('QDA prediction' = QDA.pred$class, 
      Observation = TestBoston$crim01)
(QDA.error.rate <- mean(TestBoston$crim01 != QDA.pred$class))

# KNN
glimpse(Boston)
TrainPredictorsBoston = Boston[TrainIndex, 2:length(Boston)]
TrainResponceBoston = Boston[TrainIndex, 1]
TestPredictorBoston = Boston[!TrainIndex, 2:length(Boston)]

KNN1.fit = knn(TrainPredictorsBoston, TestPredictorBoston, TrainResponceBoston, k = 1)
table('KNN prediction'= KNN1.fit, 'Observation' = TestBoston$crim01)
(KNN1.error.rate = mean(KNN1.fit != TestBoston$crim01))

KNN2.fit = knn(TrainPredictorsBoston, TestPredictorBoston, TrainResponceBoston, k = 2)
table('KNN prediction'= KNN2.fit, 'Observation' = TestBoston$crim01)
(KNN2.error.rate = mean(KNN2.fit != TestBoston$crim01))

KNN3.fit = knn(TrainPredictorsBoston, TestPredictorBoston, TrainResponceBoston, k = 3)
(KNN3.error.rate = mean(KNN3.fit != TestBoston$crim01))

KNN4.fit = knn(TrainPredictorsBoston, TestPredictorBoston, TrainResponceBoston, k = 4)
(KNN4.error.rate = mean(KNN4.fit != TestBoston$crim01))

options(digits = 4)
Error.rate =data.frame('Logistic Error Rate' = logistic.error.rate * 100,
                       'LDA Error Rate' = LDA.error.rate * 100,
                       'QDA Error Rate' = QDA.error.rate * 100,
                       'KNN-1 Error Rate' = KNN1.error.rate * 100,
                       'KNN-2 Error Rate' = KNN2.error.rate * 100,
                       'KNN-3 Error Rate' = KNN3.error.rate * 100,
                       'KNN-4 Error Rate' = KNN4.error.rate * 100)

t(Error.rate)

#### Sec 5.3.1 - The Validation Set Approach ####
# Using validation set approach to find the test error rate on linear models fit to the Auto data
data(Auto)
glimpse(Auto)

# set.seed(1)
TrainIndex = sample(392, 196) # generate random numbers between 1 and 392 for a total of 392/2

# Simple Linear Regression
lm.fit1 = lm(mpg ~ horsepower, 
             data = Auto,
             subset = TrainIndex)
# summary(lm.fit1)
# view(cbind(Auto$mpg, predict(lm.fit1, newdata = Auto), Auto$mpg-predict(lm.fit1, newdata = Auto)))
simpleMSE = mean((Auto$mpg - predict(lm.fit1, newdata = Auto))[-TrainIndex]^2)

# Quadratic Linear Regression
lm.fit2 = lm(mpg ~ poly(horsepower, 2), 
             data = Auto,
             subset = TrainIndex)
QuadMSE = mean((Auto$mpg - predict(lm.fit2, newdata = Auto))[-TrainIndex]^2) 

# Cubic Linear Regression
lm.fit3 = lm(mpg ~ poly(horsepower, 3), 
             data = Auto,
             subset = TrainIndex)
CubicMSE = mean((Auto$mpg - predict(lm.fit3, newdata = Auto))[-TrainIndex]^2) 

#### Sec 5.3.2 - The LOOCV Approach ####
# LOOCV estimate can be automatically computed for any generalized linear model using the glm() and cv.glm() functions
# in glm() if we don't specify the family option for it, it will do the same as a lm()
glm(mpg ~ horsepower, data = Auto) %>% coef()
lm(mpg ~ horsepower, data = Auto) %>% coef()

glm.fit = glm(mpg ~ horsepower, data = Auto)
LOOCV.error = cv.glm(data = Auto, glm.fit) # calculates the estimated K-fold cross-validation prediction error for generalized linear models (default k=n)
options(digits = 5)      # to show 5 total digits in the output (which will be 3 decimals in this case)

# In the cv.glm() function, the delta value is used to estimate the cross-validation error. 
# It is calculated as the difference between the deviance of the null model and the deviance of the model fit to the training data. 
# The deviance is a measure of the goodness of fit of a model, so a smaller delta value indicates a better fit.
LOOCV.error$delta

LOOCV.error = rep(0,5)
for (i in 1:5){
     glm.fit = glm(mpg ~ poly(horsepower, i), data = Auto)
     LOOCV.error[i] = cv.glm(data = Auto, glm.fit)$delta[1]
}
plot(LOOCV.error, type = "o")

#### Sec 5.3.2 - The k-fold CV Approach ####

# trying with k = 10, same cv.glm can be used but this time we assign k and not leave it as default of k = n
set.seed(17)
kFold10.error = rep(0, 8)
for (i in 1:8) {
     glm.fit = glm(mpg ~ poly(horsepower, i), data = Auto)
     kFold10.error[i] = cv.glm(data = Auto, glm.fit, K = 10)$delta[1]
}
plot(kFold10.error, type = "o")

#### Sec 5.3.2 - The Bootstrap ####

## Estimating the Accuracy of a Statistic of Interest
# 1. create a function that computes the statistic of interest
# 2. use boot() function from the boot library by repeatedly sampling from data with replacement

data(Portfolio)
summary(Portfolio)
glimpse(Portfolio)

# define a function to calculate the statistic of interest. In this case the statistic is called alpha and has a formula
alpha.fn = function(data, index) {
      X = data$X[index]
      Y = data$Y[index]
      return(((var(Y) - cov(X,Y))/(var(X) + var(Y) - 2*cov(X,Y))))
}

# e.g. find the alpha for the data Portfolio (which has 100 observations)
alpha.fn(Portfolio, 10:30)

# take a sample from Portfolio and find alpha for that sample using sample(x, size, replace = FALSE, prob = NULL)
alpha.fn(Portfolio, sample(100, 100, replace = T))
# The above can be done for so many times and record all the alpha values and then take the mean and standard deviation of all of them
# but the function boot(data, statistic, Number of bootstrap reps, ...) will do the same

boot(Portfolio, alpha.fn, R = 1000)

## Estimating the Accuracy of a Linear Regression Model
## linear regression of degree 1
# First define a function to take the data and index set in and do a regression and return the coefficients
# note this function doesn't need {} because it's only one-line and can also be written in one line
boot.fn = function(data, index) 
     return(coef(lm(mpg ~ horsepower, data = Auto, subset = index)))
boot.fn(Auto, 1:392)
boot.fn(Auto, sample(392, 392, replace = T))

boot(data = Auto, statistic = boot.fn, R = 1000)                     # to get the bootstrap estimate for coef and their SE of linear regression
summary(lm(mpg ~ horsepower, data = Auto))$coef   # to get the actual estimate for coef and their SE of linear regression

## linear regression of degree 2 (quadratic)
bootq.fn = function(data, index) coefficients(lm(mpg ~ horsepower + I(horsepower^2), data = Auto, subset = index))
set.seed(1)
boot(Auto, bootq.fn, R = 1000)
summary(lm(mpg ~ horsepower + I(horsepower^2), data = Auto))$coef

######### Ch 5 - Ex 5 - Logistic on Default Data (w Validation Set Approach) ########
data("Default")
glimpse(Default)

par(mfrow = c(1,2))
plot(Default$balance ~ Default$default)
plot(Default$income ~ Default$default)
par(mfrow = c(1,1))

# Part a
set.seed(1)
logistic.fit = glm(default ~ income + balance, data = Default, family = "binomial")
coef(logistic.fit)

# Part b - validation set approach (single run)
TrainIndex = sample(nrow(Default), nrow(Default)/2) # generate random numbers between 1 and 10,000 for a total of 5,000
logistic.fit = glm(default ~ income + balance, 
                   data = Default, 
                   subset = TrainIndex,
                   family = "binomial")
options(digits = 5)
options(scipen=100) 
coef(logistic.fit)

logistic.prob = predict(logistic.fit, 
                        newdata = Default[-TrainIndex, ],  # this is our test set using the opposite of training index
                        type = "response")
logistic.pred = rep('No', times = nrow(Default[-TrainIndex, ]))
logistic.pred[logistic.prob > 0.5] = 'Yes'
table('Logistic Prediction'= logistic.pred, 'Observation' = Default[-TrainIndex, 'default'])
(logistic.error.rate = mean(logistic.pred != Default[-TrainIndex, 'default']))

# Part c - validation set approach (multiple runs) / I just defined a function to take the number of run and give me the final mean error
cValidationSetLogistic <- function(RunTimes = 1) {
     logistic.error.rate = data.frame(matrix(0, 
                                             nrow = RunTimes, 
                                             ncol = 1, 
                                             dimnames = list(NULL, 'Error Rate')))
     for (i in 1:RunTimes) {
          TrainIndex = sample(nrow(Default), nrow(Default)/2)
          logistic.fit = glm(default ~ income + balance, 
                             data = Default, 
                             subset = TrainIndex,
                             family = "binomial")
          logistic.prob = predict(logistic.fit, 
                                  newdata = Default[-TrainIndex, ],
                                  type = "response")
          logistic.pred = rep('No', times = nrow(Default[-TrainIndex, ]))
          logistic.pred[logistic.prob > 0.5] = 'Yes'
          # table('Logistic Prediction'= logistic.pred, 'Observation' = Default[-TrainIndex, 'default'])
          logistic.error.rate[i,] = mean(logistic.pred != Default[-TrainIndex, 'default'])
     }
     row.names(logistic.error.rate) = logistic.error.rate$`Run Number`
     return(mean(logistic.error.rate[, 1]))
}
cValidationSetLogistic(100)


# Part d - Adding a student dummy variable to the model
summary(Default$student)
describe(Default)

dValidationSetLogistic <- function(RunTimes = 5) {
     logistic.error.rate = data.frame(matrix(0, 
                                             nrow = RunTimes, 
                                             ncol = 1, 
                                             dimnames = list(NULL, 'Error Rate')))
     for (i in seq_len(RunTimes)) {
          TrainIndex = sample(nrow(Default), nrow(Default)/2)
          logistic.fit = glm(default ~ income + balance + student, 
                             data = Default, 
                             subset = TrainIndex,
                             family = "binomial")
          logistic.prob = predict(logistic.fit, 
                                  newdata = Default[-TrainIndex, ],
                                  type = "response")
          logistic.pred = rep('No', times = nrow(Default[-TrainIndex, ]))
          logistic.pred[logistic.prob > 0.5] = 'Yes'
          # table('Logistic Prediction'= logistic.pred, 'Observation' = Default[-TrainIndex, 'default'])
          logistic.error.rate[i,] = mean(logistic.pred != Default[-TrainIndex, 'default'])
     }
     row.names(logistic.error.rate) = logistic.error.rate$`Run Number`
     return(mean(logistic.error.rate[, 1]))
}

# the question is only asking for 1 run but I'm running 100 times and getting an average of all runs' errors
dValidationSetLogistic(RunTimes = 10)

######### Ch 5 - Ex 6 - Logistic on Default Data (SE of Coefficients) ########
data("Default")
glimpse(Default)

# Part a - Coefficients SE from the formula
options(scipen = 4)
set.seed(1)
glm(default ~ income + balance, 
    data = Default, 
    family = "binomial") %>% 
     summary() %>% 
     coef()

# Part b - Coefficients SE using bootstrap approach
boot.fn = function(InputData, index) {
     return(coef(glm(default ~ income + balance, 
                     data = InputData,
                     subset = index,
                     family = "binomial")))
}
     
boot.fn(Default, 1:392)
options(scipen = 9)
boot.fn(Default, sample(392, 392, replace = T))

# Part c - use boot function to run the boot.fn function defined before for a certain number of times and find SE of coefficients
boot(Default, boot.fn, R = 40)

# Part d - comment on glm() vs bootstrap method
# The SE for the coefficients are very close to each other, depending on how many runs we do

######### Ch 5 - Ex 7 - Logistic on Weekly Data (SE of Coefficients) ########
data("Weekly")
glimpse(Weekly)

# Part a - fit logistic and predict Direction from Lag1 and Lag2
options(scipen = 4)
logistic.fit1 <- glm(Direction ~ Lag1 + Lag2, 
                    data = Weekly, 
                    family = "binomial") 
coef(logistic.fit1)
logistic.prob = predict(logistic.fit1, 
                        type = "response")
contrasts(Weekly$Direction)
logistic.pred = rep('Down', times = nrow(Weekly))
logistic.pred[logistic.prob > 0.5] = 'Up'
table('Logistic Prediction'= logistic.pred, 'Observation' = Weekly$Direction)
(logistic.error.rate = mean(logistic.pred != Weekly$Direction))

# Part b - fit logistic and predict Direction from Lag1 and Lag2 (using all but first observation)
set.seed(1)
logistic.fit2 <- glm(Direction ~ Lag1 + Lag2, 
                    data = Weekly[-1, ], 
                    family = "binomial")
summary(logistic.fit2)

# Part c - predict direction of the first observation (testing)
logistic.prob = predict(logistic.fit2, 
                        newdata = Weekly[1, ],
                        type = "response") > 0.5
if (logistic.prob > 0.5) {
     cat("Predicted Direction is Up for 1st observation", "\n", "Incorrect Prediction")
} else {
     cat("Predicted Direction is Down for 1st observation", "\n", "Correct Prediction")
}

# Part d - Do the LOOCV with For loop
LOOCV.error = rep(0, nrow(Weekly))
for (index in 1:nrow(Weekly)) {
     logistic.fit.i <- glm(Direction ~ Lag1 + Lag2, 
                          data = Weekly[-index, ], 
                          family = "binomial")
     logistic.prob.i = predict(logistic.fit.i, 
                             newdata = Weekly[index, ],
                             type = "response") 
     if (logistic.prob.i > 0.5) {Predicted.Direction = "Up"}
          else {Predicted.Direction = "Down"}
     if (Predicted.Direction != Weekly$Direction[index]) {LOOCV.error[index] = 1}
}
LOOCV.error

# Part e - LOOOCV estimate for test error
mean(LOOCV.error)

######### Ch 5 - Ex 8 - Cross-validation on a simulated dataset ########

# Part a - generate simulated dataset
set.seed(1)
y = rnorm(100)
x = rnorm(100)
y = -2*x^2 + x + rnorm(100)

# Part b - Create a scatter plot 
plot(x, y)
df = data.frame(x, y)
# df %>% 
#      ggplot(aes(x=x, 
#                 y=y
#                 )) +
#      geom_point(size = 2, 
#                 alpha = 0.5) +
#      theme_classic() + 
#      labs(title = "Ch 5, Ex 8.b (y vs x, Simulated Data)")

# Part c, d, e - Fit up to degree 4 polynomial with LOOCV method and find error (different seeds) - check p-values
set.seed(1)
lm.fit1 = lm(y ~ x, data = df)
LOOCV.error = rep(0,4)
options(scipen = 50)
for (i in 1:4){
     lm.fit = glm(y ~ poly(x, i), data = df)
     print(coef(summary(lm.fit)))
     LOOCV.error[i] = cv.glm(data = df, lm.fit)$delta[1]
}
plot(LOOCV.error, type = "o")
# results of c and d are the same due to LOOCV evaluating n-fold and no matter what seed is used, all will be left-out once

######### Ch 5 - Ex 9 - 

##### End ####
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
rm(list = ls()) ; dev.off() ; plot.new()
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@




