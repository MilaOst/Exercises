#  Introduction
## ══════════════

#   • Learning objectives:
##     • Learn the R formula interface
##     • Specify factor contrasts to test specific hypotheses
##     • Perform model comparisons
##     • Run and interpret variety of regression models in R

## Set working directory
## ─────────────────────────

##   It is often helpful to start your R session by setting your working
##   directory so you don't have to type the full path names to your data
##   and other files

# set the working directory
# setwd("~/Desktop/Rstatistics")
# setwd("C:/Users/dataclass/Desktop/Rstatistics")

##   You might also start by listing the files in your working directory

getwd() # where am I?
list.files("dataSets") # files in the dataSets folder

setwd("C:/Users/lyudm/Documents/Springboard/Exercises/linear_regression/dataSets")

## Load the states data
## ────────────────────────

# read the states data
states.data <- readRDS("C:/Users/lyudm/Documents/Springboard/Exercises/linear_regression/dataSets/states.rds") 
#get labels
states.info <- data.frame(attributes(states.data)[c("names", "var.labels")])
#look at last few labels
tail(states.info, 8)

## Linear regression
## ═══════════════════

## Examine the data before fitting models
## ──────────────────────────────────────────

##   Start by examining the data to check for problems.

# summary of expense and csat columns, all rows
sts.ex.sat <- subset(states.data, select = c("expense", "csat"))
summary(sts.ex.sat)
# correlation between expense and csat
cor(sts.ex.sat)

## Plot the data before fitting models
## ───────────────────────────────────────

##   Plot the data to look for multivariate outliers, non-linear
##   relationships etc.

# scatter plot of expense vs csat
plot(sts.ex.sat)

## Linear regression example
## ─────────────────────────────

##   • Linear regression models can be fit with the `lm()' function
##   • For example, we can use `lm' to predict SAT scores based on
##     per-pupal expenditures:

# Fit our regression model
sat.mod <- lm(csat ~ expense, # regression formula
              data=states.data) # data set
# Summarize and print the results
summary(sat.mod) # show regression coefficients table

## Why is the association between expense and SAT scores /negative/?
## ─────────────────────────────────────────────────────────────────────

##   Many people find it surprising that the per-capita expenditure on
##   students is negatively related to SAT scores. The beauty of multiple
##   regression is that we can try to pull these apart. What would the
##   association between expense and SAT scores be if there were no
##   difference among the states in the percentage of students taking the
##   SAT?

summary(lm(csat ~ expense + percent, data = states.data))

## The lm class and methods
## ────────────────────────────

##   OK, we fit our model. Now what?
##   • Examine the model object:

class(sat.mod)
names(sat.mod)
methods(class = class(sat.mod))[1:9]

##   • Use function methods to get more information about the fit

confint(sat.mod)
# hist(residuals(sat.mod))

## Linear Regression Assumptions
## ─────────────────────────────────

##   • Ordinary least squares regression relies on several assumptions,
##     including that the residuals are normally distributed and
##     homoscedastic, the errors are independent and the relationships are
##     linear.

##   • Investigate these assumptions visually by plotting your model:

par(mar = c(4, 4, 2, 2), mfrow = c(1, 2)) #optional
plot(sat.mod, which = c(1, 2)) # "which" argument optional

## Comparing models
## ────────────────────

##   Do congressional voting patterns predict SAT scores over and above
##   expense? Fit two models and compare them:

# fit another model, adding house and senate as predictors
sat.voting.mod <-  lm(csat ~ expense + house + senate,
                      data = na.omit(states.data))
sat.mod <- update(sat.mod, data=na.omit(states.data))
# compare using the anova() function
anova(sat.mod, sat.voting.mod)
coef(summary(sat.voting.mod))

## Exercise: least squares regression
## ────────────────────────────────────────

##   Use the /states.rds/ data set. Fit a model predicting energy consumed
##   per capita (energy) from the percentage of residents living in
##   metropolitan areas (metro). Be sure to
##   1. Examine/plot the data before fitting the model
plot (energy ~ metro, data=states.data) #looks like there is some linear relationship, but potential outliers
cor.test(states.data$energy, states.data$metro, use="pairwise.complete.obs") # significant correlation and a modearate negative linear relationship
hist(states.data$energy) #violation of normality for DV
hist(states.data$metro)
scatterplotMatrix(formula=~energy+metro, data=states.data, diagonal="histogram")

##   2. Print and interpret the model `summary'

model1 <-  lm(energy ~ metro,data = na.omit(states.data))
summary(model1)
#%of residents in metro area is a significant predictor of energy consumption. It has a negative relationship with energy consumption. The higher %, the lower energy consumption per capita. The coefficient for metro is -1.6526. So each additional point of increase in % of metro residents is estimated to decrease energy consumption by 1.6526. However, the residuals are quite sparse ranging from -179.17 (min) to 448.02 (max). Also the quartiles of the residuals suggest that they are not symmetric around zero. The regression line is not doing a good job for predicting some values. The residual standard error is 112.3. The F-test is significant, meaning that the model still predicting the data better than simply by taking the average of energy consumption to predict each data point. But the adjusted R^2 is quite small, meaning that the variation in energy consumption might be also explained by some other variables besides the % of metro residents.

##   3. `plot' the model to look for deviations from modeling assumptions
par(mfrow=c(2,2))
plot(model1)

#Residuals vs. Fitted values and square root of st. residuals vs. fitted values suggest that there is no clear pattern and the residuals are randomly distributed. But there three observations (44,19,51) with very big residuals which might indicate that they are outliers. If we look at QQplot to see if the residuals follow a normal distribution, we see that some points are not close to the diagonal line - the line where they should be if they are normally distributed. There are some points that deviate from the line and again these are the same 3 observations that were noticed on the previous plots. Finally, Residuals vs. Leverage plot shows that observations 51 (Wyoming) and 19 (Louisiana) are really problematic - they are both outliers and influential points (especially Wyoming). If we look at the energy and metro values for these states, we see Wyoming has energy level that is twice higher than the median and metro level that is twice lower than the median. Louisiana is only different on the level of energy. It's twice higher than the median. In addtion, we check observation 44 (Texas) which was also a potential outlier on some charts and see its level of energy consumption is higher than the median even though % of metro residents is not that different from the median.

#look at individual observations
states.data[rownames(states.data) %in% c("51","19","44","2"),]
summary(states.data$energy)
summary(states.data$metro)

#filter observation with the highest energy consumption
states.data[states.data$energy>900,]

#When looking at the summary of energy consumption and also at the plot of metro vs. energy we notice that maximum energy consumption is 991 but none of the outliers above have these level of energy consumption. So we filter this observation out and figure out that this is another outlier, Alaska.

#remove outliers from the original dataset
states.data_new<-states.data[!(rownames(states.data) %in% c("51","19","44","2")),]


#examine/plot data without outliers
plot (energy ~ metro, data=states.data_new)
cor.test(states.data_new$energy, states.data_new$metro, use="pairwise.complete.obs") #correlation improves from -0.34 to -0.47.
scatterplotMatrix(formula=~energy+metro, data=states.data_new, diagonal="histogram")#distribution looks less skewed

model2 <-  lm(energy ~ metro,data = na.omit(states.data_new))
summary(model2)

#After removing the outliers, we see the rersidual standard error decreased from 112.3 to 56.72 and now they are symmetric around zero and not too sparse. Adjusted R^2 is significant and has improved to the level of 0.1953 meaning that % of metro residents is now a much better predictor of energy consumption level becasue it now accounts for almost 20% of variation. 

par(mfrow=c(2,2))
plot(model2)

#The residuals look more randlomly distributed. There are several observations on a QQ plot that are not on the diagonal line but they are not that far from it compared to how far away the previous outliers were from it. Also, there are no observations on the Cook's lines or beyond them and we can barely see those lines on the plot. 


##   Select one or more additional predictors to add to your model and
##   repeat steps 1-3. Is this model significantly better than the model
##   with /metro/ as the only predictor?

##   1.Examine/plot the data before fitting the model
# Some of the variables that might be predictors of energy consumption include: toxics released, greenhouse gas, miles per capita, and % of population with college degree). From the scatterplot matrix below we can see that there might be positive linear relationships between energy and toxic, eneregy and green, energy and miles. And similarly to metro, energy and college variables have negative relationships. It's good to see that each potential predictor has linear relationship with energy. But we also notice that there are some relationships between independent variables - positive between metro and college, negative - between miles and metro, negative - between metro and green. If these relationships are strong enough there might be some multicollinearity.

scatterplotMatrix(formula=~energy+metro+toxic+green+miles+college, data=states.data_new, diagonal="histogram")

#We check correlations between these variables and see that correlation between metro and miles is even stronger than between metro and energy. So the variable miles might play more important role in explaining energy than metro, in fact making the metro variable nonsignificant. Also, metro and college are strongly related too. 

corrplot.mixed(corr=cor(states.data_new[,c("metro","toxic","green","college","energy","miles")], use="complete.obs"), upper="ellipse", tl.pos="lt", number.cex=1, tl.cex=1, order="hclust")

##   2. Print and interpret the model `summary'
#When we extend the number of potential predictors, we get the model with improved Residual standard error, less sparse residuals and much stronger adjusted R^2. 68% of the variation in energy is explained by green, miles, and college. Green and miles have positive relationships with energy, whereas % of adults with college degree is negatively associated with energy consumption. Toxic is significant predictor at .1 significance level.

model3 <- lm(energy ~ metro+toxic+green+miles+college,data = na.omit(states.data_new))
summary(model3)

#When we kick metro off from the model as a nonsignificant predictor, we get the model where each variable is significant and the slightly improved adjusted R^2 (68.4%) becasue we have less predictors. 
model4 <- lm(energy ~ toxic+green+miles+college,data = na.omit(states.data_new))
summary(model4)


##   3. `plot' the model to look for deviations from modeling assumptions
#From the charts below we see that the residuals look randomly distributed. There are a few observations that could potentially be outliers, but not too ctritical.

par(mfrow=c(2,2))
plot(model4)

#When we compare the results of the two models - with one vs. four predictors, we see that the test came out sigificant and it means that the model with more predictors yields better fit.

anova(model2,model4)

## Interactions and factors
## ══════════════════════════

## Modeling interactions
## ─────────────────────────

##   Interactions allow us assess the extent to which the association
##   between one predictor and the outcome depends on a second predictor.
##   For example: Does the association between expense and SAT scores
##   depend on the median income in the state?

  #Add the interaction to the model
sat.expense.by.percent <- lm(csat ~ expense*income,
                             data=states.data) 
#Show the results
  coef(summary(sat.expense.by.percent)) # show regression coefficients table

## Regression with categorical predictors
## ──────────────────────────────────────────

##   Let's try to predict SAT scores from region, a categorical variable.
##   Note that you must make sure R does not think your categorical
##   variable is numeric.

# make sure R knows region is categorical
str(states.data$region)
states.data$region <- factor(states.data$region)
#Add region to the model
sat.region <- lm(csat ~ region,
                 data=states.data) 
#Show the results
coef(summary(sat.region)) # show regression coefficients table
anova(sat.region) # show ANOVA table

##   Again, *make sure to tell R which variables are categorical by
##   converting them to factors!*

## Setting factor reference groups and contrasts
## ─────────────────────────────────────────────────

##   In the previous example we use the default contrasts for region. The
##   default in R is treatment contrasts, with the first level as the
##   reference. We can change the reference group or use another coding
##   scheme using the `C' function.

# print default contrasts
contrasts(states.data$region)
# change the reference group
coef(summary(lm(csat ~ C(region, base=4),
                data=states.data)))
# change the coding scheme
coef(summary(lm(csat ~ C(region, contr.helmert),
                data=states.data)))

##   See also `?contrasts', `?contr.treatment', and `?relevel'.

## Exercise: interactions and factors
## ────────────────────────────────────────

##   Use the states data set.

##   1. Add on to the regression equation that you created in exercise 1 by
##      generating an interaction term and testing the interaction.

#When we generate all possible interactions between the predictor variables, we see that none of them is significant. 
model5 <- lm(energy ~ (toxic+green+miles+college)^2,data = na.omit(states.data_new))
summary(model5)

#If we add "metro" back into the model and test for interactions, we get a significant (at .1 level) interaction between green and metro. It means that the effect of greenhouse gas on energy consumption depends on % metro population and vice versa. 
model6 <- lm(energy ~ (toxic+green+miles+college+metro)^2,data = na.omit(states.data_new))
summary(model6)

#When we leave this interaction in the model and test the model again, we get only one variable (college) significant and the non-significant interaction.
model7 <- lm(energy ~ toxic+green+miles+college+metro+green:metro,data = na.omit(states.data_new))
summary(model7)

#When we compare two models - our initial model vs. the model with interaction, the test comes out non-sognificant. It means that the model that includea intercation does not improve fit and is not a good model. Its adjusted R-square is lower than the original model, and it yields to the results when prior significant variables become non-significant.

anova(model4,model7)

##   2. Try adding region to the model. Are there significant differences
##      across the four regions?

#When we add "region" into the model, none of the regions is a significant variable. 
contrasts(states.data_new$region)
model8 <- lm(energy ~ toxic+green+miles+college+region,data = na.omit(states.data_new))
summary(model8)

