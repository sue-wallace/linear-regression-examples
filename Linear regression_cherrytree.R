# Sue Wallace
# 10.06.2019

# https://www.dataquest.io/blog/statistical-learning-for-predictive-modeling-r/

# Collect some data relevant to the problem (more is almost always better).
# Clean, augment, and preprocess the data into a convenient form, if needed.
# Conduct an exploratory analysis of the data to get a better sense of it.
# Using what you find as a guide, construct a model of some aspect of the data.
# Use the model to answer the question you started with, and validate your results.

# Predicting cherry tree volume using a linear refression model (being able to determin
# a signal through nosie)

# Load libraries----

library(datasets) # practice data sets
library(ggplot2) # plotting
library(tidyverse) 
library(GGally) # add on to ggplot
library(scatterplot3d) # visualising linear regressions

# Explore data (black cheery trees)----

data(trees)
head(trees)
str(trees)

# Before building a predictive model do see if we can predict the volume of the tree 
# we need to undersatnd if there is a relationship between girth, height, and volume

# There are three varaibles, so ggpairs will give scatterplots for each 
# relationship between variables as well as the strength of correlation

ggpairs(data=trees, columns=1:3, title="trees data")

#he closer the correlation coefficient is to 1, the stronger the relationship is.

# So - Which predictor variables seem related to the response variable? 

# We can see from the top right and bottom left chart taht girth and volume 
# have a clear relationship, or what might be considered a 'liner' relationship

# Can we form a hypothesis?----

# Can we say from the trees data chart created that we know what's going on with
# the data?

# Now to build a linear model using the 'lm' function

fit_1 <- lm(Volume ~ Girth, data = trees) #volume is what is being predicted

# This will create a line that is as close as possible to the 31 observations. 
# the difference between the line and the observations are called residuals. 

# Is this mdel appropriate for the data to make predictions?----

# Can we reject the null hypothesis that there is no relationship between our variables?
# can the model is a good fit for our data?

# Now we can have a look at a summary of the 'fit_1' model to see how well
# the model fits the data in term sof predicting volume of the trees

summary(fit_1)

# Understanding the summary

#Coefficients: Estimate and Std. Error:
  
# The intercept in our example is the expected tree volume if the value of girth was zero. 
#Of course we cannot have a tree with negative volume, but more on that later.

# The slope in our example is the effect of tree girth on tree volume. We see that for each additional inch of girth, the tree volume increases by 5.0659 ft3.
# The coefficient standard errors tell us the average variation of the estimated coefficients from the actual average of our response variable.

# If the p value is smaller than 0.05 then the null hypothesis (this is that there is no
# relationship between girth and volume) can be rejected. In this case the null hypothesis 
# can be rejected

# How well does the model fit the data?----

# We want to see where the residuals sit to see if the model fits the data. If the model 
# fits the data then we would expect to see some symmestry around the 0 residual value

ggplot(data=trees, aes(fit_1$residuals)) +
  geom_histogram(binwidth = 1, color = "black", fill = "purple4") +
  theme(panel.background = element_rect(fill = "white"),
        axis.line.x=element_line(),
        axis.line.y=element_line()) +
  ggtitle("Histogram for Model Residuals")

# residual standard error (4.252 on 29 degrees of freedom)----

# How much does the response variable devaite from the linear model (or the line on the ls graph?)

# R squared (0.9353)----

# How close the data are to the model. This is 0.9 in this example. The closer
# the number is to 1, the better the data fit the model.

# F statistic( 419.4)----

# The larger the number, the better the relationship between the dependent and independent
# variables

# Confidence interval----

ggplot(data = trees, aes(x = Girth, y = Volume)) +
  geom_point() +
  stat_smooth(method = "lm", col = "dodgerblue3") + #this adds a 0.95 confidence interval
  theme(panel.background = element_rect(fill = "white"),
        axis.line.x=element_line(),
        axis.line.y=element_line()) +
  ggtitle("Linear Model Fitted to Data")

# the 0.95 confidence interval is the probability that the true linear model for the girth 
# and volume of all black cherry trees will lie within the confidence interval of the regression model
# fitted to our data.

# Using the model----

# The above analysis shows that this basic linear regression model is fit for purpose.
# This means that we can make some predictions on tree volume using this model

# If we take one tree as an example with the following values we can use this 
# tree to see if we can predict the volume by usin the girth by using the fit_1 list
# developed earlier in the example

# example free (girth = 18.2", height = 72ft, volume = 46.2ft^3)

predict(fit_1, data.frame(Girth = 18.2))

# The predicted volume is 55.2 ft. This is fairly close to the truw value of 46.2ft
# but not quite. Let's see if adding in the height variable aids the prediction.

# multiple linear regression----

# When adding in a new variable we want to add it to the existing model, not 
# a new one. 

fit_2 <- lm(Volume ~ Girth + Height, data = trees)
summary(fit_2)

# the r quared value here is actually higher than it is for 'fit_1' at 0.944

# Now we two predictor variables we need to use scatterplot 3rd to 
# visualise. first we need to create some data

Girth <- seq(9,21, by=0.5) ## make a girth vector
Height <- seq(60,90, by=0.5) ## make a height vector
pred_grid <- expand.grid(Girth = Girth, Height = Height) # dataframe from all combination of the factor variables

# Now make the predicted volumes using the 'fit_2' lm model. This
# will add a new predicted volume variable

pred_grid$Volume2 <- predict(fit_2, new = pred_grid)

# now we can create a 3d scatterplot with the predicted volumes

fit_2_sp <- scatterplot3d(pred_grid$Girth, pred_grid$Height, pred_grid$Volume2, 
                          angle = 60, color = "dodgerblue", pch = 1, 
                          ylab = "Hight (ft)", 
                          xlab = "Girth (in)", 
                          zlab = "Volume (ft3)" )

# do our observations from the 'trees' dataset fit the model?

fit_2_sp$points3d(trees$Girth, trees$Height, trees$Volume, pch=16)

# We can now use the same model to see if we can predict the volume of 
# the tree

predict(fit_2, data.frame(Girth = 18.2, Height = 72))

# Now that height is included we get a predicted volume of 52.13ft. this
# is closer to the true volume that than the 'fit_1' model. So
# we can see that including another variable into the model
# has helped

# However, this model doesn't take into account any interaction
# between height and girth. 

# We know that the taller a tree, the wider the girth will be (see
# the ggpairs plots that were developed earlier). so to account for this
# we need to add an interactivity clause into the model

fit_3 <- lm(Volume ~ Girth * Height, data = trees)
summary(fit_3)

# We use girth* height here, which is actually Girth + Height + Girth * Height

# This amendment to the model has pushed the R value closer to 1,
# which means that the model is now stronger. 

# Now we can apply the model to the predictor values generated for the 
# 'fit_2' model. 

pred_grid$Volume3 <-predict(fit_3, new = pred_grid)

# Let's make a scatterplot

fit_3_sp <- scatterplot3d(pred_grid$Girth, pred_grid$Height, pred_grid$Volume3,
                          angle = 60, color = "dodgerblue", 
                          pch = 1, ylab = "Hight (ft)", xlab = "Girth (in)", 
                          zlab = "Volume (ft3)")

# Now we can overlay our trees data into the plot to see how it fits

fit_3_sp$points3d(trees$Girth, trees$Height, trees$Volume, pch=16)

# Now let's utilise the 'fit_3' model, to see if it can predict the volume
# to a closer degree

predict(fit_3, data.frame(Girth = 18.2, Height = 72))

# Now we get 45.8, which is much closer to the true value of 46.2ft. 

# Caution!----

# This model appears to work for the trees data, however it's important not
# to apply to model to too wide a range of data. 

# To illustrate this point, let’s try to estimate the volume of a small sapling

predict(fit_3, data.frame(Girth = 0.25, Height = 4))

# This gives the sapling which is 4ft tall a volume of 62.8ft^3. 
# we know by common sense that this can't be correct. 











