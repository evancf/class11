# EBIO 338/538 Analysis and Visualization of Biological Data
# Class 13 Linear modeling continued

# Load data for clown fish body mass and Sex. Examine the data.
fish <- read.csv(file="fish_data.csv")
str(fish)
head(fish)

# Plot clown fish body mass as a function of Sex
plot(fish$Mass ~ fish$Sex, las=1, xlab="Sex", ylab="Body mass", type="p")

# Add the data points on top of the plot
points(fish$Mass ~ fish$Sex)

# Calculate the mean body mass for females
mean(fish$Mass[fish$Sex=="F"])

# Calculate the mean body mass for males
mean(fish$Mass[fish$Sex=="M"])

# Calculate the mean body mass for hermaphrodites
mean(fish$Mass[fish$Sex=="H"])

# Conduct a linear regression of fish body mass as a function of Sex. 
m <- lm(fish$Mass ~ fish$Sex)

# Examine a summary of the model
summary(m)

# What is the relationship between the intercept estimate and female body mass?
# They are the same

# What is the relationship between mean male body mass and the SexM estimate?
# mean male body mass is the female body mass estimate (the intercept) plus the SexM estimate

# What is the relationship between hermaphrodite body mass and the SexH estimate?
# mean hermaphrodite body mass is the female body mass estimate (the intercept) plus the SexH estimate

# What is the proportion of variance explained by the model? 
summary(m)[9]

# TRUE OR FALSE? Body mass varies significantly based on Sex 
FALSE


# Load the data on milk quality and brain development. Examine the data

milk <- read.csv(file="milk_data.csv")
str(milk)
head(milk)

# Remove observations with missing values for neocortex.perc. Examine the dimensions

milk <- milk[is.na(milk$neocortex.perc)==FALSE,]
dim(milk)

# Model milk kcal per gram as a function of neocortex size. 
m1 <- lm(milk$kcal.per.g ~ milk$neocortex.perc)

# Examine the model summary
summary(m1)

# Is this a bivariate model or a multivariate model?
# Bivariate model

# Is this a useful model? Why or why not?
# It's not very useful because it has a negative adj R squared indicating it 
# does not explain the data at all and the p-value for the F-statistic shows
# this model is not significantly better than a model without any predictors

# Now model milk kcal per gram as a function of log(body mass)
m2 <- lm(milk$kcal.per.g ~ log(milk$mass))
# Examine the model summary
summary(m2)

# Is this a bivariate model or a multivariate model?
# Bivariate

# Is this a useful model? Why or why not?
# It's not very useful because it has a small adj R squared indicating it 
# explains only 6% of the variance in the data and the p-value for the F-statistic shows
# this model is not significantly better than a model without any predictors

# Examine pairwise relationships between kcal per g, log(mass) and neocortex.perc
pairs(~kcal.per.g + log(mass) + neocortex.perc, data=milk)

# What underlying relationship exists among these variables?
cor(milk$neocortex.perc, milk$mass)

# Model milk kcal per gram as a function of log(body mass) and neocortex size
m3 <- lm(milk$kcal.per.g ~ log(milk$mass) + milk$neocortex.perc)
# Examine a summary of the model
summary(m3)

# Is this a bivariate model or a multivariate model?
# Multivariate
# Is this a useful model? Why or why not?
# Yes, this is a useful model because it explains ~46% of the variance in the observations
# and both mass and neocortex size are significant predictors of milk kcal per g

# Work through the code in "class11_plots.R" for plotting predicted values with
# confidence intervals and for plotting multivariate regression results for a single predictor

