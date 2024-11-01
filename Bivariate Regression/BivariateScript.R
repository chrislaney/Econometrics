#****************************************************
# This program sets up R for Module3 Learning Exercise
#   - Estimating and interpreting bivariate regressions
#
# Last Modified:  09/10/2020  Created
#                 01/12/2021  Modified
#                 08/29/2021  Minor changes
#                 08/23/2022  Data and regression updating
#                 08/31/2023  Further updating for Fall 2023
#                 07/29/2024  Updating
#
# Authors:  Nick Williams, CHRIS LANEY University of Cincinnati
#***************************************************

# ****************** PUT THIS CODE IN ALL OF YOUR SCRIPTS ********************************************
# ****************** PUT THIS CODE IN ALL OF YOUR SCRIPTS ********************************************
#****************************************************
#
# Clear environment, which means totally clear R environment
# of objects and loaded packages
rm(list=ls())
# To clear just the console window, type "Ctrl+L" or use Edit pull down menu

#*********************** Install Packages, Add to Library**********************#
#
# This is an efficient set of code to make sure that all needed packages are installed and loaded into the library.  
# This list contains most (if not all) the packages we will use this term.
#
# *** The "AER" package (Applied Econometrics with R) contains a number of useful commands, including
#     easily producing the var-cov matrix when use robust standard errors
#
# *** The "car" package stands for "Companion to Applied Regression" and
#     contains some nice commands,including an easy linear hypothesis test
#
# *** The "gmodels" package produces somewhat nicer tables and crosstabs.
#     I use the CrossTable command below. Not 100% sold on this package, 
#     but it does produce nicer output than the table command.  
#
# *** The "haven" package allows Stata data sets to be read easily into R
#
# *** The "ivreg"  Instrumental Variables package.  Note:  An older version of this package is in the AER
#        package.  The newer version is a little easier to use.
#
# *** The "jtools" package is one that produces easier to 
#     read regression output.  In particular the "summ" command replaces
#     the "summary" command.
#
# *** The "pastecs" package is one of several that can easily produce a 
#     a set of descriptive statistics.
#
# *** The "plm" package (Linear Models for Panel Data).  Contains a set of estimators and tests for panel data econometrics.
# 
# *** The "psych" package is another that can produce descriptive statistics.  It is
#     particular useful for looking at stats by groups in the data.
#
# *** We will need to use the "readxl" package to read it into R
#     This package is a part of the "tidyverse" package, which we installed above.
#     Now we just have to make sure it is in the library
#
# *** The "skedastic" package (Heteroskedasticity Diagnostics for Linear Regression Models).
#     Implements numerous methods for detecting heteroskedasticity including the Breusch-Pagan/Cook-Weisberg test
#     and the White test
#
# *** The "stargazer" package produces well-formatted regression tables.  See
#     https://www.rdocumentation.org/packages/stargazer/versions/5.2.2/topics/stargazer for more 
#     details
#
# *** The "summarytools" package has functions for looking at frequencies, cross-tabs
#     descriptive statistics, and dataframe summaries
#
# *** The "tidyverse" package contains a number of useful commands for reading, tidying
#     transforming, and visualizations.  
#-------------------------------

#---------------------------------------------------------
# If adding a new package, simply add to the list below
#---------------------------------------------------------
# Package names.  Will simply add more packages to this list as the semester proceeds
packages <- c("AER","car", "gmodels", "haven", "ivreg", "jtools", "pastecs", "plm", "psych", "readxl", "skedastic",
              "stargazer", "summarytools","tidyverse")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) 
{install.packages(packages[!installed_packages])}

# Load the packages
invisible(lapply(packages, library, character.only = TRUE))
search()

#*******************************************************************************
# Some packages have options that can be set.  These are done here
#*******************************************************************************
# Set the number of digits to display when using the jtools commands, like summ
options("jtools-digits"=8) 

# ****************** LAST BIT OF CODE TO INCLUDE IN ALL OF YOUR SCRIPTS ********************************************
# ****************** LAST BIT OF CODE TO INCLUDE IN ALL OF YOUR SCRIPTS ********************************************
#setwd("~/R/")

# ---------------------------------------------------------
# Variable      Label                                      
# ------------- ------------------------------------------ 
# statecode     State FIPS Code                            
# countycode    County FIPS Code                           
# fipscode      5-digit FIPS Code                          
# county        Name                                                           
# income        Median household income raw value, $       
# teenbirths    Num births per 1000 female population, ages 15-19
# uerate        Unemployment rate 

descr(healthData)

#Qb
ggplot(healthData, aes(x = teenbirths)) +
  geom_histogram(binwidth = 2, fill = "skyblue", color = "black") +  # Adjust binwidth as needed
  labs(title = "Histogram of Teen Births",
       x = "Teen Births",
       y = "Frequency")

#Qc

# Estimate the OLS model
model <- lm(teenbirths ~ income, data = healthData)

# Display the model summary with 8-digit precision
summ(model)

ggplot(healthData, aes(x = income, y = teenbirths)) +
  geom_point() +  # Scatter plot of data points
  geom_smooth(method = "lm", col = "blue") +  # Add regression line
  labs(title = "Regression of Teen Births on Income",
       x = "Income",
       y = "Teen Births")

#Qe

income_th <- healthData$income/1000

#Qe Scatter Plot
ggplot(healthData, aes(x = income_th, y = teenbirths)) +
  geom_point() +  # Scatter plot
  labs(title = "Scatter Plot of Teen Births vs. Income (in Thousands)",
       x = "Income (Thousands of $)",
       y = "Teen Births") +
  theme_minimal()

#Qf Regression
model <- lm(teenbirths ~ income_th, data = healthData)

# Display the model summary with 8-digit precision
summ(model)

ggplot(healthData, aes(x = income_th, y = teenbirths)) +
  geom_point() +  # Scatter plot of data points
  geom_smooth(method = "lm", col = "blue") +  # Add regression line
  labs(title = "Regression of Teen Births on Income",
       x = "Income",
       y = "Teen Births")+
  theme_minimal()

#Qh

model <- lm(teenbirths ~ uerate, data = healthData)

# Display the model summary with 8-digit precision
summ(model)

ggplot(healthData, aes(x = income_th, y = uerate)) +
  geom_point() +  # Scatter plot of data points
  geom_smooth(method = "lm", col = "blue") +  # Add regression line
  labs(title = "Regression of Teen Births on Unemployment",
       x = "Unemployment Rate",
       y = "Teen Births")+
  theme_minimal()

#QLast

model <- lm(teenbirths ~ uerate, data = healthData)

# Get the residuals
residuals <- resid(model)

# Calculate the mean of residuals
mean_residual <- mean(residuals)

# Display the mean residual value
mean_residual


