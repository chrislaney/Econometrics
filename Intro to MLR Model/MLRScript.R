#***************************************************************
# This program is used in Module 5 HOMEWORK.  Focused on MLR
# regression intepretation and significance testing.  
#
# Last Modified:  02/07/2021  Created
#                 02/01/2023  Modified for Spring 2023
#                 09/20/2024  Updated
#
# Authors:  Nick Williams, University of Cincinnati
#****************************************************

## ****************** PUT THIS CODE IN ALL OF YOUR SCRIPTS ********************************************
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
options("jtools-digits"=3) 

# ****************** LAST BIT OF CODE TO INCLUDE IN ALL OF YOUR SCRIPTS ********************************************
# ****************** LAST BIT OF CODE TO INCLUDE IN ALL OF YOUR SCRIPTS ********************************************

#*************** Read in the Data ******************************#
# There are several ways to do this.  Most students find that 
# using the Files window in the bottom right corner of RStudio is easiest.
#
# To do this, locate the file you want to read into R in the Files list
#
# 1) If the file is an Rdata file, you should be just click on the file to load it
#
# 2) For other files (like *.dta and *.xlsx)
#       a) Click on the file
#       b) Choose the import data option.   This will create a popup asking you
#          if you want to import the data.  
#       c) In the bottom left corner of this popup, will be a box labeled "NAME"
#          Sometimes the dataset name is long -- and you can make your programming
#          easier by choosing a shorter object name for the data.
# 
# Note:  If RStudio needs packages to be installed to be able to read in the data
#        click on "Yes" to allow this.
#***************************************************************#

#-----------------------------------------------------------------------
# -- Read in the data
#-----------------------------------------------------------------------
# Hours worked data for women in the UK, 2008
#
# Variables in data: age = 	age, in years
#                    agechy	age of youngest child in household 
#                    cjnumtrain	number of training periods on current job
#                    educyrs	years of education
#                    expal	years of labor market experience
#                    hhsize	total size of household
#                    hrstot     total hours worked
#                    nchild     number of own children in household
#                    nkids      number of children in houshold
#                   
#-----------------------------------------------------------------------

price_mil <- egypt_housing_clean$price/1000000

#filter for apts
egypt_housing_htype1 <- subset(egypt_housing_clean, htype == 1)
price_mil <- egypt_housing_htype1$price/1000000
#regs for apts only
reg0 = lm(price_mil~size_sqm+bedroom, data=egypt_housing_htype1)
reg1 = lm(price_mil~size_sqm+bedroom+bathroom, data=egypt_housing_htype1)

#Illustrate the Stargazer package.  Useful to produce nice comparison table
stargazer(reg0, reg1, type="text",align=TRUE,
          report="vcsp",
          notes = c("Standard errors in parentheses"),
          keep.stat=c("n","rsq","f"), no.space=FALSE,
          digit.separator = "",digits=3)


descr(egypt_housing_clean[c("price","bedroom","bathroom")])

#QUESTION 2 ====================================================
#rename to county_health

incometh <- county_health$income / 1000

rega <- lm(birthweight~incometh+obesity, data=county_health)
regb <- lm(birthweight~incometh+obesity+diabetes+noenglish, data=county_health)

stargazer(rega, regb, type="text",align=TRUE,
          report="vcsp",
          notes = c("Standard errors in parentheses"),
          keep.stat=c("n","rsq","f"), no.space=FALSE,
          digit.separator = "",digits=3)






