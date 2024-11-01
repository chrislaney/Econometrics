#*******************************************************************
# This program is for use in interpreting log and quadratic models
#
# Last Modified:  03/09/2020  Created
#                 10/10/2020  Updated
#                 10/19/2020  Separated 7 LE and 8 LE
#                 09/26/2021  Updated using tidyverse
#                 10/02/2022  Minor cleaning up for Fall 2022
#                 10/13/2024  Updated
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
# ****************** LAST BIT OF CODE TO INCLUDE IN ALL OF YOUR SCRIPTS ***

house <- houseOH %>% 
  subset(saleprice>0) %>%
  subset(acreage>0) %>%
    mutate(ln_saleprice=log(saleprice)) %>%
    mutate(ln_acre=log(acreage)) %>%
    mutate(eq1=as.numeric(basement==1)) %>%
    mutate(eq2=as.numeric(basement==2))

mod1 <- lm(saleprice~acreage+ageconstr+nobedrm+totbath+aircon+eq1+eq2, data=house)

mod2 <- lm(ln_saleprice~acreage+ageconstr+nobedrm+totbath+aircon+eq1+eq2, data=house)

mod3 <- lm(ln_saleprice~ln_acre+ageconstr+nobedrm+totbath+aircon+eq1+eq2, data=house)


stargazer(mod1,mod2,mod3, type="text",align=TRUE,intercept.bottom = FALSE,
          keep.stat=c("n","rsq","f"),no.space=FALSE,df=FALSE,
          report="vcsp",notes="Standard errors in parentheses",
          digit.separator = "",digits=3)

descr(houseOH[c("saleprice","acreage")])

#Q2

dta <- dta %>% mutate(age2=Age^2)
  
reg1 = lm(Amount~MPHover+Age+age2+Black+Hispanic+Female, data=dta)

summ(reg1)


# Assuming no other predictors in the model
#testing = to 0
lht(reg1, c("Age","age2"))

#Testing if they are equal to each other
lht(reg1, c("Age-age2=0"))



