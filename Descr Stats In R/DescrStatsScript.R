#***************************************************
# This program provides the R script template with partial solutions to M02_HW
#
# This homework provides an opportunity to practice generating
# descriptive statistics and visualizations in R, and then interpreting them.
#
# Last Modified:  08/10/23  Created
#                 09/8/24  Modified
#
# Authors:  Nick Williams, University of Cincinnati
#           Chris Laney, University of Cincinnati
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

#**********************************************************
# Read in the catalog data.  This data contains a random sample of
# 1000 customer's orders from a company that uses catalogs to sell its products. 
#
# -- List of Variables ---
# Salary     = Total household salary, in $
# Children   = Number of children in the household
# Catalogs   = Number of catalogs sent to the household in a year
# AmountSpent= Total amount spent by household, in $
#**********************************************************

#********* part b) ***************************************
# ==== Read in Data.  Since this is R data set, can use load command
# load(catalogs_sub)

# === Take a quick look at the dataset (can also use the glimpse command)
head(catalogs_sub)

#********* part c) ***************************************
# Using "freq" from summarytools package
freq(catalogs_sub$Children)
freq(catalogs_sub$Catalogs)

# Using CrossTable from gmodels package.  Commented out.  Remove comment (#) if summarytools does not work
#CrossTable(catalogs_sub$Children)
#CrossTable(???????)

#********* part d) ***************************************
CrossTable(catalogs_sub$Children,catalogs_sub$Catalogs, prop.chisq = FALSE)


# D (i)	How many households in the sample with no children received 12 or more catalogs?
no_children_12_or_more_catalogs <- catalogs_sub[catalogs_sub$Children == 0 & catalogs_sub$Catalogs >= 12, ]
num_households <- nrow(no_children_12_or_more_catalogs)
num_households

#D (i-b) How many with 1 or more children received 12 or more catalogs? 
no_children_12_or_more_catalogs <- catalogs_sub[catalogs_sub$Children >= 1 & catalogs_sub$Catalogs >= 12, ]
num_households <- nrow(no_children_12_or_more_catalogs)
num_households

#D (ii)	Given that the household received 6 catalogs in a year, what percentage of these households had 1 or more children?
one_child_6_or_more_catalogs <- catalogs_sub[catalogs_sub$Catalogs == 6 & catalogs_sub$Children >= 1, ]
num_households <- nrow(one_child_6_or_more_catalogs)
total_households_6_catalogs <- catalogs_sub[catalogs_sub$Catalogs == 6,]
percent_6_catalog_1_child = (num_households / nrow(total_households_6_catalogs)) * 100
percent_6_catalog_1_child


#********* part e) ***************************************
#(i)
descr(catalogs_sub$Salary)

#********* part f) ***************************************
# First create a subset of the data that includes only fulltime workers ??? I think this is wrong? In word it says recieved 18 or more catalogs
cat18 <- catalogs_sub[catalogs_sub$Catalogs >= 18,]


# Then create your boxplot -- MAKE SURE TO USE SUBSET!
ggplot(data = cat18, mapping = aes(group = Catalogs,y=AmountSpent)) +
  geom_boxplot() 
  
#********* part g) ***************************************
ggplot(catalogs_sub, aes(x = AmountSpent)) +
  geom_histogram(binwidth = 10, fill = "white", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Amount Spent", x = "Amount Spent", y = "Num Households") +
  geom_vline(aes(xintercept=mean(AmountSpent)),color="blue",linewidth=1) +
  geom_vline(aes(xintercept=median(AmountSpent)),color="red",linewidth=1)




