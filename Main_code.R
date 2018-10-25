# This is the master script which calls all other scripts used in my Master Thesis. 
# It is divided into several sections to clean the data, conduct the analysis and create 
# tables/descriptives.

# Packages needed for running the code:

install.packages("tidyr")
install.packages("haven")
install.packages("data.table")
install.packages("dplyr")
install.packages("tibble")
install.packages("ggplot2")
install.packages("purrr")
install.packages("sm")
install.packages("KernSmooth")
install.packages("locpol")
install.packages("stargazer")
install.packages("HH")
install.packages("scales")
install.packages("ineq")
install.packages("readxl")

# Loading packages: 
library(haven) # Export data from STATA
library(tidyr) # Data manipulation
library(readr) # Export data .csv format
library(data.table) # Data manipulation
library(dplyr) # Data manipulation
library(tibble) # To be able to use tibbles
library(ggplot2) # Regressions and plots
library(purrr) # Multiple join
library(sm) # Non-parametric regression and density estimations
library(KernSmooth) # Kernel distributions and local linear estimations
library(locpol) # local linear regression
library(stargazer) # table of regression to latex
library(HH) # multicolinearity
library(ineq) # gini and lorenz curve
library(readxl) #Read excel files
library(reshape2) 

######################################################################################
# Data Cleaning Section:

# Calling script where I read all data:
source("Code_database.R")

#Calling script for the data in 2013:
source("data_13.R")

#Calling script for the data in 2011:
source("data_11.R")

#Calling script that merge the data from 2011 and 2013:
source("main_data_frame.R")

######################################################################################################
# Methodology Section:

#Calling script for the creation of the variables for the regressions:
source("regression_variables.R")

# Calling script for the methodology of the project:
source("methodology.R")

# Calling script for middle class analysis:
source("MC_characteristics.R")


#######################################################################################
# Descriptives and Table Section:

# TABLE : Share of population in Uganda using different methods:
source("Table_1.R")

# TABLE: Characteristics of hh in 2011:
source("Table_2.R")

# Uganda's Economic Background:
source("Uganda_background.R")
