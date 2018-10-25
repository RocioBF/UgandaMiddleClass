# Defining the Middle Class in Developing Countries: Evidence from Uganda's Household Panel Survey
This is the code of my Master Thesis about the identification and analysis of the new middle class in Uganda using panel survey data from the Uganda Bureau of Statistics.


### How to run the Code:
1. Download or clone the repository to your local machine.
2. Download the [National Panel Survey Data](http://microdata.worldbank.org/index.php/catalog/lsms) for 2011 and 2013 from [Uganda Bureau of Statistics](https://www.ubos.org/) - Note that you will need to ask for permission.
You will need the .csv Version for 2013 and stata format for 2011.
3. Download the data for the Economic Indicators of Uganda (upon request from the author). 
4. Place the data in the following structure in the folder
```
UgandaMiddleClass
│   [Code files of the repository]    
│
└───raw_data
│       └───background_uganda_data
|           | [Files of the Economic Indicators]
│       └───UGA_2011_UNPS_v01_M_Stata8
|           | [Files from the National Panel Survey for 2011] 
│       └───UGA_2013_UNPS_v01_M_CSV
|           | [Files from the National Panel Survey for 2013] 
```
5. Run the script `Main_code.R`. It will call all other relevant scripts accordingly. To see specific regression tables or intermediate
varibles you might need to uncomment parts of the code. Further explanations for this can be found in the respective scripts.
6. Warnings can be ignored.

### Requirements:
R version 3.5.0 
List of packages needed: 
haven
tidyr
readr
data.table
dplyr
tibble
ggplot2
purrr
sm
KernSmooth
locpol
stargazer
HH
ineq
readxl
reshape2

### Brief explanation of the different scripts:
#### Code_database.R
This script is used to call all data that is related to the methodology analysis. Furthermore, varibles are renamed to be used in the analysis.
#### data_13.R
In this script the data frame for the year 2013 is created. This data contains personal information of the head of the household, education level, sector of the labor activity, household sanitation, energy use and assets hold.  
#### data_11.R
In this script the data frame for the year 2011 is created. This data contains personal information of the head of the household, education level, sector of the labor activity, household sanitation, energy use and assets hold.
#### main_data_frame.R

#### regression_variables.R
#### methodology.R
#### MC_characteristics.R
#### Table_1.R
#### Table_2.R 
#### Uganda_background.R



