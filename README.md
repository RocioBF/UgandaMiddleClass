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
* haven
* tidyr
* readr
* data.table
* dplyr
* tibble
* ggplot2
* purrr
* sm
* KernSmooth
* locpol
* stargazer
* HH
* ineq
* readxl
* reshape2

### Brief explanation of the different scripts:
#### Code_database.R
This script is used to call all data that is related to the methodology analysis. Furthermore, varibles are renamed to be used in the analysis.
#### data_13.R
In this script the data frame for the year 2013 is created. This data contains personal information of the head of the household, education level, sector of the labor activity, household sanitation, energy use and assets hold.  
#### data_11.R
In this script the data frame for the year 2011 is created. This data contains personal information of the head of the household, education level, sector of the labor activity, household sanitation, energy use and assets hold.
#### main_data_frame.R
I use this section to append the data frames from 2011 and 2013.
#### regression_variables.R
In this script the varibales as used in the main regression of the paper are created.
#### methodology.R
In this script the classification of households according with to their vulnerability is made. Also, you can find the vulnerability analysis of the methodology choices.
The first section classifies households into four categories: *Poor_poor*, *nonpoor_poor*, *poor_nonpoor* and *nonpoor_nonpoor*.
The second section presents the regressions made for the analysis. 
In the third section, I match the estimated prob with the estimated expenditure using a local linear regression approach.
The fourth section classifies households into four groups: poor, vulnerable, middle class and elite. Tables for the population distirbution and transition to poverty can be found also in this section.
For the analysis of the use of different poverty lines you should go to the 1st section of the script. There, several possibilities for the poverty line are provided. You will need to change *poverty_line* variable to see the changes in the results. The analysis of the lower and upper bound can be found in the 5th section. The two last sections are the robustness checks. 
#### MC_characteristics.R
In this script the middle class is analyze in comparison with the other social classes. There you can find the representation of income shares by social classes, income distribution graphs, Lorenz curve and calculations for the Gini coefficient. Moreover, a t-test is performed to be able to test the differences between social classes.
#### Table_1.R
This script corresponds to the Table that presents the share of population in Uganda using different methods from the existing literature. 
#### Table_2.R
This script presents the characteristics of the households in 2011
#### Uganda_background.R
This script is used to plot some economic indicators of Uganda and compare them with Africa.



