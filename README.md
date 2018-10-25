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


### Requirements:


### Brief explanation of the different scripts:
