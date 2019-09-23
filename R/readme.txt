This folder contains R scripts

It contains the folders 
functions/
  Contains user written functions that will be used in the analysis
source/ 
  contains files to be sourced at the top of each scripts
  
It also contains .R file where the code is.

0a. pkg_installs - has the packages required if needed.
0b. source - file that setups the environment with libraries and user written 
			functions
1. prepare_data - creates indicators for the analysis that use hypergrammeR
		  it also calls prepare_disagg which create the disaggregation variables
2. analysis_nonhouseholdlevel_denoms - creates variables and outputs for 
		analyses that have different denominators so are not done in 
		hypergrammeR
3. run_analysis - runs the hypergrammeR analyses
4. expoer_datamerge_to_indesign - creates a nicer format for the indesign
			factsheet.
