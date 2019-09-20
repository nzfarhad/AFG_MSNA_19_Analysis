# source.r

# This setups the environment for analysis

# anything the needs to be setup for all analyses
# goes in the source folder
# this will be links to data
# and libraries that are loaded
source <- here::here("R", 
                "source")

# any user written functions go in the functions
# folder

functions <- here::here("R", 
                  "functions")

# Get all of the scripts in the source and functions
# folders
scripts <- dir(source, full.names = TRUE)
scripts_fun <- dir(functions, full.names = TRUE)

# Run all the scripts
for (script in scripts){
  source(script)
}
for (script in scripts_fun){
  source(script)
}