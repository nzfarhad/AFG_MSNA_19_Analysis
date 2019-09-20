# helper functions



# Takes in data and a list of vars in the format
# c("var1", "var2", "var3", "var4") and spits out 
# the sum of the row ignoring missing values
comp_score <- function(data, vars) {
  rowsum(data[vars], na.rm = TRUE)
  
}


