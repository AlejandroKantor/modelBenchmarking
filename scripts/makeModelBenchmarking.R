# R 3.2.2
# Dependencies
# -data.table 
# -MASS
# -partykit 
# -e1071 
# -reshape2 
# -xtable 
# -ggplot2 

#remove any objects
rm(list = ls())

#import functions
source("./scripts/functions.r")

library(data.table)

#--------------------------------------------------------------------------------------------------
# import data
#--------------------------------------------------------------------------------------------------
library(MASS)

data(Boston)
dt_data <- data.table(Boston)

# transform nominal variables to factor
dt_data[, chas:=factor(chas)]
dt_data[, rad:=factor(rad)]

#--------------------------------------------------------------------------------------------------
# set parameters
#--------------------------------------------------------------------------------------------------
# set target variable and explanatory variables
s_y <- "medv"
v_x <- setdiff( names(dt_data), s_y)

# set number of groups for cross validation
i_num <- 8

#--------------------------------------------------------------------------------------------------
# make model bencharking
#--------------------------------------------------------------------------------------------------

makeModelBenchmarking( dt_data,  s_y, v_x ,i_num)

#--------------------------------------------------------------------------------------------------
# compile latex document
#--------------------------------------------------------------------------------------------------
# set working directory to location of .tex file
s_path <- getwd()
setwd(paste0(s_path, "/documentation"))

# run system command
s_system <- 'lualatex -synctex=1 -interaction=nonstopmode "documentation".tex'
system(s_system)

# set working directory to orginal path
setwd(s_path)





