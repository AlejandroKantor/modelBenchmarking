# modelBenchmarking

This repository contains the program *modelBenchmarking.R* which allows for the comparison of performance of several supervised models and a data set with predetermined target variable and explanatory variables. See [./documentation/documentation.pdf][doc] for more information.


## Main Files

| File   | Description |
|:-------|:------|
| [./modelBenchmarking.RProj][proj]   |    R project file  |
| [./scripts/makeModelBenchmarking.R][makeR]   |  Main script |
| [./scripts/scripts.R][makeR]    | Functions called by main script |
| [./documentation/documentation.pdf][doc]    |     Documentation |
| [./documentation/documentation.tex][doc]    |     Latex document of documentation |


## Usage

Open ./modelBenchmarking.RProj, then open ./scripts/makeModelBenchmarking.R within the R Project. Run or source ./scripts/makeModelBenchmarking.R. Optionally, we can modify the data set and parameters used chanching the following code.

```r
#-----------------------------------------
# import data
#-----------------------------------------
library(MASS)

data(Boston)
dt_data <- data.table(Boston)

# transform nominal variables to factor
dt_data[, chas:=factor(chas)]
dt_data[, rad:=factor(rad)]

#-----------------------------------------
# set parameters
#-----------------------------------------
# set target variable and explanatory variables
s_y <- "medv"
v_x <- setdiff( names(dt_data), s_y)

# set number of groups for cross validation
i_num <- 8

```


[doc]: https://github.com/AlejandroKantor/modelBenchmarking/tree/master/documentation/documentation.pdf

[proj]: https://github.com/AlejandroKantor/modelBenchmarking/blob/master/modelBenchmarking.Rproj

[makeR]: https://github.com/AlejandroKantor/modelBenchmarking/tree/master/scripts/makeModelBenchmarking.R

[ScriptR]: https://github.com/AlejandroKantor/modelBenchmarking/tree/master/scripts/functions.R

[tex]: https://github.com/AlejandroKantor/modelBenchmarking/tree/master/documentation/documentation.tex