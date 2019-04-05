# R shiny app distributions

under construction

Before using the shiny app distributions you should install all required R packages

required_lib <- c("shiny", "DT")

new_lib <- required_lib[!(required_lib %in% installed.packages()[, "Package"])]

if(length(new_lib)) install.packages(new_lib, dependencies = TRUE)

Run in RStudio

shiny::runGitHub( "distributions", "statistical-bulb") 

