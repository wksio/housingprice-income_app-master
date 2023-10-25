# init.R 
# 
# Example R code to install packages if not already installed 
# 
my_packages = c("leaflet", "shiny", "shinyWidgets","plotly","DT","dplyr","shinythemes","naniar","imputeTS","rgdal","randomcoloR","ggplot2") 
install_if_missing = function(p) { 
  if (p %in% rownames(installed.packages()) == FALSE) { 
    install.packages(p) 
  } 
} 
invisible(sapply(my_packages, install_if_missing))

