library(arules)
library(readr)
library(arulesViz)
library(rstudioapi)




current_path = getActiveDocumentContext()$path
setwd(dirname(current_path))
setwd("..")
rm(current_path)
EP <- read.csv( file ="./data/ElectronidexTransactions2017.csv" , header = TRUE , sep = ',')

