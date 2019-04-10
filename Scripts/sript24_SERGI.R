####0.LOAD LIBRARIES AND DATA####

if(require("pacman")=="FALSE"){
  install.packages('pacman')
  library('pacman')
  pacman::p_load(here, stringr, readxl, plyr, caret, dplyr, doParallel,
                 lubridate, crayon, corrplot, ggplot2, e1071, reshape2, tidyverse, arules, arulesViz)
} else {
  library('pacman')
  pacman::p_load(here, stringr, readxl, plyr, caret, dplyr, doParallel,
                 lubridate, crayon, corrplot, ggplot2, e1071, reshape2, tidyverse, arules, arulesViz)
}
#DATA
ElectronidexTransactions2017 <- read_csv("C:/Users/Sergi Ch/Downloads/UBIQUM/PROJECT 2 - R/RM2T4/Datasets/ElectronidexTransactions2017.csv", 
                                         +     col_names = FALSE)
#SET DATA AS A TRANSACTION
tr <- read.transactions("C:/Users/Sergi Ch/Downloads/UBIQUM/PROJECT 2 - R/RM2T4/Datasets/ElectronidexTransactions2017.csv", format = "basket", sep=",",  rm.duplicates=FALSE)

#### 1.DATA ANALYSIS####

#List all the transactions
inspect(tr)
#Save the matrix with transactions numbers 
M<-size(tr)
M

#histogram of the numer of transactions
hist(M, breaks=30)
hist(M, freq=FALSE, main="Density plot")
itemFrequencyPlot(tr, topN=5)
