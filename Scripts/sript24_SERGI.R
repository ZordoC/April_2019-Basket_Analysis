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

inspect (tr) # You can view the transactions. Is there a way to see a certain # of transactions?
length (tr) # Number of transactions.
M<-size (tr) # Number of items per transaction
LIST(tr) # Lists the transactions by conversion (LIST must be capitalized)
itemLabels(tr)# To see the item labels

#histogram of the number of transactions
#may need to do dev.off
dev.off()
hist(M, breaks=30)

#top 10 most purchased items
itemFrequencyPlot(tr, topN=10)

image1<-image(sample(tr[1:100]))
image1

####2.CREATING RULES####
Rules<- apriori (tr, parameter = list(supp = 0.002, conf = 0.75))

inspect(Rules)
summary(Rules)
plot(Rules)

####3.RULES ANALYSIS####
#sort by (conf, support, lift...)
inspect(sort(Rules, by = "lift"))
Rules.sorted<-sort(Rules, by = "lift", order = FALSE)

#subset by item name
ItemRules <- subset(Rules, items %in% "iMac")
ItemRules

#check for redundant rules
is.redundant(Rules)

#show the redundant rules
which(is.redundant(Rules), arr.ind = TRUE, useNames = TRUE)
inspect(Rules[c(63,73)])


#Plotting the rules
plot(Rules, method="graph", control=list(type="items"))
plot(Rules, method="paracoord", control=list(reorder=TRUE))
