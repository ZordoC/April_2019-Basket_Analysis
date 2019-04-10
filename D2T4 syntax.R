library(arules)
library(arulesViz)
library(data.table)
library(dplyr)
library(here)
setwd("C:/Users/eloic/Documents/R")

####dont know how to set relative working directory####
#> here() starts at /"C:/Users/eloic/Desktop/01. Ubiqum 2019 DATA/DA II"
here("R")
#> [1] C:/Users/eloic/Desktop/01. Ubiqum 2019 DATA/DA II"
here()


####DAY 1 PLAN OF ATTACK####
trans<-read.transactions("C:/Users/eloic/Desktop/01. Ubiqum 2019 DATA/DA II/ElectronidexTransactions2017.csv",format="basket",sep=",",rm.duplicates = TRUE)
ElectronidexTransactions2017 <- read.csv("C:/Users/eloic/Desktop/01. Ubiqum 2019 DATA/DA II/ElectronidexTransactions2017.csv")
#str(trans)
#inspect (trans[9828]) # You can view the transactions. Is there a way to see a certain # of transactions?
#length(trans)
#size(trans[9828])
#LIST(trans[9828])
#itemLabels(trans)# To see the item labels

####PLOTS####
itemFrequencyPlot(trans,topN=10, support=3)
#??itemFrequencyPlot
#??image
#image(sample(trans,120))
#image(trans, topN=10)
#??image()
#image(trans[900:1000])
#??apropri

####DAY 2####
####STEP 1 CREATE TABLE####
DataTran_df_true<-as.data.frame(as(trans, "matrix"))
#View(DataTran_df_true)

#assign 0 and 1
DF01<-DataTran_df_true+0
#View(DF01)
plot(DF01$`HP Desktop`)
Databinary<-write.csv(DF01,"Binarydata.csv", row.names=TRUE)


#test not use
#TestApple<-DF01
#TestApple$AppleYes<-ifelse(DF01$`Apple Earpods`==1|DF01$`Apple MacBook Air`|DF01$`Apple MacBook Pro`==1|DF01$`Apple Magic Keyboard`==1|DF01$`Apple TV`==1|DF01$`Apple Wired Keyboard`==1|DF01$`Apple Wireless Keyboard`==1|DF01$`iMac`==1|DF01$`iPad`|DF01$`iPad Pro`==1|DF01$`iPhone Charger Cable`==1,1,0)
#View(TestApple)
#summary(TestApple)  
####Deplyr####
#TestApple %>% filter(`Apple Earpods`==0|`Apple MacBook Air`) %>% select(`Apple Earpods`,`Apple MacBook Air`,AppleYes)
#assign label column to rows TRUE
#DataTran_df[DataTran_df==TRUE] <- colnames(DataTran_df)[which(DataTran_df==TRUE, arr.ind=TRUE)[,'col']]

#assign condition apple lovers
DF01$Apple<-0
  
#respondents who bought at least 1 apple product
DF01$AppleLovers<-ifelse(DF01$`Apple Earpods`==1|DF01$`Apple MacBook Air`==1|DF01$`Apple MacBook Pro`==1|DF01$`Apple Magic Keyboard`==1|DF01$`Apple TV`==1|DF01$`Apple Wired Keyboard`==1|DF01$`Apple Wireless Keyboard`==1|DF01$`iMac`==1|DF01$`iPad`|DF01$`iPad Pro`==1|DF01$`iPhone Charger Cable`==1,1,0)
dfAppleLovers<-cbind(DF01,DF01$AppleLovers)
summary(dfAppleLovers)
####split dataset Apple not Apple###
dfAppleLovers$AppleLovers<-NULL

#onlyapple
OnlyApple<-subset(dfAppleLovers,DF01$AppleLovers==1)
View(OnlyApple)
OnlyApple$DF01<-NULL
OnlyApplecsv<-write.csv(OnlyApple,"Only Apple2.csv", row.names=TRUE)

####DAY 4 using apropri on Apple####
Only.Apple2 <- read.csv("C:/Users/eloic/Desktop/01. Ubiqum 2019 DATA/DA II/Only Apple2.csv")

Only.Apple2$X->rownumber2
OnlyApple<-ElectronidexTransactions2017[rownumber2,]
OnlyApple_trans<-write.csv(OnlyApple,"OnlyApple_trans.csv",row.names = TRUE)
OnlyApple_trans<-read.transactions("C:/Users/eloic/Desktop/01. Ubiqum 2019 DATA/DA II/OnlyApple_trans.csv",format="basket",sep=",",rm.duplicates = TRUE)
itemFrequencyPlot(OnlyApple_trans, support = 0.1)
apriori(OnlyApple_trans)
Applerules <- apriori(OnlyApple_trans, parameter = list(support = 0.006, confidence = 0.25, minlen = 2))
summary(Applerules)
inspect(sort(Applerules, by = "lift")[1:5])
imacrules <- subset(Applerules, items %in% "iMac")
inspect(imacrules)
ruleExplorer(Applerules)

####DAY 4 using apriori not Apple####
Not.Apple$X -> rownumber
NotApple <- ElectronidexTransactions2017[rownumber,]
NotApple_trans<-write.csv(NotApple,"NotAppletrans.csv",row.names = TRUE)


NotAppletrans<-read.transactions("C:/Users/eloic/Desktop/01. Ubiqum 2019 DATA/DA II/NotAppletrans.csv",format="basket",sep=",",rm.duplicates = TRUE)
