library(arules)
library(arulesViz)



#doing tutorial#
??arules
??read.transactions

trans<-read.transactions("C:/Users/eloic/Desktop/01. Ubiqum 2019 DATA/DA II/ElectronidexTransactions2017.csv",format="basket",sep=",",rm.duplicates = TRUE)
View(trans)
str(trans)
inspect (trans[9828]) # You can view the transactions. Is there a way to see a certain # of transactions?
length(trans)
size(trans[9828])
LIST(trans[9828])
itemLabels(trans)# To see the item labels

####PLOTS####
itemFrequencyPlot(trans,topN=10, support=3)
??itemFrequencyPlot
??image
image(sample(trans,120))
image(trans, topN=10)
??image()
image(trans[900:1000])
??apropri


####APRIORI####
outofthebox<- apriori (trans, parameter = list(supp = 0.002, conf = 0.9))
inspect(outofthebox)
outofthebox
#what is minlen

summary(outofthebox)
inspect(sort( outofthebox, by = "lift"))
?subset
itemsoutofthebox <- subset(outofthebox, items %in% "Apple MacBook Air")


####titanic tutorial go throguh it####



####check duplicates####
n_occur <- data.frame(table(ElectronicTransactions2017$id))

labels_df<-trans@itemInfo$labels
labels_df

####DAY 2####
####STEP 1 CREATE TABLE####
DataTran_df<-as.data.frame(as(trans, "matrix"))
View(DataTran_df)

#assign label column to rows TRUE
DataTran_df[DataTran_df==TRUE] <- colnames(DataTran_df)[which(DataTran_df==TRUE, arr.ind=TRUE)[,'col']]

#assign condition apple lovers

DataTran_df$Apple<-0
DataTran_df$Apple[1]


DataTran_df$Apple = ifelse (DataTran_df$`Apple Earpods`!=FALSE & DataTran_df$`Apple TV`!=FALSE, value_1, df$col_4 )

