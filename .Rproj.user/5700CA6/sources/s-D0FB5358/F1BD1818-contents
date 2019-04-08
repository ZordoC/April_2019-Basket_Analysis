if(require("pacman")=="FALSE"){
  install.packages('pacman')
  library('pacman')
  pacman::p_load(here, stringr, readxl, plyr, caret, dplyr, doParallel,
                 lubridate, crayon, corrplot, ggplot2, e1071, reshape2, tidyverse, arules, arulesViz)
}



trans<-read.transactions("/home/zordo/Documents/Ubiqum/R-M2Task4/RM2T4/data/ElectronidexTransactions2017.csv",
                         format="basket",sep=",",rm.duplicates = TRUE) 


DataTran_df<-as.data.frame(as(trans, "matrix"))


Sum <- apply(DataTran_df,1,sum)


DataTran_df <-  cbind(DataTran_df,Sum)


DataTran_df[DataTran_df==TRUE] <- colnames(DataTran_df)[which(DataTran_df==TRUE, arr.ind=TRUE)[,'col']]


ByCategories <- unite(data = DataTran_df,Laptops,Laptop)




ColumnsNames<- colnames(DataTran_df)


obj = c("food", "clothing", "entertainment", "forest")
mylist = mget(obj) 
mylist = lapply(obj, function(x){
  temp = mylist[[x]]
  setNames(rep(x, length(temp)), temp)
})
mylist = unlist(mylist)

df[-1] = lapply(df[-1], function(x) as.vector(mylist[as.character(x)]))


mylist = mget(ColumnsNames)


##https://stackoverflow.com/questions/54895370/how-to-replace-multiple-values-among-multiple-columns-in-r-dataframe##


####Categories####
Laptop <- grep(c("Laptop|Aspire|Chromebook|MacBook"),ColumnsNames)

Desktop <- grep(c("Desktop|iMac"),ColumnsNames)

Tablets <- grep(c("iPad|Tablet|Tab|kindle"),ColumnsNames)

Monitor <- grep(c("Monitor"),ColumnsNames)

Keyboard <-grep(c("Mouse|Keyboard"),ColumnsNames)

Drives <- grep(c("Hard Drive"),ColumnsNames)

Speakers <- grep(c("Speakers|Speaker|Sonos|Cyber Acoustics|DOSS"),ColumnsNames)

Cables <- grep(c("Cable|HDMI Adapter"),ColumnsNames)

Combo <- grep(c("Combo"),ColumnsNames)

ActiveHeadPhones <- grep(c("Apple Earpods|Monster Beats|Wireless Sports|In-Ear|APIE|Panasonic On-Ear Stereo Headphones RP-HT21|Philips Flexible Earhook Headphone|Panasonic On-Ear Stereo Headphones"),trans@itemInfo$labels)

Acessories <- grep(c("Microsoft Office Home and Student 2016|Computer Game|Belkin Mouse Pad|Large Mouse Pad"),ColumnsNames)

Printers <- grep(c("Epson Printer|HP Wireless Printer|Canon Office Printer|Brother Printer|DYMO Label Manker"),ColumnsNames)

PrintersINk <- grep(c("Epson Black Ink|HP Black & Tri-color Ink|Canon Ink|Brother Printer Toner|DYMO Labeling Tape"),ColumnsNames)

CStands <- grep(c("Halter Acrylic Monitor Stand|Height-Adjustable Standing Desk|Multi Media Stand|Full Motion Monitor Mount"),ColumnsNames)

SmartHome <- grep(c("Apple TV|Google Home|Smart Light Bulb|Fire TV Stick|Roku Express"),ColumnsNames)

Mouse <- grep(c("3-Button Mouse|Logitech Wireless Mouse|Logitech 3-button Mouse|Redragon Gaming Mouse|HP Wireless Mouse|Generic Black 3-Button|Slim Wireless Mouse|Microsoft Basic Optical Mouse|Wireless Portable Mouse|Gaming Mouse Professional"),ColumnsNames)

Keyboard <- grep(c("HP USB Keyboard|Logitech Wireless Keyboard|Rii LED Keyboard|Logitech Keyboard|Backlit LED Gaming Keyboard|Dell Wired Keyboard|Apple Wired Keyboard|Apple Wireless Keyboard|Apple Magic Keyboard"),ColumnsNames)

Combo <- grep(c("Logitech MK550 Wireless Wave Keyboard and Mouse Combo|Logitech Desktop MK120 Mouse and keyboard Combo|Logitech MK270 Wireless Keyboard and Mouse Combo|Dell KM117 Wireless Keyboard & Mouse|EagleTec Wireless Combo Keyboard and Mouse|Microsoft Wireless Comfort Keyboard and Mouse|Microsoft Wireless Desktop Keyboard and Mouse|Rii LED Gaming Keyboard & Mouse Combo|Logitech MK360 Wireless Keyboard and Mouse Combo"),ColumnsNames)

ComputerHeadPhones <- grep(c("Zombie Gaming Headset|Logitech ClearChat Headset|Panasonic On-Ear Stereo Headphones RP-HT21|PC Gaming Headset|Kensington Headphones|Logitech Stereo Headset|Koss Home Headphones|Microsoft Headset|Ailihen Stereo Headphones|XIBERIA Gaming Headset"),ColumnsNames)

#### 

