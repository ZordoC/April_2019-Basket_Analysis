if(require("pacman")=="FALSE"){
  install.packages('pacman')
  library('pacman')
  pacman::p_load(here, stringr, readxl, plyr, caret, dplyr, doParallel,
                 lubridate, crayon, corrplot, ggplot2, e1071, reshape2, tidyverse, arules, arulesViz)
}


library(gdata)

trans<-read.transactions("/home/zordo/Documents/Ubiqum/R-M2Task4/RM2T4/data/ElectronidexTransactions2017.csv",
                         format="basket",sep=",",rm.duplicates = TRUE) 


DataTran_df<-as.data.frame(as(trans, "matrix"))


#View(DataTran_df)



#### CSV breakdown  #### 

DataTran_df[DataTran_df==0] <- colnames(DataTran_df)[which(DataTran_df==1, arr.ind=1)[,'col']]


DataTran_df$busimess <- 1


DTrue <- DataTran_df +0


MySum <- rowSums(DTrue)


DTrue <-cbind(DTrue,MySum)

summary(MySum)




save(BiggerThan6,file = "BiggerThan6.csv")

Vector <- DTrue[,c("MySum")] > 6
View(Vector)


Indices <- which(Vector,TRUE)
View(Indices)

BiggerThan6 <-DTrue[Indices,]

SmallerThan6 <- DTrue[-Indices,]


BiggerThan6$busimess <- 1
SmallerThan6$busimess <- 0

B2borB2C<-rbind(BiggerThan6,SmallerThan6)


transB6 = as(BiggerThan6, "transactions")


transB6<-read.transactions("/home/zordo/Documents/Ubiqum/R-M2Task4/RM2T4/BiggerThan6.csv",
                         format="basket",sep=",",rm.duplicates = TRUE) 


str(transB6)


####Levels####


trans@itemInfo$category <- 0

####Indices####


indicesLaptop <- grep(c("Laptop|Aspire|Chromebook|MacBook"),trans@itemInfo$labels)


indicesDesktop <- grep(c("Desktop|iMac"),trans@itemInfo$labels)

indicesTablets <- grep(c("iPad|Tablet|Tab|kindle"),trans@itemInfo$labels)

indicesMonitor <- grep(c("Monitor"),trans@itemInfo$labels)

indicesMice <-

indicesMiceAndKeyboard <-grep(c("Mouse|Keyboard"),trans@itemInfo$labels)
 
indicesHardDRives <- grep(c("Hard Drive"),trans@itemInfo$labels)

indicesSpeakers <- grep(c("Speakers|Speaker|Sonos|Cyber Acoustics|DOSS"),trans@itemInfo$labels)

indicesCables <- grep(c("Cable|HDMI Adapter"),trans@itemInfo$labels)

indicesCombo <- grep(c("Combo"),trans@itemInfo$labels)


indicesActiveHeadPhones <- grep(c("Apple Earpods|Monster Beats|Wireless Sports|In-Ear|APIE|Panasonic On-Ear Stereo Headphones RP-HT21|Philips Flexible Earhook Headphone|Panasonic On-Ear Stereo Headphones"),trans@itemInfo$labels)



indicesAcessories <- grep(c("Microsoft Office Home and Student 2016|Computer Game|Belkin Mouse Pad|Large Mouse Pad"),trans@itemInfo$labels)

indicesPrinters <- grep(c("Epson Printer|HP Wireless Printer|Canon Office Printer|Brother Printer|DYMO Label Manker"),trans@itemInfo$labels)



indicesPrintersINk <- grep(c("Epson Black Ink|HP Black & Tri-color Ink|Canon Ink|Brother Printer Toner|DYMO Labeling Tape"),trans@itemInfo$labels)


indicesCStands <- grep(c("Halter Acrylic Monitor Stand|Height-Adjustable Standing Desk|Multi Media Stand|Full Motion Monitor Mount"),trans@itemInfo$labels)


indicesSmartHome <- grep(c("Apple TV|Google Home|Smart Light Bulb|Fire TV Stick|Roku Express"),trans@itemInfo$labels)


indicesMouse <- grep(c("3-Button Mouse|Logitech Wireless Mouse|Logitech 3-button Mouse|Redragon Gaming Mouse|HP Wireless Mouse|Generic Black 3-Button|Slim Wireless Mouse|Microsoft Basic Optical Mouse|Wireless Portable Mouse|Gaming Mouse Professional"),trans@itemInfo$labels)

indicesKeyboard <- grep(c("HP USB Keyboard|Logitech Wireless Keyboard|Rii LED Keyboard|Logitech Keyboard|Backlit LED Gaming Keyboard|Dell Wired Keyboard|Apple Wired Keyboard|Apple Wireless Keyboard|Apple Magic Keyboard"),trans@itemInfo$labels)

indicesCombo <- grep(c("Logitech MK550 Wireless Wave Keyboard and Mouse Combo|Logitech Desktop MK120 Mouse and keyboard Combo|Logitech MK270 Wireless Keyboard and Mouse Combo|Dell KM117 Wireless Keyboard & Mouse|EagleTec Wireless Combo Keyboard and Mouse|Microsoft Wireless Comfort Keyboard and Mouse|Microsoft Wireless Desktop Keyboard and Mouse|Rii LED Gaming Keyboard & Mouse Combo|Logitech MK360 Wireless Keyboard and Mouse Combo"),trans@itemInfo$labels)

indicesComputerHeadPhones <- grep(c("Zombie Gaming Headset|Logitech ClearChat Headset|Panasonic On-Ear Stereo Headphones RP-HT21|PC Gaming Headset|Kensington Headphones|Logitech Stereo Headset|Koss Home Headphones|Microsoft Headset|Ailihen Stereo Headphones|XIBERIA Gaming Headset"),trans@itemInfo$labels)

#### Replacing Values #### 


trans@itemInfo$category <- replace(trans@itemInfo$labels,indicesLaptop,"Laptop")

trans@itemInfo$category <- replace(trans@itemInfo$category,indicesDesktop,"Desktop")

trans@itemInfo$category <- replace(trans@itemInfo$category,indicesHardDRives,"Hard-Drive")

trans@itemInfo$category <- replace(trans@itemInfo$category,indicesMonitor,"Monitor")

trans@itemInfo$category <- replace(trans@itemInfo$category,indicesSpeakers,"Speakers")

trans@itemInfo$category <- replace(trans@itemInfo$category,indicesTablets,"Tablets")

trans@itemInfo$category <- replace(trans@itemInfo$category,indicesCables,"Cables")

trans@itemInfo$category <- replace(trans@itemInfo$category,indicesActiveHeadPhones,"Active HeadPhones")

trans@itemInfo$category <- replace(trans@itemInfo$category,indicesAcessories,"Acessories")

trans@itemInfo$category <- replace(trans@itemInfo$category,indicesPrinters,"Printers")

trans@itemInfo$category <- replace(trans@itemInfo$category,indicesPrintersINk,"Printers Ink")

trans@itemInfo$category <- replace(trans@itemInfo$category,indicesCStands,"Computer Stands")

trans@itemInfo$category <- replace(trans@itemInfo$category,indicesSmartHome,"Smart Home Devices")

trans@itemInfo$category <- replace(trans@itemInfo$category,indicesMouse,"Mice")

trans@itemInfo$category <- replace(trans@itemInfo$category,indicesKeyboard,"Keyboard")

trans@itemInfo$category <- replace(trans@itemInfo$category,indicesCombo,"Keyboard and Mice Combo")



trans@itemInfo$category <- replace(trans@itemInfo$category,indicesComputerHeadPhones,"Computer Head Phones")





## Test ## 

TestVEctor <- trans@itemInfo$labels 

NewTestVector <- replace(TestVEctor,indicesLaptop,"Laptop")

NewTestVector

## 


