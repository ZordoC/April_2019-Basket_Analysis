if(require("pacman")=="FALSE"){
  install.packages('pacman')
  library('pacman')
  pacman::p_load(here, stringr, readxl, plyr, caret, dplyr, doParallel,rstudioapi,gdata,
                 lubridate, crayon, corrplot, ggplot2, e1071, reshape2, tidyverse, arules, arulesViz)
}

current_path = getActiveDocumentContext()$path
setwd(dirname(current_path))
setwd("..")
rm(current_path)
Data <- read.csv(file = "./data/ElectronidexTransactions2017.csv", header = TRUE , sep =',')

trans<-read.transactions("./data/ElectronidexTransactions2017.csv",
                         format="basket",sep=",",rm.duplicates = TRUE) 


#### Get product Names into a vector ####


DataToTransf<-as.data.frame(as(trans, "matrix"))


#DataToTransf[DataToTransf==TRUE] <- colnames(DataToTransf)[which(DataToTransf==TRUE, arr.ind=TRUE)[,'col']] 


##### Manipulate data-frame #####

Data$rownumber <- 1:nrow(Data)


DataReady <- melt(Data,"rownumber")

DataReady <-  DataReady %>%  filter(value != "") %>%
  arrange(rownumber) 




#### Subbing product Names, by categories ####

Laptop <- grep(c("Laptop|Aspire|Chromebook|MacBook"),DataReady$value)
Desktop <- grep(c("Desktop|iMac"),DataReady$value)
Tablets <- grep(c("iPad|Tablet|Tab|kindle"),DataReady$value)
Monitor <- grep(c("Monitor"),DataReady$value)
#Keyboard <-grep(c("Mouse|Keyboard"),DataReady$value)
Drives <- grep(c("Hard Drive"),DataReady$value)
Speakers <- grep(c("Speakers|Speaker|Sonos|Cyber Acoustics|DOSS"),DataReady$value)
Cables <- grep(c("Cable|HDMI Adapter"),DataReady$value)
#Combo <- grep(c("Combo"),DataReady$value)
ActiveHeadPhones <- grep(c("Apple Earpods|Monster Beats|Wireless Sports|In-Ear|APIE|Panasonic On-Ear Stereo Headphones RP-HT21|Philips Flexible Earhook Headphone|Panasonic On-Ear Stereo Headphones"),DataReady$value)
Acessories <- grep(c("Microsoft Office Home and Student 2016|Computer Game|Belkin Mouse Pad|Large Mouse Pad"),DataReady$value)
Printers <- grep(c("Epson Printer|HP Wireless Printer|Canon Office Printer|Brother Printer|DYMO Label Manker"),DataReady$value)
PrintersINk <- grep(c("Epson Black Ink|HP Black & Tri-color Ink|Canon Ink|Brother Printer Toner|DYMO Labeling Tape"),DataReady$value)
CStands <- grep(c("Halter Acrylic Monitor Stand|Height-Adjustable|Multi Media Stand|Full Motion Monitor Mount"),DataReady$value)
SmartHome <- grep(c("Apple TV|Google Home|Smart Light Bulb|Fire TV Stick|Roku Express"),DataReady$value)
Mouse <- grep(c("3-Button Mouse|Logitech Wireless Mouse|Logitech 3-button Mouse|Redragon Gaming Mouse|HP Wireless Mouse|Generic Black 3-Button|Slim Wireless Mouse|Microsoft Basic Optical Mouse|Wireless Portable Mouse|Gaming Mouse Professional"),DataReady$value)
Keyboard <- grep(c("HP USB Keyboard|Logitech Wireless Keyboard|Rii LED Keyboard|Logitech Keyboard|Backlit LED Gaming Keyboard|Dell Wired Keyboard|Apple Wired Keyboard|Apple Wireless Keyboard|Apple Magic Keyboard"),DataReady$value)
Combo <- grep(c("Logitech MK550 Wireless Wave Keyboard and Mouse Combo|Logitech Desktop MK120 Mouse and keyboard Combo|Logitech MK270 Wireless Keyboard and Mouse Combo|Dell KM117 Wireless Keyboard & Mouse|EagleTec Wireless Combo Keyboard and Mouse|Microsoft Wireless Comfort Keyboard and Mouse|Microsoft Wireless Desktop Keyboard and Mouse|Rii LED Gaming Keyboard & Mouse Combo|Logitech MK360 Wireless Keyboard and Mouse Combo"),DataReady$value)
ComputerHeadPhones <- grep(c("Zombie Gaming Headset|Logitech ClearChat Headset|Panasonic On-Ear Stereo Headphones RP-HT21|PC Gaming Headset|Kensington Headphones|Logitech Stereo Headset|Koss Home Headphones|Microsoft Headset|Ailihen Stereo Headphones|XIBERIA Gaming Headset"),DataReady$value)
##
DataReady$value -> Names

Names <- replace(Names,Laptop,"Laptop")
Names <- replace(Names,Desktop,"Desktop")
Names <- replace(Names,Tablets,"Tablet")
Names <- replace(Names,Monitor,"Monitor")
Names <- replace(Names,Keyboard,"Keyboard")
Names <- replace(Names,Drives,"Driver")
Names <- replace(Names,Speakers,"Speakers")
Names <- replace(Names,Cables,"Cables")
Names <- replace(Names,Combo,"Combo")
Names <- replace(Names,ActiveHeadPhones,"Active Headphone")
Names <- replace(Names,Acessories,"Acessories")
Names <- replace(Names,Printers,"Printer")
Names <- replace(Names,PrintersINk,"Printer Ink")
Names <- replace(Names,CStands,"Computer Stand")
Names <- replace(Names,SmartHome,"Smart-Home Device")
Names <- replace(Names,Mouse,"Mice")
Names <- replace(Names,ComputerHeadPhones,"Computer Headphone")

####Split ####
DataReady <- cbind(DataReady,Names)

DataByCategory <- DataReady[,c(1,3,4)]



laptops <- c()



for(i in 1:9835){

laptops[i] <-sum(DataByCategory$rownumber == i  & DataByCategory =="Laptop")

laptops

}

 desktops <- c()

for(i in 1:9835){
  
  desktops[i] <-sum(DataByCategory$rownumber == i  & DataByCategory =="Desktop")
  
  desktops
  
}
 
printers <- c()

for(i in 1:9835){
  
  printers[i] <-sum(DataByCategory$rownumber == i  & DataByCategory =="Printer")
  
  
  
}

monitors <- c()

for(i in 1:9835){
  
  monitors[i] <-sum(DataByCategory$rownumber == i  & DataByCategory =="Monitor")
  
  monitors
  
}




#### Conditions for business #### 


DataTran_df<-as.data.frame(as(trans, "matrix"))
DTrue <- DataTran_df +0
MySum <- rowSums(DTrue)

DTrue <-cbind(DTrue,MySum)
Vector <- DTrue[,c("MySum")] > 6
Indices <- which(Vector,TRUE)


MoreThan3laptops <- which(laptops >= 3)
MoreThan2desktops <- which(desktops >= 3) 
MoreThan2Printers <-which(printers>=2)
MoreThan3Monitors <-which(monitors>4 )


BusinessIndices <- (c(MoreThan2desktops,MoreThan2Printers,MoreThan3laptops,MoreThan3Monitors))

BIggerThan6 <- Indices 

B2b2 <- Data[Indices,]

B2b <- Data[BusinessIndices,]

BusinessIndices1 <- as.data.frame(BusinessIndices)
Indices1 <- as.data.frame(Indices)

  B2b3$rownumber

  
B2b3 <- unique(B2b3)
  

B2c <- Data[-B2b3$rownumber,]

B2c <- as.data.frame(B2c)


B2b3 <- B2b3[,-126]

B2bc

B2bT <- as(B2b3,"transactions")

B2cT <- as(B2c,"transactions")





