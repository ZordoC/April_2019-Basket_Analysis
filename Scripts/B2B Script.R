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

Data <- read_csv("C:/Users/Sergi Ch/Downloads/UBIQUM/PROJECT 2 - R/RM2T4/Datasets/ElectronidexTransactions2017.csv", col_names = FALSE)
B2b <- read_csv("C:/Users/Sergi Ch/Downloads/UBIQUM/PROJECT 2 - R/RM2T4/Datasets/B2bREAL.txt")
B2b[1]<-NULL

trans<-as(B2b, "transactions")


#set the labels
indicesLaptop <- grepl(c("Laptop|Aspire|Chromebook|MacBook"),trans@itemInfo$labels)
indicesDesktop <- grepl(c("Desktop|iMac"),trans@itemInfo$labels)
indicesTablets <- grepl(c("iPad|Tablet|Tab|Kindle"),trans@itemInfo$labels)
indicesMonitor <- grepl(c("Monitor"),trans@itemInfo$labels)
indicesMiceAndKeyboard <-grepl(c("Mouse|Keyboard"),trans@itemInfo$labels)
indicesHardDRives <- grepl(c("Hard Drive"),trans@itemInfo$labels)
indicesSpeakers <- grepl(c("Speakers|Speaker|Sonos|Cyber Acoustics|DOSS"),trans@itemInfo$labels)
indicesCables <- grepl(c("Cable|HDMI Adapter"),trans@itemInfo$labels)
indicesCombo <- grepl(c("Combo"),trans@itemInfo$labels)
indicesActiveHeadPhones <- grepl(c("Apple Earpods|Monster Beats|Wireless Sports|In-Ear|APIE|Panasonic On-Ear Stereo Headphones RP-HT21|Philips Flexible Earhook Headphone|Panasonic On-Ear Stereo Headphones"),trans@itemInfo$labels)
indicesAcessories <- grepl(c("Microsoft Office Home and Student 2016|Computer Game|Belkin Mouse Pad|Large Mouse Pad"),trans@itemInfo$labels)
indicesPrinters <- grepl(c("Epson Printer|HP Wireless Printer|Canon Office Printer|Brother Printer|DYMO Label Manker"),trans@itemInfo$labels)
indicesPrintersINk <- grepl(c("Epson Black Ink|HP Black & Tri-color Ink|Canon Ink|Brother Printer Toner|DYMO Labeling Tape"),trans@itemInfo$labels)
indicesCStands <- grepl(c("Halter Acrylic Monitor Stand|Height-Adjustable Standing Desk|Multi Media Stand|Full Motion Monitor Mount"),trans@itemInfo$labels)
indicesSmartHome <- grepl(c("Apple TV|Google Home|Smart Light Bulb|Fire TV Stick|Roku Express"),trans@itemInfo$labels)
indicesMouse <- grepl(c("3-Button Mouse|Logitech Wireless Mouse|Logitech 3-button Mouse|Redragon Gaming Mouse|HP Wireless Mouse|Generic Black 3-Button|Slim Wireless Mouse|Microsoft Basic Optical Mouse|Wireless Portable Mouse|Gaming Mouse Professional"),trans@itemInfo$labels)
indicesKeyboard <- grepl(c("HP USB Keyboard|Logitech Wireless Keyboard|Rii LED Keyboard|Logitech Keyboard|Backlit LED Gaming Keyboard|Dell Wired Keyboard|Apple Wired Keyboard|Apple Wireless Keyboard|Apple Magic Keyboard"),trans@itemInfo$labels)
indicesCombo <- grepl(c("Logitech MK550 Wireless Wave Keyboard and Mouse Combo|Logitech Desktop MK120 Mouse and keyboard Combo|Logitech MK270 Wireless Keyboard and Mouse Combo|Dell KM117 Wireless Keyboard & Mouse|EagleTec Wireless Combo Keyboard and Mouse|Microsoft Wireless Comfort Keyboard and Mouse|Microsoft Wireless Desktop Keyboard and Mouse|Rii LED Gaming Keyboard & Mouse Combo|Logitech MK360 Wireless Keyboard and Mouse Combo"),trans@itemInfo$labels)
indicesComputerHeadPhones <- grepl(c("Zombie Gaming Headset|Logitech ClearChat Headset|Panasonic On-Ear Stereo Headphones RP-HT21|PC Gaming Headset|Kensington Headphones|Logitech Stereo Headset|Koss Home Headphones|Microsoft Headset|Ailihen Stereo Headphones|XIBERIA Gaming Headset"),trans@itemInfo$labels)
Names<-trans@itemInfo$labels
#### Levels #### 

trans@itemInfo$labels

#CATEGORIZE THE LABELS
trans@itemInfo$category <- replace(trans@itemInfo$category,indicesLaptop,"Laptop")
trans@itemInfo$category <- replace(trans@itemInfo$category,indicesDesktop,"Desktop")
trans@itemInfo$category <- replace(trans@itemInfo$category,indicesHardDRives,"Acessories")
trans@itemInfo$category <- replace(trans@itemInfo$category,indicesMonitor,"Monitor")
trans@itemInfo$category <- replace(trans@itemInfo$category,indicesSpeakers,"Acessories")
trans@itemInfo$category <- replace(trans@itemInfo$category,indicesTablets,"Tablets")
trans@itemInfo$category <- replace(trans@itemInfo$category,indicesCables,"Accesories")
trans@itemInfo$category <- replace(trans@itemInfo$category,indicesActiveHeadPhones,"Acessories")
trans@itemInfo$category <- replace(trans@itemInfo$category,indicesAcessories,"Acessories")
trans@itemInfo$category <- replace(trans@itemInfo$category,indicesPrinters,"Printers")
trans@itemInfo$category <- replace(trans@itemInfo$category,indicesPrintersINk,"Printers Ink")
trans@itemInfo$category <- replace(trans@itemInfo$category,indicesCStands,"Acessories")
trans@itemInfo$category <- replace(trans@itemInfo$category,indicesSmartHome,"Acessories")
trans@itemInfo$category <- replace(trans@itemInfo$category,indicesMouse,"Acessories")
trans@itemInfo$category <- replace(trans@itemInfo$category,indicesKeyboard,"Acessories")
trans@itemInfo$category <- replace(trans@itemInfo$category,indicesCombo,"Acessories")
trans@itemInfo$category <- replace(trans@itemInfo$category,indicesComputerHeadPhones,"Acessories")


#APPLY RULES
Rules<- apriori (trans, parameter = list(supp = 0.02, conf = 0.8,  minlen=2))

#view and plot the rules
inspect(Rules)
summary(Rules)

#AGGREGATE BY CATEGORY
B2b_cat<-aggregate(Rules, by="category")

#PLOTS
plot(Rules, method="graph", control=list(type="items"))
plot(Rules, method="paracoord", control=list(reorder=TRUE))