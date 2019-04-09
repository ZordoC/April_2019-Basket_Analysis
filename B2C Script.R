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

B2c <- read_csv("C:/Users/Sergi Ch/Downloads/UBIQUM/PROJECT 2 - R/RM2T4/Datasets/B2cReal.txt")
B2c[1]<-NULL

transC<-as(B2c, "transactions")


#####1.CATEGORY LEVELS####
indicesLaptop <- grepl(c("Laptop|Aspire|Chromebook|MacBook"),transC@itemInfo$labels)
indicesDesktop <- grepl(c("Desktop|iMac"),transC@itemInfo$labels)
indicesTablets <- grepl(c("iPad|Tablet|Tab|Kindle"),transC@itemInfo$labels)
indicesMonitor <- grepl(c("Monitor"),transC@itemInfo$labels)
indicesMiceAndKeyboard <-grepl(c("Mouse|Keyboard"),transC@itemInfo$labels)
indicesHardDRives <- grepl(c("Hard Drive"),transC@itemInfo$labels)
indicesSpeakers <- grepl(c("Speakers|Speaker|Sonos|Cyber Acoustics|DOSS"),transC@itemInfo$labels)
indicesCables <- grepl(c("Cable|HDMI Adapter"),transC@itemInfo$labels)
indicesCombo <- grepl(c("Combo"),transC@itemInfo$labels)
indicesActiveHeadPhones <- grepl(c("Apple Earpods|Monster Beats|Wireless Sports|In-Ear|APIE|Panasonic On-Ear Stereo Headphones RP-HT21|Philips Flexible Earhook Headphone|Panasonic On-Ear Stereo Headphones"),transC@itemInfo$labels)
indicesAcessories <- grepl(c("Microsoft Office Home and Student 2016|Computer Game|Belkin Mouse Pad|Large Mouse Pad"),transC@itemInfo$labels)
indicesPrinters <- grepl(c("Epson Printer|HP Wireless Printer|Canon Office Printer|Brother Printer|DYMO Label Manker"),transC@itemInfo$labels)
indicesPrintersINk <- grepl(c("Epson Black Ink|HP Black & Tri-color Ink|Canon Ink|Brother Printer Toner|DYMO Labeling Tape"),transC@itemInfo$labels)
indicesCStands <- grepl(c("Halter Acrylic Monitor Stand|Height-Adjustable Standing Desk|Multi Media Stand|Full Motion Monitor Mount"),transC@itemInfo$labels)
indicesSmartHome <- grepl(c("Apple TV|Google Home|Smart Light Bulb|Fire TV Stick|Roku Express"),transC@itemInfo$labels)
indicesMouse <- grepl(c("3-Button Mouse|Logitech Wireless Mouse|Logitech 3-button Mouse|Redragon Gaming Mouse|HP Wireless Mouse|Generic Black 3-Button|Slim Wireless Mouse|Microsoft Basic Optical Mouse|Wireless Portable Mouse|Gaming Mouse Professional"),transC@itemInfo$labels)
indicesKeyboard <- grepl(c("HP USB Keyboard|Logitech Wireless Keyboard|Rii LED Keyboard|Logitech Keyboard|Backlit LED Gaming Keyboard|Dell Wired Keyboard|Apple Wired Keyboard|Apple Wireless Keyboard|Apple Magic Keyboard"),transC@itemInfo$labels)
indicesCombo <- grepl(c("Logitech MK550 Wireless Wave Keyboard and Mouse Combo|Logitech Desktop MK120 Mouse and keyboard Combo|Logitech MK270 Wireless Keyboard and Mouse Combo|Dell KM117 Wireless Keyboard & Mouse|EagleTec Wireless Combo Keyboard and Mouse|Microsoft Wireless Comfort Keyboard and Mouse|Microsoft Wireless Desktop Keyboard and Mouse|Rii LED Gaming Keyboard & Mouse Combo|Logitech MK360 Wireless Keyboard and Mouse Combo"),transC@itemInfo$labels)
indicesComputerHeadPhones <- grepl(c("Zombie Gaming Headset|Logitech ClearChat Headset|Panasonic On-Ear Stereo Headphones RP-HT21|PC Gaming Headset|Kensington Headphones|Logitech Stereo Headset|Koss Home Headphones|Microsoft Headset|Ailihen Stereo Headphones|XIBERIA Gaming Headset"),transC@itemInfo$labels)
Names<-transC@itemInfo$labels

### Levels ###


#CATEGORIZE THE LABELS
transC@itemInfo$category <- replace(transC@itemInfo$category,indicesLaptop,"Laptop")
transC@itemInfo$category <- replace(transC@itemInfo$category,indicesDesktop,"Desktop")
transC@itemInfo$category <- replace(transC@itemInfo$category,indicesHardDRives,"Acessories")
transC@itemInfo$category <- replace(transC@itemInfo$category,indicesMonitor,"Monitor")
transC@itemInfo$category <- replace(transC@itemInfo$category,indicesSpeakers,"Acessories")
transC@itemInfo$category <- replace(transC@itemInfo$category,indicesTablets,"Tablets")
transC@itemInfo$category <- replace(transC@itemInfo$category,indicesCables,"Acessories")
transC@itemInfo$category <- replace(transC@itemInfo$category,indicesActiveHeadPhones,"Acessories")
transC@itemInfo$category <- replace(transC@itemInfo$category,indicesAcessories,"Acessories")
transC@itemInfo$category <- replace(transC@itemInfo$category,indicesPrinters,"Printers")
transC@itemInfo$category <- replace(transC@itemInfo$category,indicesPrintersINk,"Printers Ink")
transC@itemInfo$category <- replace(transC@itemInfo$category,indicesCStands,"Acessories")
transC@itemInfo$category <- replace(transC@itemInfo$category,indicesSmartHome,"Acessories")
transC@itemInfo$category <- replace(transC@itemInfo$category,indicesMouse,"Acessories")
transC@itemInfo$category <- replace(transC@itemInfo$category,indicesKeyboard,"Acessories")
transC@itemInfo$category <- replace(transC@itemInfo$category,indicesCombo,"Acessories")
transC@itemInfo$category <- replace(transC@itemInfo$category,indicesComputerHeadPhones,"Acessories")


#AGGREGATE BY CATEGORY
B2c_cat<-aggregate(transC, by="category")

#####2.APPLY RULES#####
RulesC<- apriori (transC, parameter = list(supp = 0.0009, conf = 0.30,  minlen=2))
RulesCat<- apriori (B2c_cat, parameter = list(supp = 0.0002, conf = 0.40,  minlen=2))



####3. REDUNDANT #####
#show the redundant rules
which(is.redundant(RulesC), arr.ind = TRUE, useNames = TRUE)
#DELETE THE REDUNDANT
RulesC_RED <- RulesC[!is.redundant(RulesC)]


####4.PLOTTING#####

ruleExplorer(RulesC)
ruleExplorer(RulesCat)


