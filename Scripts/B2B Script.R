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

#Data <- read_csv("C:/Users/Sergi Ch/Downloads/UBIQUM/PROJECT 2 - R/RM2T4/Datasets/ElectronidexTransactions2017.csv", col_names = FALSE)
B2b <- read_csv("C:/Users/Sergi Ch/Downloads/UBIQUM/PROJECT 2 - R/RM2T4/Datasets/B2bREAL.txt")
B2b[c(1,127)]<-NULL

transB<-as(B2b, "transactions")


#####1.CATEGORIZE LEVEL#####
indicesLaptop <- grepl(c("Laptop|Aspire|Chromebook|MacBook"),transB@itemInfo$labels)
indicesDesktop <- grepl(c("Desktop|iMac"),transB@itemInfo$labels)
indicesTablets <- grepl(c("iPad|Tablet|Tab|Kindle"),transB@itemInfo$labels)
indicesMonitor <- grepl(c("Monitor"),transB@itemInfo$labels)
indicesMiceAndKeyboard <-grepl(c("Mouse|Keyboard"),transB@itemInfo$labels)
indicesHardDRives <- grepl(c("Hard Drive"),transB@itemInfo$labels)
indicesSpeakers <- grepl(c("Speakers|Speaker|Sonos|Cyber Acoustics|DOSS"),transB@itemInfo$labels)
indicesCables <- grepl(c("Cable|HDMI Adapter"),transB@itemInfo$labels)
indicesCombo <- grepl(c("Combo"),transB@itemInfo$labels)
indicesActiveHeadPhones <- grepl(c("Apple Earpods|Monster Beats|Wireless Sports|In-Ear|APIE|Panasonic On-Ear Stereo Headphones RP-HT21|Philips Flexible Earhook Headphone|Panasonic On-Ear Stereo Headphones"),transB@itemInfo$labels)
indicesAcessories <- grepl(c("Microsoft Office Home and Student 2016|Computer Game|Belkin Mouse Pad|Large Mouse Pad"),transB@itemInfo$labels)
indicesPrinters <- grepl(c("Epson Printer|HP Wireless Printer|Canon Office Printer|Brother Printer|DYMO Label Manker"),transB@itemInfo$labels)
indicesPrintersINk <- grepl(c("Epson Black Ink|HP Black & Tri-color Ink|Canon Ink|Brother Printer Toner|DYMO Labeling Tape"),transB@itemInfo$labels)
indicesCStands <- grepl(c("Halter Acrylic Monitor Stand|Height-Adjustable Standing Desk|Multi Media Stand|Full Motion Monitor Mount"),transB@itemInfo$labels)
indicesSmartHome <- grepl(c("Apple TV|Google Home|Smart Light Bulb|Fire TV Stick|Roku Express"),transB@itemInfo$labels)
indicesMouse <- grepl(c("3-Button Mouse|Logitech Wireless Mouse|Logitech 3-button Mouse|Redragon Gaming Mouse|HP Wireless Mouse|Generic Black 3-Button|Slim Wireless Mouse|Microsoft Basic Optical Mouse|Wireless Portable Mouse|Gaming Mouse Professional"),transB@itemInfo$labels)
indicesKeyboard <- grepl(c("HP USB Keyboard|Logitech Wireless Keyboard|Rii LED Keyboard|Logitech Keyboard|Backlit LED Gaming Keyboard|Dell Wired Keyboard|Apple Wired Keyboard|Apple Wireless Keyboard|Apple Magic Keyboard"),transB@itemInfo$labels)
indicesCombo <- grepl(c("Logitech MK550 Wireless Wave Keyboard and Mouse Combo|Logitech Desktop MK120 Mouse and keyboard Combo|Logitech MK270 Wireless Keyboard and Mouse Combo|Dell KM117 Wireless Keyboard & Mouse|EagleTec Wireless Combo Keyboard and Mouse|Microsoft Wireless Comfort Keyboard and Mouse|Microsoft Wireless Desktop Keyboard and Mouse|Rii LED Gaming Keyboard & Mouse Combo|Logitech MK360 Wireless Keyboard and Mouse Combo"),transB@itemInfo$labels)
indicesComputerHeadPhones <- grepl(c("Zombie Gaming Headset|Logitech ClearChat Headset|Panasonic On-Ear Stereo Headphones RP-HT21|PC Gaming Headset|Kensington Headphones|Logitech Stereo Headset|Koss Home Headphones|Microsoft Headset|Ailihen Stereo Headphones|XIBERIA Gaming Headset"),transB@itemInfo$labels)
Names<-transB@itemInfo$labels
transB@itemInfo$labels

#CATEGORIZE THE LABELS
transB@itemInfo$category <- replace(transB@itemInfo$category,indicesLaptop,"Laptop")
transB@itemInfo$category <- replace(transB@itemInfo$category,indicesDesktop,"Desktop")
transB@itemInfo$category <- replace(transB@itemInfo$category,indicesHardDRives,"Acessories")
transB@itemInfo$category <- replace(transB@itemInfo$category,indicesMonitor,"Monitor")
transB@itemInfo$category <- replace(transB@itemInfo$category,indicesSpeakers,"Acessories")
transB@itemInfo$category <- replace(transB@itemInfo$category,indicesTablets,"Tablets")
transB@itemInfo$category <- replace(transB@itemInfo$category,indicesCables,"Acessories")
transB@itemInfo$category <- replace(transB@itemInfo$category,indicesActiveHeadPhones,"Acessories")
transB@itemInfo$category <- replace(transB@itemInfo$category,indicesAcessories,"Acessories")
transB@itemInfo$category <- replace(transB@itemInfo$category,indicesPrinters,"Printers")
transB@itemInfo$category <- replace(transB@itemInfo$category,indicesPrintersINk,"Printers Ink")
transB@itemInfo$category <- replace(transB@itemInfo$category,indicesCStands,"Acessories")
transB@itemInfo$category <- replace(transB@itemInfo$category,indicesSmartHome,"Acessories")
transB@itemInfo$category <- replace(transB@itemInfo$category,indicesMouse,"Acessories")
transB@itemInfo$category <- replace(transB@itemInfo$category,indicesKeyboard,"Acessories")
transB@itemInfo$category <- replace(transB@itemInfo$category,indicesCombo,"Acessories")
transB@itemInfo$category <- replace(transB@itemInfo$category,indicesComputerHeadPhones,"Acessories")

#####2.APPLY RULES####

#AGGREGATE BY CATEGORY
B2b_cat<-aggregate(transB, by="category")
RulesBCat<- apriori (B2b_cat, parameter = list(supp = 0.008, conf = 0.2,  minlen=2))

RulesB<- apriori (transB, parameter = list(supp = 0.004, conf = 0.2,  minlen=2))

####3. REDUNDANT #####
#show the redundant rules
is.redundant(RulesB)

#DELETE THE REDUNDANT
RulesB_RED <- RulesBCat[!is.redundant(RulesBCat)]


####3.PLOTTING####

ruleExplorer(RulesB)
ruleExplorer(RulesB_RED)
ruleExplorer(RulesBCat)
