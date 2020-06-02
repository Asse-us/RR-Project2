
# Impact Analysis of Sever Storm Events on Public Health and Economy in the USA
install.packages("cowplot")

#load the required packages for th assessment 
library(ggplot2)
library(dplyr)
library(R.utils)
require(gridExtra)
library(cowplot)
# Loading and preprocessing the data
filePath<- getwd()
fileName<- "stormData.csv.bz2"
fileUrl<- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
download.file(fileUrl, destfile = fileName, method = "curl")
bunzip2(fileName)
list.files(filePath)

Storm_Data <- read.csv("stormData.csv")

names(Storm_Data)
str(Storm_Data)
summary(Storm_Data)
head(Storm_Data)
unique(Storm_Data$EVTYPE)
dim(Storm_Data)

formatedDate<- as.numeric(format(as.Date(Storm_Data$BGN_DATE, format = "%m/%d/%Y %H:%M:%S"), "%Y"))
png(plot1.png)
hist(formatedDate, xlab = "Year",col = "lightblue", breaks = 60,
     main = "Frequency of Storm Events from 1950 to 2011")
dev.off()
# The above histogram shows that there is an increase in the measurement of storm events 
# increasing gradually till around 1990 and more frequent measurements are taken then afterward.

#extract interested variables for the analysis
# state variable: STATE
# Target Events variable: EVTYPE
# Health variables: FATALITIES and INJURIES
# Economic variables: PROPDMG, PROPDMGEXP, CROPDMG and CROPDMGEXP

SelectedData <- c("STATE", "EVTYPE", "FATALITIES", "INJURIES", "PROPDMG", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP")
Sel_StormData <- Storm_Data[SelectedData]
dim(Sel_StormData)
head(Sel_StormData)
tail(Sel_StormData)
str(Sel_StormData)


#Total fatalities and top five fatalities
T_Fatality <- aggregate(FATALITIES ~ EVTYPE, data = Sel_StormData,  FUN="sum")
Top5Fatalities <- T_Fatality[order(-T_Fatality$FATALITIES) , ][1:5, ]
Top5Fatalities

#Total injuries and top five injuries
T_Injury <- aggregate(INJURIES ~ EVTYPE, data = Sel_StormData,  FUN="sum")
Top5Injuries <- T_Injury[order(-T_Injury$INJURIES) , ][1:5, ]
Top5Injuries

# Health Impact
# The graphical representation of the top five events with the highest total fatalities and injuries are shown 
# below:
png(HealthImpact.png)
Top5FatalityPlot<-ggplot(Top5Fatalities, aes(x = reorder(EVTYPE,-FATALITIES), y = FATALITIES)) + geom_bar(stat = "identity", fill = "red") +
    theme(axis.text.x = element_text(angle = 45,hjust = 1)) +
    xlab("Events Type") + ylab("Total Number of Fatalities") + 
    ggtitle("Top five Severe Weather Events causing Fatalities\n in USA")

Top5InjuryPlot<- ggplot(Top5Injuries, aes(x = reorder(EVTYPE,-INJURIES), y = INJURIES)) + geom_bar(stat = "identity", fill = "green") +
    theme(axis.text.x = element_text(angle = 45,hjust = 1)) +
    xlab("Events Type") + ylab("Total Number of Injuries")+
    ggtitle("Top five Severe Weather Events causing Injuries\n in USA")
plot_grid(Top5FatalityPlot,Top5InjuryPlot, ncol = 2)
dev.off()

# PROPDMG = Property damage
# CROPDMG = crop damage
# PROPDMGEXP = property damage exponent
# CROPDMGEXP = crop damage exponents
unique(Sel_StormData$PROPDMGEXP)
unique(Sel_StormData$CROPDMGEXP)

# cleaning and preparing the property and crop damages exponent variables 
# (PROPDMGEXP and `CROPDMGEXP)

# +, -, ? <- 0
# 0-8 -> 10
# H, h<- 100
# K,k -> 1,000
# M,m -> 1,000,000
# B,b -> 1,000,000,000

exp_multiplier<- function(exp) {
    ifelse(exp == '+', 1,                         
        ifelse(exp %in% paste(seq(0,8)), 10^1,      
            ifelse(exp %in% c('H', 'h'), 10^2,       
                ifelse(exp %in% c('K', 'k'), 10^3,      
                    ifelse(exp %in% c('M', 'm'), 10^6,    
                        ifelse(exp %in% c('B', 'b'), 10^9,
                               0)
                        )
                    )
                )
            )
    )
    }

Sel_StormData$PropDmgMult <- exp_multiplier(Sel_StormData$PROPDMGEXP)
Sel_StormData$CropDmgMult <- exp_multiplier(Sel_StormData$CROPDMGEXP)

# add two variables namely "TotalCropDMG" and TotalPropDMG and get their values by multiplying them 
# against the corresponding damage variables.
# and finall add the above two expenses to get the total economic danage expenses.
# The overall health imact is calculated by adding one variable T_HealthImpct with the 
# sum of FATALITIES AND INJURIES variables 
Sel_StormData$TotalCropDMG<- Sel_StormData$CROPDMG * Sel_StormData$CropDmgMult
Sel_StormData$TotalPropDMG<- Sel_StormData$PROPDMG * Sel_StormData$PropDmgMult
Sel_StormData$TotalDamage<- Sel_StormData$TotalCropDMG + Sel_StormData$TotalPropDMG
Sel_StormData$T_HealthImpct<- Sel_StormData$FATALITIES + Sel_StormData$INJURIES

# Crop damage
CropDamageByEvent <- aggregate(TotalCropDMG ~ EVTYPE, data = Sel_StormData,  FUN="sum")
Top5EventsCROPDMG <- CropDamageByEvent[order(-CropDamageByEvent$TotalCropDMG) , ][1:5, ]
Top5EventsCROPDMG

#Property Damage
ProDamageByEvent <- aggregate(TotalPropDMG ~ EVTYPE, data = Sel_StormData,  FUN="sum")
Top5EventsPropDMG <- ProDamageByEvent[order(-ProDamageByEvent$TotalPropDMG) , ][1:5, ]
Top5EventsPropDMG

#Total Damage
TotalDamageByEvent <- aggregate(TotalDamage ~ EVTYPE, data = Sel_StormData,  FUN="sum")
Top5EventsTotalDMG <- TotalDamageByEvent[order(-TotalDamageByEvent$TotalDamage) , ][1:5, ]
Top5EventsTotalDMG

#Health Impact
HealthDamageByEvent <- aggregate(T_HealthImpct ~ EVTYPE, data = Sel_StormData,  FUN="sum")
Top5EventsHealthImpact <- HealthDamageByEvent[order(-HealthDamageByEvent$T_HealthImpct) , ][1:5, ]
Top5EventsHealthImpact

# Ques 2. Across the United States, which types of events have the greatest economic
# consequences?
png(HealthEconomy.png)
p1<- ggplot(Top5EventsPropDMG, aes(x=reorder(EVTYPE, -TotalPropDMG), y=TotalPropDMG/10^9))+geom_bar(stat="identity", fill="blue")+theme(text = element_text(size = 8), axis.text.x = element_text(angle=35, hjust = 1))+labs(x="Events Type", y="Total Property Damage(Billion Dollars)")+ggtitle("Top five Events with Highest property damage") 
p2<- ggplot(Top5EventsCROPDMG, aes(x=reorder(EVTYPE, -TotalCropDMG), y=TotalCropDMG/10^9))+geom_bar(stat="identity", fill="green")+theme(text = element_text(size = 8), axis.text.x = element_text(angle=35, hjust = 1)) +labs(x="Events Type", y="Total Crop Damage(Billion Dollars)")+ggtitle("Top five Events with Highest Crop damage")
p3<- ggplot(Top5EventsTotalDMG, aes(x=reorder(EVTYPE, -TotalDamage), y=TotalDamage/10^9))+geom_bar(stat="identity", fill="red")+theme(text = element_text(size = 8), axis.text.x = element_text(angle=35, hjust = 1))+labs(x="Events Type", y="Total Economic Damage(Billion Dollars)")+ggtitle("Top five Events with Highest Economic Impact") 
p4<- ggplot(Top5EventsHealthImpact, aes(x=reorder(EVTYPE, -T_HealthImpct), y=T_HealthImpct))+geom_bar(stat="identity", fill="purple")+theme(text = element_text(size = 8), axis.text.x = element_text(angle=35, hjust = 1))+labs(x="Events Type", y="Total Health Impact")+ggtitle("Top five Events with Highest Health Impact\n(Fatalities & Injuries)") 
plot_grid(p1, p2, p3, p4, nrow =2, ncol = 2, align = 'vh')
dev.off()




