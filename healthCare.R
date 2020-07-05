getwd()

library(dplyr)
library(ggplot2)
library(corrplot)


data <- read.xls("hospitalcosts.xlsx")

#1. Analysis

aggregate(data$TOTCHG, list(data$AGE), sum ) -> maxChgAge
jpeg("Analysis1.jpeg")
ggplot(data=maxChgAge, aes(x=maxChgAge$Group.1, y=maxChgAge$x)) + geom_bar(stat = "identity") +xlab("Age Category") +ylab("Total Charge")+ggtitle(" AGE vs Maximum Expenditure ")
dev.off()
print(paste("The age category of people who frequent the hospsital and has the maximum expenditure - ",max(maxChgAge$x), sep = ""))


#2 Analysis
aggregate(data[, c("LOS", "TOTCHG")], list(data$APRDRG), sum)->modifieddata1
modifieddata1$Group.1[modifieddata1$LOS == max(modifieddata1$LOS, na.rm = T)] -> maxLOSDiag
modifieddata1$Group.1[modifieddata1$TOTCHG == max(modifieddata1$TOTCHG, na.rm = T)] -> maxChgDiag
jpeg("Analysis2.jpeg")
ggplot(modifieddata1, aes(x=modifieddata1$Group.1, y=modifieddata1$LOS, colour=modifieddata1$TOTCHG)) + geom_point() +xlab("Diagnosis Group") +ylab("LOS")+ggtitle(" DiagnosisGrp VS AGE & Max Expenditure ")
dev.off()

print(paste("The agency wants to find the diagnosis-related group that has maximum hospitalization is ",maxLOSDiag," Group and has the maximum expenditure - ",maxChgDiag,"  Group ", sep = ""))

#3 Analysis
summary(aov(TOTCHG ~ RACE, data=data) )
summary(lm(TOTCHG~RACE, data = data))
jpeg("Analysis3.jpeg")
ggplot(data,aes(x = data$TOTCHG, 
     y = data$RACE))+ geom_point()+ xlab ("TOTOAL COST")+ ylab ("RACE") +ggtitle("Race Vs TotalCharge")
dev.off()
print("With Linear regression - RACE has accuracy of 38% which is too small and from anova - 'Race' has Probaility of 68.6%  which is not leading to a significant variance in the hospital costs")

#4 Analysis
summary(aov(TOTCHG ~ AGE + FEMALE, data = data))
summary(lm(TOTCHG ~ AGE + FEMALE, data))
jpeg("Analysis4.jpeg")
ggplot(data, aes(data$AGE, data$TOTCHG, colour=data$FEMALE)) + geom_point() +xlab("Age") +ylab("Total Charge") +ggtitle("Total Charge vs Age vs Female")
dev.off()

print("'AGE' and 'FEMALE' of the patient are leading to a significant variance in the hospital costs")

#5 Analysis
summary(lm(LOS ~ AGE + FEMALE + RACE, data = data))
jpeg("Analysis5.jpeg")
corrplot(cor(data[,c("LOS","AGE","FEMALE","RACE")]))
dev.off()

print("R Squared value is very small as 0.78%. So, these can't help in prediction")

#6 Complete analysis 
summary(aov(TOTCHG ~ ., data = data))
jpeg("Analysis6.jpeg")
corrplot(cor(data))
dev.off()

print("Variables LOS & APRDRG are mainly affects hospital costs ")

