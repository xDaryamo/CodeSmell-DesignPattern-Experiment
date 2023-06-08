library(xlsx)
library(tidyverse)
path <- "G:\\Dazza\\Università\\I ANNO M\\CodeSmell-DesignPattern-Experiment\\"

dataA <- read.xlsx(paste(path,"Results.xlsx", sep = ""), sheetIndex = 2, rowIndex = 1:9)
dataB <- read.xlsx(paste(path,"Results.xlsx", sep = ""), sheetIndex = 2, rowIndex = 10:18)
dataC <- read.xlsx(paste(path,"Results.xlsx", sep = ""), sheetIndex = 2, rowIndex = 19:27)
dataD <- read.xlsx(paste(path,"Results.xlsx", sep = ""), sheetIndex = 2, rowIndex = 28:36)

xVector <- c("A", "B", "C", "D")

dfA <- data.frame(xVector[1], dataA$Corrette.Comprensione)
dfB <- data.frame(xVector[2], dataB$Corrette.Comprensione)
dfC <- data.frame(xVector[3], dataC$Corrette.Comprensione)
dfD <- data.frame(xVector[4], dataD$Corrette.Comprensione)

library(ggplot2)
ggplot(dfA, aes(x = xVector[1], y = dataA.Corrette.Comprensione)) +
geom_boxplot() +
geom_boxplot(data = dfB, aes(x = xVector[2], y = dataB.Corrette.Comprensione)) +
geom_boxplot(data = dfC, aes(x = xVector[3], y = dataC.Corrette.Comprensione)) +
geom_boxplot(data = dfD, aes(x = xVector[4], y = dataD.Corrette.Comprensione)) +
labs(title = "Comprensione", x = "Group", y = "Risposte Corrette")

dfA <- data.frame(xVector[1], dataA$Corrette.Manutenzione)
dfB <- data.frame(xVector[2], dataB$Corrette.Manutenzione)
dfC <- data.frame(xVector[3], dataC$Corrette.Manutenzione)
dfD <- data.frame(xVector[4], dataD$Corrette.Manutenzione)

library(ggplot2)
ggplot(dfA, aes(x = xVector[1], y = dataA.Corrette.Manutenzione)) +
  geom_boxplot() +
  geom_boxplot(data = dfB, aes(x = xVector[2], y = dataB.Corrette.Manutenzione)) +
  geom_boxplot(data = dfC, aes(x = xVector[3], y = dataC.Corrette.Manutenzione)) +
  geom_boxplot(data = dfD, aes(x = xVector[4], y = dataD.Corrette.Manutenzione)) +
  labs(title = "Manutenzione", x = "Group", y = "Risposte Corrette")

statsA <- summary(dfA)
statsB <- summary(dfB)
statsC <- summary(dfC)
statsD <- summary(dfD)

statsA
statsB 
statsC 
statsD

dfTest <- data.frame(read.xlsx(paste(path,"Results.xlsx", sep = ""), sheetIndex = 3, colIndex = 1,3))




