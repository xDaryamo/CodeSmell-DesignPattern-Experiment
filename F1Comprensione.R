library(xlsx)

dataA <- read.xlsx("D:\\Test\\Results.xlsx", sheetIndex = 3, rowIndex = 1:7)
dataB <- read.xlsx("D:\\Test\\Results.xlsx", sheetIndex = 3, rowIndex = 8:13)
dataC <- read.xlsx("D:\\Test\\Results.xlsx", sheetIndex = 3, rowIndex = 14:18)
dataD <- read.xlsx("D:\\Test\\Results.xlsx", sheetIndex = 3, rowIndex = 19:32)

xVector <- c("A", "B", "C", "D")

sa1 <- dataA$Corrette.Comprensione[1] / (dataA$Corrette.Comprensione[1] + dataA$Errate..Comprensione[1])
sa2 <- dataA$Corrette.Comprensione[2] / (dataA$Corrette.Comprensione[2] + dataA$Errate..Comprensione[2])
sa3 <- dataA$Corrette.Comprensione[3] / (dataA$Corrette.Comprensione[3] + dataA$Errate..Comprensione[3])
sa4 <- dataA$Corrette.Comprensione[4] / (dataA$Corrette.Comprensione[4] + dataA$Errate..Comprensione[4])
sa5 <- dataA$Corrette.Comprensione[5] / (dataA$Corrette.Comprensione[5] + dataA$Errate..Comprensione[5])
sa6 <- dataA$Corrette.Comprensione[6] / (dataA$Corrette.Comprensione[6] + dataA$Errate..Comprensione[6])

sav <- c(sa1, sa2, sa3, sa4, sa5, sa6)

dfA <- data.frame(xVector[1], sav)

sb1 <- dataB$Corrette.Comprensione[1] / (dataB$Corrette.Comprensione[1] + dataB$Errate..Comprensione[1])
sb2 <- dataB$Corrette.Comprensione[2] / (dataB$Corrette.Comprensione[2] + dataB$Errate..Comprensione[2])
sb3 <- dataB$Corrette.Comprensione[3] / (dataB$Corrette.Comprensione[3] + dataB$Errate..Comprensione[3])
sb4 <- dataB$Corrette.Comprensione[4] / (dataB$Corrette.Comprensione[4] + dataB$Errate..Comprensione[4])
sb5 <- dataB$Corrette.Comprensione[5] / (dataB$Corrette.Comprensione[5] + dataB$Errate..Comprensione[5])

sbv <- c(sb1, sb2, sb3, sb4, sb5)

dfB <- data.frame(xVector[2], sbv)

sc1 <- dataC$Corrette.Comprensione[1] / (dataC$Corrette.Comprensione[1] + dataC$Errate..Comprensione[1])
sc2 <- dataC$Corrette.Comprensione[2] / (dataC$Corrette.Comprensione[2] + dataC$Errate..Comprensione[2])
sc3 <- dataC$Corrette.Comprensione[3] / (dataC$Corrette.Comprensione[3] + dataC$Errate..Comprensione[3])
sc4 <- dataC$Corrette.Comprensione[4] / (dataC$Corrette.Comprensione[4] + dataC$Errate..Comprensione[4])

scv <- c(sc1, sc2, sc3, sc4)

dfC <- data.frame(xVector[3], scv)

sd1 <- dataD$Corrette.Comprensione[1] / (dataD$Corrette.Comprensione[1] + dataD$Errate..Comprensione[1])
sd2 <- dataD$Corrette.Comprensione[2] / (dataD$Corrette.Comprensione[2] + dataD$Errate..Comprensione[2])
sd3 <- dataD$Corrette.Comprensione[3] / (dataD$Corrette.Comprensione[3] + dataD$Errate..Comprensione[3])
sd4 <- dataD$Corrette.Comprensione[4] / (dataD$Corrette.Comprensione[4] + dataD$Errate..Comprensione[4])
sd5 <- dataD$Corrette.Comprensione[5] / (dataD$Corrette.Comprensione[5] + dataD$Errate..Comprensione[5])

sdv <- c(sd1, sd2, sd3, sd4, sd5)

dfD <- data.frame(xVector[4], sdv)

sv <- c(sav, sbv, scv, sdv)
F1 <- 2 * (sv*sv) / (sv + sv)

AF <- data.frame(xVector[1], F1[1:6])
BF <- data.frame(xVector[2], F1[7:11])
CF <- data.frame(xVector[3], F1[12:15])
DF <- data.frame(xVector[4], F1[16:20])

library(ggplot2)
ggplot(AF, aes(x = xVector[1], y = F1.1.6.)) +
  geom_boxplot() +
  geom_boxplot(data = BF, aes(x = xVector[2], y = F1.7.11.))+
  geom_boxplot(data = CF, aes(x = xVector[3], y = F1.12.15.))+
  geom_boxplot(data = DF, aes(x = xVector[4], y = F1.16.20.))+
  labs(x = "Group Task Comprensione", y = "F1 - score")