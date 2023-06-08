library(xlsx)

dataA <- read.xlsx("D:\\Test\\Results.xlsx", sheetIndex = 3, rowIndex = 1:7)
dataB <- read.xlsx("D:\\Test\\Results.xlsx", sheetIndex = 3, rowIndex = 8:13)
dataC <- read.xlsx("D:\\Test\\Results.xlsx", sheetIndex = 3, rowIndex = 14:18)
dataD <- read.xlsx("D:\\Test\\Results.xlsx", sheetIndex = 3, rowIndex = 19:32)

xVector <- c("A", "B", "C", "D")

sa1 <- dataA$Corrette.Manutenzione[1] / (dataA$Corrette.Manutenzione[1] + dataA$Errate.Manutenzione[1])
sa2 <- dataA$Corrette.Manutenzione[2] / (dataA$Corrette.Manutenzione[2] + dataA$Errate.Manutenzione[2])
sa3 <- dataA$Corrette.Manutenzione[3] / (dataA$Corrette.Manutenzione[3] + dataA$Errate.Manutenzione[3])
sa4 <- dataA$Corrette.Manutenzione[4] / (dataA$Corrette.Manutenzione[4] + dataA$Errate.Manutenzione[4])
sa5 <- dataA$Corrette.Manutenzione[5] / (dataA$Corrette.Manutenzione[5] + dataA$Errate.Manutenzione[5])
sa6 <- dataA$Corrette.Manutenzione[6] / (dataA$Corrette.Manutenzione[6] + dataA$Errate.Manutenzione[6])

sav <- c(sa1, sa2, sa3, sa4, sa5, sa6)

dfA <- data.frame(xVector[1], sav)

sb1 <- dataB$Corrette.Manutenzione[1] / (dataB$Corrette.Manutenzione[1] + dataB$Errate.Manutenzione[1])
sb2 <- dataB$Corrette.Manutenzione[2] / (dataB$Corrette.Manutenzione[2] + dataB$Errate.Manutenzione[2])
sb3 <- dataB$Corrette.Manutenzione[3] / (dataB$Corrette.Manutenzione[3] + dataB$Errate.Manutenzione[3])
sb4 <- dataB$Corrette.Manutenzione[4] / (dataB$Corrette.Manutenzione[4] + dataB$Errate.Manutenzione[4])
sb5 <- dataB$Corrette.Manutenzione[5] / (dataB$Corrette.Manutenzione[5] + dataB$Errate.Manutenzione[5])

sbv <- c(sb1, sb2, sb3, sb4, sb5)

dfB <- data.frame(xVector[2], sbv)

sc1 <- dataC$Corrette.Manutenzione[1] / (dataC$Corrette.Manutenzione[1] + dataC$Errate.Manutenzione[1])
sc2 <- dataC$Corrette.Manutenzione[2] / (dataC$Corrette.Manutenzione[2] + dataC$Errate.Manutenzione[2])
sc3 <- dataC$Corrette.Manutenzione[3] / (dataC$Corrette.Manutenzione[3] + dataC$Errate.Manutenzione[3])
sc4 <- dataC$Corrette.Manutenzione[4] / (dataC$Corrette.Manutenzione[4] + dataC$Errate.Manutenzione[4])

scv <- c(sc1, sc2, sc3, sc4)

dfC <- data.frame(xVector[3], scv)

sd1 <- dataD$Corrette.Manutenzione[1] / (dataD$Corrette.Manutenzione[1] + dataD$Errate.Manutenzione[1])
sd2 <- dataD$Corrette.Manutenzione[2] / (dataD$Corrette.Manutenzione[2] + dataD$Errate.Manutenzione[2])
sd3 <- dataD$Corrette.Manutenzione[3] / (dataD$Corrette.Manutenzione[3] + dataD$Errate.Manutenzione[3])
sd4 <- dataD$Corrette.Manutenzione[4] / (dataD$Corrette.Manutenzione[4] + dataD$Errate.Manutenzione[4])
sd5 <- dataD$Corrette.Manutenzione[5] / (dataD$Corrette.Manutenzione[5] + dataD$Errate.Manutenzione[5])

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
  labs(x = "Group Task Manutenzione", y = "F1 - score")