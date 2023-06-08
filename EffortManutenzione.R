library(xlsx)

dataA <- read.xlsx("D:\\Test\\Results.xlsx", sheetIndex = 5, rowIndex = 1:9)
dataB <- read.xlsx("D:\\Test\\Results.xlsx", sheetIndex = 5, rowIndex = 10:18)
dataC <- read.xlsx("D:\\Test\\Results.xlsx", sheetIndex = 5, rowIndex = 19:26)
dataD <- read.xlsx("D:\\Test\\Results.xlsx", sheetIndex = 5, rowIndex = 27:35)

xVector <- c("A", "B", "C", "D")

sa1 <- c(dataA$S1)
sa2 <- c(dataA$S2)
sa3 <- c(dataA$S3)
sa4 <- c(dataA$S4)
sa5 <- c(dataA$S5)
sa6 <- c(dataA$S6)

sav <- c(sa1, sa2, sa3, sa4, sa5, sa6)

summary(sav)

dfA <- data.frame(xVector[1], sav)

sb1 <- c(dataB$S1)
sb2 <- c(dataB$S2)
sb3 <- c(dataB$S3)
sb4 <- c(dataB$S4)
sb5 <- c(dataB$S5)
sb6 <- c(dataB$S6)

sbv <- c(sb1, sb2, sb3, sb4, sb5, sb6)

summary(sbv)

dfB <- data.frame(xVector[2], sbv)

sc1 <- c(dataC$S1)
sc2 <- c(dataC$S2)
sc3 <- c(dataC$S3)
sc4 <- c(dataC$S4)
sc5 <- c(dataC$S5)
sc6 <- c(dataC$S6)

scv <- c(sc1, sc2, sc3, sc4, sc5, sc6)

summary(scv)

dfC <- data.frame(xVector[3], scv)

sd1 <- c(dataD$S1)
sd2 <- c(dataD$S2)
sd3 <- c(dataD$S3)
sd4 <- c(dataD$S4)
sd5 <- c(dataD$S5)
sd6 <- c(dataD$S6)

sdv <- c(sd1, sd2, sd3, sd4, sd5, sd6)

summary(sdv)

dfD <- data.frame(xVector[4], sdv)

library(ggplot2)
ggplot(dfA, aes(x = xVector[1], y = sav)) +
  geom_boxplot() +
  geom_boxplot(data = dfB, aes(x = xVector[2], y = sbv))+
  geom_boxplot(data = dfC, aes(x = xVector[3], y = scv))+
  geom_boxplot(data = dfD, aes(x = xVector[4], y = sdv))+
  labs(x = "Group Task Manutenzione", y = "Effort(min)")