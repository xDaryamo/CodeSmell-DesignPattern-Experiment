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

dfc1 <- read.xlsx(paste(path,"Results.xlsx", sep = ""), sheetIndex = 3, colIndex = 1)
dfc2 <- read.xlsx(paste(path,"Results.xlsx", sep = ""), sheetIndex = 3, colIndex = 3)
dfTest <- data.frame(dfc1,dfc2)



for (gruppo in xVector) {
  dati_gruppo <- dfTest$Corrette.Comprensione[dfTest$Gruppo == gruppo]  # Seleziona i dati per il gruppo corrente
  shapiro_test <- shapiro.test(dati_gruppo)  # Esegui il test di Shapiro-Wilk
  print(paste("Gruppo", gruppo, ": p-value =", shapiro_test$p.value))  # Stampa il p-value
}


dfTest$log_trasformati <- log(dfTest$Corrette.Comprensione)

for (gruppo in xVector) {
  dati_gruppo <- dfTest$log_trasformati[dfTest$Gruppo == gruppo]  # Seleziona i dati per il gruppo corrente
  shapiro_test <- shapiro.test(dati_gruppo)  # Esegui il test di Shapiro-Wilk
  print(paste("Gruppo", gruppo, ": p-value =", shapiro_test$p.value))  # Stampa il p-value
}

dfTest$radice_quadrata <- sqrt(dfTest$Corrette.Comprensione)

for (gruppo in xVector) {
  dati_gruppo <- dfTest$radice_quadrata[dfTest$Gruppo == gruppo]  # Seleziona i dati per il gruppo corrente
  shapiro_test <- shapiro.test(dati_gruppo)  # Esegui il test di Shapiro-Wilk
  print(paste("Gruppo", gruppo, ": p-value =", shapiro_test$p.value))  # Stampa il p-value
}

dfTest$esponenziale <- exp(dfTest$Corrette.Comprensione)

for (gruppo in xVector) {
  dati_gruppo <- dfTest$esponenziale[dfTest$Gruppo == gruppo]  # Seleziona i dati per il gruppo corrente
  shapiro_test <- shapiro.test(dati_gruppo)  # Esegui il test di Shapiro-Wilk
  print(paste("Gruppo", gruppo, ": p-value =", shapiro_test$p.value))  # Stampa il p-value
}

dfTest$inversa <- 1 / dfTest$Corrette.Comprensione

for (gruppo in xVector) {
  dati_gruppo <- dfTest$inversa[dfTest$Gruppo == gruppo]  # Seleziona i dati per il gruppo corrente
  shapiro_test <- shapiro.test(dati_gruppo)  # Esegui il test di Shapiro-Wilk
  print(paste("Gruppo", gruppo, ": p-value =", shapiro_test$p.value))  # Stampa il p-value
}
kruskal.test(Corrette.Comprensione ~ Gruppo, data = dfTest)
                 
anova <- aov(Corrette.Comprensione ~ Gruppo, data = dfTest)
summary(anova)[[1]][["Pr(>F)"]][1]

