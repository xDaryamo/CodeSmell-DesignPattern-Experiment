library(readxl)
library(ggplot2)
library(dplyr)
library(tidyverse)

#percorso assoluto root del progetto
path <- "C:\\Users\\Xzeni\\OneDrive\\Desktop\\CodeSmell-DesignPattern-Experiment-master\\"

#caricamento del dataset
df <- read_excel(paste0(path,"risultati.xlsx"), sheet = "dataset")

gruppi <- c("A", "B", "C", "D")

variabili_dipendenti <- c("Rapporto_Comprensione", "Rapporto_Manutenzione", "Effort_Medio_Comprensione", "Effort_Medio_Manutenzione")

# Statistiche descrittive
by(df$Rapporto_Comprensione, df$Gruppo, summary)
by(df$Rapporto_Manutenzione, df$Gruppo, summary)
by(df$Effort_Medio_Comprensione, df$Gruppo, summary)
by(df$Effort_Medio_Manutenzione, df$Gruppo, summary)

by(df$Rapporto_Comprensione, df$Gruppo, sd)
by(df$Rapporto_Manutenzione, df$Gruppo, sd)
by(df$Effort_Medio_Comprensione, df$Gruppo, sd)
by(df$Effort_Medio_Manutenzione, df$Gruppo, sd)

#Distribuzione dei dati

# Boxplot per le risposte corrette nel task di manutenzione
boxplot(Rapporto_Manutenzione ~ Gruppo, data = df, xlab = "Gruppo", ylab = "Percentuale risposte corrette - Manutenzione")

# Boxplot per le risposte corrette nel task di comprensione
boxplot(Rapporto_Comprensione ~ Gruppo, data = df, xlab = "Gruppo", ylab = "Percentuale risposte corrette - Comprensione")

# Boxplot per l'effort medio nel task di manutenzione
boxplot(Effort_Medio_Manutenzione ~ Gruppo, data = df, xlab = "Gruppo", ylab = "Effort medio (minuti) - Manutenzione")

# Boxplot per l'effort medio nel task di comprensione
boxplot(Effort_Medio_Comprensione ~ Gruppo, data = df, xlab = "Gruppo", ylab = "Effort medio (minuti) - Comprensione")


#Precision

# Per ogni gruppo
for (gruppo in gruppi) {
  cat("Gruppo:", gruppo, "\n")
  

  dati_gruppo <- df %>% 
    filter(Gruppo == gruppo)
  
  #Per ogni variabile dipendente
  for (variabile in variabili_dipendenti) {
    cat("Variabile:", variabile, "\n")
    
    #Test di Shapiro-Wilk per la variabile dipendente corrente
    risultato_test <- shapiro.test(dati_gruppo[[variabile]])
    
    # Stampa il risultato del test per la variabile dipendente corrente
    cat("Test statistic:", risultato_test$statistic, "\n")
    cat("P-value:", risultato_test$p.value, "\n")
    cat("-------------------------------------------\n")
  }
  cat("\n")
}

#Test e significatività statistica


# Esegui il test di Kruskal-Wallis per ogni variabile dipendente
for (var in unique(variabili_dipendenti)) {
  test_result <- kruskal.test(df[[var]] ~ Gruppo, data = df)
  
  # Stampa i risultati del test
  cat("Variabile:", var, "\n")
  cat("Test statistic:", test_result$statistic, "\n")
  cat("P-value:", test_result$p.value, "\n")
  cat("-------------------------------------------\n\n")
}

variabili <- c("Rapporto_Comprensione", "Rapporto_Manutenzione", "Effort_Medio_Comprensione")  # Escludi Effort_Medio_Manutenzione")

library(FSA)

for (var in variabili) {
  
  # Confronto a coppie tra i gruppi
 
  dunn_result <-  dunnTest(df[[var]], g =  df$Gruppo,
                               method="bonferroni")
  
  # Visualizzazione dei risultati dei confronti a coppie
  print(paste("Variabile:", var))
  print(dunn_result)
  print("-------------------------------------------")
}