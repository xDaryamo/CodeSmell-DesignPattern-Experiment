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
summary(df$Rapporto_Manutenzione)
summary(df$Rapporto_Comprensione)
summary(df$Effort_Medio_Comprensione)
summary(df$Effort_Medio_Manutenzione)

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


#Solo Effort_Medio_Comprensione ci permette di rigettare l'ipotesi nulla, indaghiamo ulteriormente
altre_variabili <- c("Rapporto_Comprensione", "Rapporto_Manutenzione", "Effort_Medio_Manutenzione")  # Escludi la variabile "EffortComprensione")

# Ciclo per confrontare "Effort_Medio_Comprensione" con tutte le altre variabili

for (var in altre_variabili) {
  result <- combn(unique(df$Gruppo), 2, function(groups) {
    test_result <- wilcox.test(
      subset(df, Gruppo == groups[1])[[var]],
      subset(df, Gruppo == groups[2])[[var]]
    )
    c(Group1 = groups[1], Group2 = groups[2], p_value = test_result$p.value)
  }, simplify = FALSE)
  
  print(paste("Variabile:", var))
  print(result)
  print("-------------------------------------------")
}