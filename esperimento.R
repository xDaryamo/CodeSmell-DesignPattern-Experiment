library(readxl)
library(ggplot2)
library(dplyr)
library(tidyverse)


#percorso assoluto root del progetto
path <- "G:\\Dazza\\Università\\I ANNO M\\CodeSmell-DesignPattern-Experiment\\"

#caricamento del dataset
df <- read_excel(paste0(path,"risultati.xlsx"), sheet = "dataset")


# Statistiche descrittive
summary(df$Corrette_Manutenzione)
summary(df$Corrette_Comprensione)
summary(df$Effort_Medio_Comprensione)
summary(df$Effort_Medio_Manutenzione)


# Boxplot per le risposte corrette nel task di manutenzione
boxplot(Corrette_Manutenzione ~ Gruppo, data = df, xlab = "Gruppo", ylab = "Numero di risposte corrette - Manutenzione")


# Boxplot per le risposte corrette nel task di comprensione
boxplot(Corrette_Comprensione ~ Gruppo, data = df, xlab = "Gruppo", ylab = "Numero di risposte corrette - Comprensione")

# Boxplot per l'effort medio nel task di manutenzione
boxplot(Effort_Medio_Manutenzione ~ Gruppo, data = df, xlab = "Gruppo", ylab = "Effort medio (minuti) - Manutenzione")

# Boxplot per l'effort medio nel task di comprensione
boxplot(Effort_Medio_Comprensione ~ Gruppo, data = df, xlab = "Gruppo", ylab = "Effort medio (minuti) - Comprensione")



gruppi <- c("A", "B", "C", "D")

variabili_dipendenti <- c("Corrette_Comprensione", "Corrette_Manutenzione", "Effort_Medio_Comprensione", "Effort_Medio_Manutenzione")

#Analisi normalità dei dati

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
altre_variabili <- c("Corrette_Comprensione", "Corrette_Manutenzione", "Effort_Medio_Manutenzione")  # Escludi la variabile "EffortComprensione")

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



anova <- aov(Effort_Medio_Comprensione ~ Gruppo, data = df)
summary(anova)
anova <- aov(Effort_Medio_Manutenzione ~ Gruppo, data = df)
summary(anova)
