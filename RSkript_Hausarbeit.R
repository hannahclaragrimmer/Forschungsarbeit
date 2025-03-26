## Herzlich Wilkommen zu meinem R-Skript, das ich für die Daten meiner Forschungsarbeit verwendet habe! 

# Datensatz einlesen
data <- read.csv("/Users/hannahgrimmer/Downloads/Hochladen-2/data_studis_clean.csv")

# 1. Unerwünschte Spalten entfernen
data <- data[, !names(data) %in% c("Alter_stud","Mean2", 
                                   "Item.A", "Item.B", "Item.C", "Item.D", "Item.E")]

data <- na.omit(data)
# Kontrollieren, ob noch NA-Werte vorhanden sind
sum(is.na(data))  # Sollte 0 ausgeben, wenn alle entfernt wurden

# 2. Daten aus Gruppe 4 und 5 entfernen
data <- data[!(data$Gruppe %in% c(4, 5)), ]

# 3. Variable "Gruppe" als kategoriale Variable (Faktor) umwandeln
data$Gruppe <- as.factor(data$Gruppe)

# Überprüfen, ob die Änderungen geklappt haben
str(data)
table(data$Gruppe)  # Zeigt die Häufigkeit der Gruppen 1-3

# Variable umbenennen
colnames(data)[colnames(data) == "Mean1"] <- "Kreativität"
colnames(data)[colnames(data) == "Gruppe"] <- "IQ"

# Überprüfen, ob die Änderung geklappt hat
str(data)  # Zeigt die neue Spaltenbezeichnung

# Bereinigten Datensatz speichern
write.csv(data, "data_studis_clean_final.csv", row.names = FALSE)

# Hiermit wolte ich überprüfen, dass der RIBS Kreativitätstest auch wirklich zu meinem Datensatz passt, der kleinste Wert von Kreativität darf nämlich nicht 20 unterschreiten.
min(data$Kreativität, na.rm = TRUE) # kleinster Wert ist 27, ich kann also RIBS verwenden
# Das gleiche nochmal, nur mit dem maximalen Wert, da der RIBS Punktescore nur bis 100 geht
max(data$Kreativität, na.rm = TRUE) # höchster Wert ist 69, ich kann also RIBS verwenden

# Sind die Werte meiner AV in jeder Gruppe annähernd normalverteilt? (Homogenität der Varianzen)
shapiro.test(data$Kreativität[data$IQ == 1])  # Test für Gruppe 1
shapiro.test(data$Kreativität[data$IQ == 2])  # Test für Gruppe 2
shapiro.test(data$Kreativität[data$IQ == 3])  # Test für Gruppe 3

# Ist Varianz der AV in allen Gruppen ähnlich groß? 
library(car)
leveneTest(Kreativität ~ IQ, data = data)

# ANOVA durchführen
anova_result <- aov(Kreativität ~ IQ, data = data)

# Ergebnisse anzeigen
summary(anova_result)

#Post-hoc-Test um herauszufinden, zwischen welchen Gruppen IQ es Unterschiede gibt
TukeyHSD(anova_result)

library(ggplot2)

# Boxplot für Kreativität nach IQ erstellen
ggplot(data, aes(x = as.factor(IQ), y = Kreativität, fill = as.factor(IQ))) +
  geom_boxplot(outlier.colour = "maroon", outlier.shape = 16, outlier.size = 2) +
  labs(title = "Vergleich der Kreativität nach IQ",
       x = "IQ-Gruppen",
       y = "Kreativität") +
  theme_minimal() +
  scale_fill_brewer(palette = "Pastel1") # Schöne Farben für die Gruppen

library(dplyr)

# Berechnung der Mittelwerte & Standardfehler pro IQ-Gruppe
summary_data <- data %>%
  group_by(IQ) %>%
  summarise(Mittelwert = mean(Kreativität, na.rm = TRUE),
            SE = sd(Kreativität, na.rm = TRUE) / sqrt(n()))

# Balkendiagramm mit Fehlerbalken erstellen
ggplot(summary_data, aes(x = as.factor(IQ), y = Mittelwert, fill = as.factor(IQ))) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  geom_errorbar(aes(ymin = Mittelwert - SE, ymax = Mittelwert + SE), 
                width = 0.2, color = "black") +
  labs(title = "Mittelwert der Kreativität nach IQ-Gruppen",
       x = "IQ-Gruppen",
       y = "Mittelwert Kreativität") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") # Schöne Farben

ggsave("kreativitaet_nach_IQ.png", width = 6, height = 4, dpi = 300)


## Mit diesen Codes habe ich die Tabelle für die Anova erstellt

install.packages("papaja")
install.packages("apaTables")

library(papaja)
library(apaTables)

# ANOVA durchführen
anova_result <- aov(Kreativität ~ IQ, data = data)

# ANOVA als APA-Tabelle exportieren
apa.aov.table(anova_result, filename = "ANOVA_APA.doc", table.number = 1)


##Mit diesen Codes habe ich die Tabelle zur Post-hoc-Analyse erstellt und erweitert
# Tukey-Test
posthoc <- TukeyHSD(anova_result)
posthoc_df <- as.data.frame(posthoc$IQ)  

# Spalten anpassen
colnames(posthoc_df) <- c("Mean Difference", "Lower CI", "Upper CI", "p-value")

# Word-Tabelle erstellen
ft_posthoc <- flextable(posthoc_df) %>%
  set_caption("Tukey Post-hoc Test Results") %>%
  autofit()

save_as_docx(ft_posthoc, path = "Tukey_APA.docx")

# Standardfehler (SE) für jede IQ-Gruppe in Bezug auf Kreativität berechnen
library(dplyr)

se_calc <- function(x) {
  sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x)))
}

se_values <- df %>%
  group_by(IQ) %>%
  summarise(
    Mittelwert_Kreativität = mean(Kreativität, na.rm = TRUE),
    SE_Kreativität = se_calc(Kreativität)
  )

print(se_values)

# t-Tests zwischen den IQ-Gruppen in Bezug auf Kreativität
t_12 <- t.test(Kreativität ~ IQ, data = df %>% filter(IQ %in% c(1, 2)))
t_13 <- t.test(Kreativität ~ IQ, data = df %>% filter(IQ %in% c(1, 3)))
t_23 <- t.test(Kreativität ~ IQ, data = df %>% filter(IQ %in% c(2, 3)))

# Ergebnisse ausgeben
list(
  "t-Test 1 vs 2" = list(t_value = t_12$statistic, p_value = t_12$p.value),
  "t-Test 1 vs 3" = list(t_value = t_13$statistic, p_value = t_13$p.value),
  "t-Test 2 vs 3" = list(t_value = t_23$statistic, p_value = t_23$p.value))











