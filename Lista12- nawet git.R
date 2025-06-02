#lista 12

#options(repos = "https://cloud.r-project.org")
#zadanie 1

#a) obliczenia ręczne
#b) statystyka f
#c) test hipotezy
#d) weryfikacja aov()

# dane z zadania 1
polacy <- c(68, 80, 74, 62)
brytyjczycy <- c(85, 67, 79, 73)
chinczycy <- c(60, 66, 57)

# a) obliczenia anova "ręcznie"
dane <- c(polacy, brytyjczycy, chinczycy)
grupy <- factor(rep(1:3, times = c(4, 4, 3)))
srednia_ogolna <- mean(dane)

srednie_grup <- tapply(dane, grupy, mean)

n_grup <- tapply(dane, grupy, length)

c_total <- sum((dane - srednia_ogolna)^2)
mg <- sum(n_grup * (srednie_grup - srednia_ogolna)^2)
wg <- c_total - mg

# stopnie swobody
df_mg <- length(unique(grupy)) - 1  # między grupami
df_wg <- length(dane) - length(unique(grupy))  # wewnątrz grup
df_c <- length(dane) - 1  # całkowite

# średnie kwadratowe
ms_mg <- mg / df_mg
ms_wg <- wg / df_wg

# tworzenie tabeli anova
tabela_anova <- data.frame(
  `suma kwadratów` = c(mg, wg, c_total),
  `stopnie swobody` = c(df_mg, df_wg, df_c),
  `średnia kwadratów` = c(ms_mg, ms_wg, NA),
  row.names = c("mg", "wg", "c") )

#a) prezentacja tabeli
cat("Zadanie1_a)\n")
print(tabela_anova)
cat("\n")

#b) statystyka f
f_stat <- ms_mg / ms_wg
cat("Zadanie1_b)\nZ =", f_stat, "\n\n")

#c) test hipotezy
alpha <- 0.05
f_krytyczny <- qf(1 - alpha, df_mg, df_wg)
p_value <- 1 - pf(f_stat, df_mg, df_wg)

decyzja <- if(p_value < alpha) {
  "odrzucamy h0 - istnieją istotne różnice między grupami (waga zależy od narodowości)"
} else {
  "brak podstaw do odrzucenia h0 - nie ma istotnych różnic między grupami"
}

cat("Zadanie1_c)\n", decyzja, "(p-value =", p_value, ")\n\n")

#d) weryfikacja za pomocą aov()
model_aov <- aov(dane ~ grupy)
cat("Zadanie1_d)\n")
print(summary(model_aov))
cat("\n")

#zadanie 2
#a) test anova dla metrażu wg dzielnic
#b) porównanie parami

# wczytanie danych - zakładając, że plik jest w bieżącym katalogu
if (!require("readxl")) install.packages("readxl")
library(readxl)

# wczytanie danych
mieszkania <- read_excel("mieszkania.xlsx")

#a) test anova
model_metraz <- aov(metraz ~ dzielnica, data = mieszkania)
cat("Zadanie2_a)\n")
print(summary(model_metraz))
cat("\n")

#b) porównanie parami (tukey hsd)
tukey_metraz <- TukeyHSD(model_metraz)
cat("Zadanie2_b)\n")
print(tukey_metraz)
cat("\n")

#zadanie 3
#a) kategoryzacja mieszkań
#b) test anova dla ceny/m2 wg kategorii
#c) porównanie parami

#a) tworzenie kategorii
mieszkania$kategoria <- cut(
  mieszkania$pokoje,
  breaks = c(0, 1, 2, 3, Inf),
  labels = c("1-pokojowe", "2-pokojowe", "3-pokojowe", "wielopokojowe"),
  include.lowest = TRUE
)

# obliczenie ceny za m2
mieszkania$cena_m2 <- mieszkania$cena / mieszkania$metraz

#b) test anova
model_cena <- aov(cena_m2 ~ kategoria, data = mieszkania)
cat("Zadanie3_b)\n")
print(summary(model_cena))
cat("\n")

#c) porównanie parami (tukey hsd)
tukey_cena <- TukeyHSD(model_cena)
cat("Zadanie3_c)\n")
print(tukey_cena)