#lista11

#zadanie1  tablica rzutu kostka

#a) oczekiwana frekwencja , kostka jest symetryczna
#wzór:  e = n / 6
observed <- c(171, 200, 168, 213, 226, 222)
n <- sum(observed)
expected <- rep(n/6, 6) #kosta jest uczciwa
cat("Zadanie1_a:\nZ =", expected, "\n\n")

#b) realizacja statyski testowej , test zgodnosci
#wzór:  x^2 = suma[ (Oi - Ei)^2 / Ei ]
chi_stat <- sum((observed - expected)^2 / expected)
cat("Zadanie1_b:\nZ =", chi_stat, "\n\n")

#c) wyznaczanie p
#wzór: p = 1 - Fx^2 (x;df)
df <- length(observed) - 1  #liczba stopni swodosy
p_value <- 1 - pchisq(chi_stat, df)
cat("Zadanie1_c:\nZ =", p_value, "\n\n")

#d) wniosek
alpha <- 0.05 #poziom istotnosci
wniosek <- ifelse(p_value < alpha, "Odrzucamy H0: Kostka nie jest symetryczna.", "Brak podstaw do odrzucenia H0: Kostka jest symetryczna.")
cat("Zadanie1_d:\nZ =", wniosek, "\n\n")

#e) chisq.test
result <- chisq.test(observed, p = rep(1/6, 6))
cat("Zadanie1_e: (statystyka) :\nZ =", result$statistic, "\n\n")
cat("Zadanie1_e: (p-value) :\nZ =", result$p.value, "\n\n")


#zadanie2  tablica  plec/wyksztalcenie
#a)  oczekiwana frekwencja , niezaleznosc cech
#wzór: Eij = Ri* Cj / N
observed_matrix <- matrix(c(200, 300, 150, 350), nrow = 2, byrow = TRUE)
expected <- outer(rowSums(observed_matrix), colSums(observed_matrix)) / sum(observed_matrix)
cat("Zadanie2_a:\n")
print(expected)
cat("\n")

#b) realizacja statyski testowej , test niezależnosci Pearsona
#wzór:  x^2 = suma[ (Oi - Ei)^2 / Ei ]
chi_stat2 <- sum((observed_matrix - expected)^2 / expected)
cat("Zadanie2_b:\nZ =", chi_stat2, "\n\n")

#c) wartosc p
#wzór: p = 1 - Fx^2 (x;df)
df2 <- (nrow(observed_matrix)-1)*(ncol(observed_matrix)-1) # (r-1)*(c-1)
p_value2 <- 1 - pchisq(chi_stat2, df2)
cat("Zadanie2_c:\nZ =", p_value2, "\n\n")

#d) wniosek
wniosek2 <- ifelse(p_value2 < alpha, "Odrzucamy H0: Istnieje zależność między płcią a wykształceniem.", "Brak podstaw do odrzucenia H0: Cechy są niezależne.")
cat("Zadanie2_d:\nZ =", wniosek2, "\n\n")

#e) chisq.test
result2 <- chisq.test(observed_matrix)
cat("Zadanie2_e: (statystyka):\nZ =", result2$statistic, "\n\n")
cat("Zadanie2_e: (p-value):\nZ =", result2$p.value, "\n\n")

#f) dokladny test Fishera  (fisher.test)
fisher_result <- fisher.test(observed_matrix)
cat("Zadanie2_f: (p-value):\nZ =", fisher_result$p.value, "\n\n")


#zadanie3
data <- read.csv2(text = gsub(",", ".", readLines("mieszkania.csv")), sep = ";")

#a) rozklad liczby pokoi w zaleznosci od dzielnicy
table_3a <- table(data$Dzielnica, data$Pokoje)
cat("Zadanie3_a:\n")
print(table_3a)
cat("\n")

#b) jakas smieszna zmienna
data$nowa_pokoje <- ifelse(data$Pokoje >= 4, 4, data$Pokoje) #wiecej niż 4 pokoje

#c) liczba pokoi niezalezna od dzielnicy
chi3 <- chisq.test(table(data$Dzielnica, data$nowa_pokoje))
cat("Zadanie3_c: (statystyka):\nZ =", chi3$statistic, "\n\n")
cat("Zadanie3_c: (p-value):\nZ =", chi3$p.value, "\n\n")


#zadanie4
#a) zmienna która wskazuje czy cena za m^2 > 6000
data$cena_m2 <- data$Cena / data$Metraz
data <- data[!is.infinite(data$cena_m2), ] # Usuń wartości niepoprawne
data$cena_wysoka <- ifelse(data$cena_m2 > 6000, "Tak", "Nie")

#b) czy cena za m^2 jest zalezna od dzielnicy
chi4 <- chisq.test(table(data$Dzielnica, data$cena_wysoka))
cat("Zadanie4_b: (statystyka):\nZ =", chi4$statistic, "\n\n")
cat("Zadanie4_b_p-value:\nZ =", chi4$p.value, "\n\n")


#zadanie5
# a) Test normalności dla ceny za m²
#wzór suma[ ai x(i) ]^2  / suma [ xi- x (pod kreską)^2 ]
cat("Zadanie5_a:\n")
shapiro_test_cena <- shapiro.test(data$cena_m2)
cat("Shapiro-Wilk p-value (cena/m2):", shapiro_test_cena$p.value, "\n")

# estymator gęstości wykres
plot(density(data$cena_m2, na.rm = TRUE), main = "Estymator gęstości dla ceny za m²", xlab = "Cena za m²", col = "blue")
abline(v = mean(data$cena_m2, na.rm = TRUE), col = "red", lty = 2)
cat("\n")

#b) Test normalności dla metrażu mieszkań w Śródmieściu
cat("Zadanie5_b:\n")
srodmiescie_data <- subset(data, Dzielnica == "Śródmieście")
srodmiescie_metraz <- na.omit(srodmiescie_data$Metraz)

if (length(srodmiescie_metraz) >= 3) {
  shapiro_test_metraz <- shapiro.test(srodmiescie_metraz)
  cat("Shapiro-Wilk p-value (metraż w Śródmieściu):", shapiro_test_metraz$p.value, "\n")

  # Estymator gęstości
  plot(density(srodmiescie_metraz),
       main = "Estymator gęstości dla metrażu w Śródmieściu",
       xlab = "Metraż",
       col = "darkgreen")
  abline(v = mean(srodmiescie_metraz), col = "red", lty = 2)
} else {
  cat("Za mało danych do wykonania testu Shapiro-Wilka (min. 3 obserwacje).\n")
}
cat("\n")

#zadanie6
#a) 1000 realizacji z rozkładu wykladniczego o wartosci oczekiwanej 1
set.seed(123)
exp_sample <- rexp(1000, rate = 1)
cat("Zadanie6_a:\nZ",exp_sample,"\n\n")

#b) test
#i) rozkad normalny o sredniej 1 odchylenie 1
ks_normal <- ks.test(exp_sample, "pnorm", mean = 1, sd = 1)
cat("Zadanie6_b_i:\nZ =", ks_normal$p.value, "\n\n")

#ii) rozklad wykladniczy lambda=1
ks_exp <- ks.test(exp_sample, "pexp", rate = 1)
cat("Zadanie6_b_ii:\nZ =", ks_exp$p.value, "\n\n")

#c) 1000 realizacji Gammma ,ksztalt 100 , skala 1
gamma_sample <- rgamma(1000, shape = 100, rate = 1)
cat("Zadanie6_c:\nZ =" ,gamma_sample, "\n\n")

#d) testy hipotez
#i) rozkalad normalny , srednia 100 , odchylenie 10
ks_normal_gamma <- ks.test(gamma_sample, "pnorm", mean = 100, sd = 10)
cat("Zadanie6_d_i:\nZ =", ks_normal_gamma$p.value, "\n\n")

#ii) rozklad Gamma , parametry 100,1   (srednia,odchylenie)
ks_gamma <- ks.test(gamma_sample, "pgamma", shape = 100, rate = 1)
cat("Zadanie6_d_ii:\nZ =", ks_gamma$p.value, "\n\n")