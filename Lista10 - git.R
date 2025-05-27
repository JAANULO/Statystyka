# lista10

# Wczytanie danych z pliku CSV z użyciem średnika jako separatora
waga1 <- read.csv2("waga1.csv", stringsAsFactors = FALSE, sep = ";")

# Konwersja kolumn na typ numeryczny (zamiana przecinków na kropki w danych numerycznych)
waga1$plec <- as.numeric(waga1$plec)
waga1$Waga_po <- as.numeric(gsub(",", ".", waga1$Waga_po))
waga1$Waga_przed <- as.numeric(gsub(",", ".", waga1$Waga_przed))
waga1$Wzrost <- as.numeric(gsub(",", ".", waga1$Wzrost))

# Usunięcie wierszy z brakującymi danymi w kluczowych kolumnach
waga1 <- waga1[complete.cases(waga1[, c("plec", "Waga_po", "Waga_przed", "Wzrost")]), ]

#zadanie 1
#a) Testowanie hipotezy, że 40% populacji ma wyższe wykształcenie
#i) Ręczny test Z dla proporcji
#wzór Z = p^ - po / √( po( 1 - po) / n
n <- 1000       # Całkowita liczba obserwacji
x <- 385        # Liczba osób z wyższym wykształceniem
p0 <- 0.4       # Hipotezowana proporcja
p_hat <- x/n    # Obliczona proporcja z próby
se <- sqrt(p0*(1-p0)/n)  # Błąd standardowy przy założeniu H0
z <- (p_hat - p0)/se     # Statystyka Z
p_value <- 2 * (1 - pnorm(abs(z)))  # Dwustronne p-value
cat("Zadanie1_a_i)\nZ =", z, "p-value =", p_value, "\n\n")

#ii) Test proporcji używając funkcji prop.test (bez poprawki na ciągłość)
cat("Zadanie1_a_ii)\n")
prop.test(x, n, p = p0, correct = FALSE)

#b) Testowanie niezależności płci a ukończenia studiów
#i) Ręczny test Z dla dwóch proporcji (ze wspólną proporcją pod H0)

n1 <- 520; x1 <- 220   # Kobiety: próba i liczba z wykształceniem
n2 <- 480; x2 <- 165   # Mężczyźni: próba i liczba z wykształceniem
p1 <- x1/n1; p2 <- x2/n2
p_pool <- (x1 + x2)/(n1 + n2)  # Wspólna proporcja pod H0
se_pool <- sqrt(p_pool*(1 - p_pool)*(1/n1 + 1/n2))  # Błąd standardowy
z <- (p1 - p2)/se_pool         # Statystyka Z
p_value <- 2 * (1 - pnorm(abs(z)))  # Dwustronne p-value
cat("Zadanie1_b_i)\nZ =", z, "p-value =", p_value, "\n\n")

#ii) Test proporcji dla dwóch grup z prop.test
cat("Zadanie1_b_ii)\n")
prop.test(c(x1, x2), c(n1, n2), correct = FALSE)

#c) Test Z dla różnicy średnich wzrostu między płciami (znane wariancje)
mean_w <- 166; var_w <- 100; n_w <- 520  # Kobiety: średnia, wariancja, próba
mean_m <- 174; var_m <- 121; n_m <- 480  # Mężczyźni: średnia, wariancja, próba
se_diff <- sqrt(var_w/n_w + var_m/n_m)   # Błąd standardowy różnicy
z <- (mean_w - mean_m)/se_diff           # Statystyka Z
p_value <- 2 * (1 - pnorm(abs(z)))       # Dwustronne p-value
cat("Zadanie1_c_i)\nZ =", z, "p-value =", p_value, "\n\n")

#zadanie2 : Testowanie proporcji kobiet wśród studentów
#a) Ręczny test Z dla proporcji 0.5
n_female <- sum(waga1$plec == 1)  # Liczba kobiet
n_total <- nrow(waga1)            # Całkowita liczba studentów
p0 <- 0.5                         # Hipotezowana proporcja
p_hat <- n_female/n_total         # Obliczona proporcja
se <- sqrt(p0*(1 - p0)/n_total)   # Błąd standardowy
z <- (p_hat - p0)/se              # Statystyka Z
p_value <- 2 * (1 - pnorm(abs(z))) # Dwustronne p-value
cat("Zadanie2_a)\nZ =", z, "p-value =", p_value, "\n\n")

#b) Test z użyciem prop.test
cat("Zadanie2_b)\n")
prop.test(n_female, n_total, p = 0.5, correct = FALSE)


#zadanie 3: Testowanie różnicy średniej wagi między płciami
#a) Ręczny test Z dla dwóch średnich (niezakładający równości wariancji)
#wzór: SE = √(σ₁² / n₁ + σ₂² / n₂)

waga_k <- waga1$Waga_po[waga1$plec == 1]  # Wagi kobiet
waga_m <- waga1$Waga_po[waga1$plec == 0]  # Wagi mężczyzn

mean_k <- mean(waga_k); var_k <- var(waga_k); n_k <- length(waga_k)
mean_m <- mean(waga_m); var_m <- var(waga_m); n_m <- length(waga_m)

se_diff <- sqrt(var_k/n_k + var_m/n_m)  # Błąd standardowy różnicy
z <- (mean_k - mean_m)/se_diff          # Statystyka Z
p_value <- 2 * (1 - pnorm(abs(z)))      # Dwustronne p-value
cat("Zadanie3_a)\nZ =", z, "p-value =", p_value, "\n\n")

#b) Test t Studenta (Welch) z użyciem t.test
cat("Zadanie3_b)\n")
t.test(waga_k, waga_m, var.equal = FALSE)  # Nie zakłada równości wariancji


#zadanie 4: Proporcje osób z wagą >70kg między płciami
waga_k_70 <- sum(waga1$plec == 1 & waga1$Waga_po > 70)  # Kobiety >70kg
n_k <- sum(waga1$plec == 1)                             # Liczba kobiet
waga_m_70 <- sum(waga1$plec == 0 & waga1$Waga_po > 70)  # Mężczyźni >70kg
n_m <- sum(waga1$plec == 0)                             # Liczba mężczyzn

#a) Ręczny test Z dla dwóch proporcji z wspólną proporcją pod H0
cat("Zadanie4_a)\n")
if (n_k > 0 && n_m > 0) {
  p1 <- waga_k_70/n_k; p2 <- waga_m_70/n_m
  p_pool <- (waga_k_70 + waga_m_70)/(n_k + n_m)
  se <- sqrt(p_pool*(1 - p_pool)*(1/n_k + 1/n_m))  # Błąd standardowy
  z <- (p1 - p2)/se
  p_value <- 2 * pnorm(-abs(z))  # Dwustronne p-value
  cat("Proporcje: K =", p1, ", M =", p2, "\nZ =", z, "p-value =", p_value, "\n")
}

#b) Test proporcji z prop.test
cat("\nZadanie4_b)\n")
prop.test(c(waga_k_70, waga_m_70), c(n_k, n_m), correct = FALSE)

#c) Bootstrap dla różnicy proporcji (95% CI)
library(boot)

# Funkcja do bootstrapu: oblicza różnicę proporcji między płciami
boot_func <- function(data, indices) {
  sample_data <- data[indices, ]
  prop_k <- mean(sample_data$Waga_po[sample_data$plec == 1] > 70)
  prop_m <- mean(sample_data$Waga_po[sample_data$plec == 0] > 70)
  return(prop_k - prop_m)
}
set.seed(123)  # Powtarzalność wyników
boot_res <- boot(waga1, boot_func, R = 1000)  # 1000 replikacji
cat("\nZadanie4_c)\n")
print(boot.ci(boot_res, type = "perc", conf = 0.95))  # Przedział percentylowy


#zadanie 5: Testowanie, czy mężczyźni są średnio o 5cm wyżsi
wzrost_k <- waga1$Wzrost[waga1$plec == 1]
wzrost_m <- waga1$Wzrost[waga1$plec == 0]
mean_diff <- mean(wzrost_m) - mean(wzrost_k)  # Rzeczywista różnica
se_diff <- sqrt(var(wzrost_m)/length(wzrost_m) + var(wzrost_k)/length(wzrost_k))
z <- (mean_diff - 5)/se_diff  # Testowanie H0: różnica = 5cm
p_value <- 2 * (1 - pnorm(abs(z)))
cat("\nZadanie5\nZ =", z, "p-value =", p_value, "\n\n")


#zadanie 6: Testowanie, czy 80% studentów przybiera na wadze
waga1$change <- waga1$Waga_po - waga1$Waga_przed  # Zmiana wagi
n_gained <- sum(waga1$change > 0)  # Liczba osób z przyrostem wagi
n_total <- nrow(waga1)
p_hat <- n_gained/n_total          # Obliczona proporcja
se <- sqrt(0.8 * 0.2 / n_total)    # Błąd standardowy przy H0: p=0.8
z <- (p_hat - 0.8)/se              # Statystyka Z
p_value <- 2 * (1 - pnorm(abs(z))) # Dwustronne p-value
cat("Zadanie6\nZ =", z, "p-value =", p_value, "\n\n")


#zadanie 7: Proporcje osób wyższych niż 170cm między płciami
#a) Ręczny test Z dla dwóch proporcji
#wzór: w = (suma )
taller_k <- sum(waga1$plec == 1 & waga1$Wzrost > 170)
n_k <- sum(waga1$plec == 1)
taller_m <- sum(waga1$plec == 0 & waga1$Wzrost > 170)
n_m <- sum(waga1$plec == 0)
p_pool <- (taller_k + taller_m)/(n_k + n_m)  # Wspólna proporcja pod H0
se_pool <- sqrt(p_pool*(1 - p_pool)*(1/n_k + 1/n_m))
z <- (taller_k/n_k - taller_m/n_m)/se_pool   # Statystyka Z
p_value <- 2 * (1 - pnorm(abs(z)))           # Dwustronne p-value
cat("Zadanie7_a)\nZ =", z, "p-value =", p_value, "\n\n")

#b) Bootstrap dla różnicy proporcji (98% CI)
boot_diff_prop <- function(data1, data2, R) {
  diff_props <- numeric(R)
  for (i in 1:R) {
    samp1 <- sample(data1$Wzrost, replace = TRUE)  # Próbka kobiet
    samp2 <- sample(data2$Wzrost, replace = TRUE)  # Próbka mężczyzn
    diff_props[i] <- mean(samp1 > 170) - mean(samp2 > 170)  # Różnica proporcji
  }
  diff_props
}
set.seed(123)
female <- subset(waga1, plec == 1)
male <- subset(waga1, plec == 0)
boot_diffs <- boot_diff_prop(female, male, 1000)  # 1000 replikacji
ci <- quantile(boot_diffs, c(0.01, 0.99))         # 98% przedział
cat("Zadanie7_b)\n98% CI:", ci, "\n")
