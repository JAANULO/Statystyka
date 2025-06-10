#lista13

#zadanie 1 – egzaminy studentów

# dane punktowe
analiza <- c(28, 26, 23, 18, 14, 12)
algebra <- c(25, 27, 20, 24, 16, 13)

#a) Wyznaczanie współczynników korelacji
#i) Pearson
pearson_r <- cor(analiza, algebra, method = "pearson")
cat("\nZadanie1_ai: Współczynnik korelacji Pearsona =", pearson_r, "\n")

#ii) Spearman
spearman_r <- cor(analiza, algebra, method = "spearman")
cat("Zadanie1_aii: Współczynnik korelacji Spearmana =", spearman_r, "\n")

#iii) Kendall
kendall_r <- cor(analiza, algebra, method = "kendall")
cat("Zadanie1_aiii: Współczynnik korelacji Kendalla =", kendall_r, "\n")

#b) Testowanie hipotezy o braku korelacji
#i) Test Pearsona
cat("\nZadanie1_bi: Test korelacji Pearsona\n")
print(cor.test(analiza, algebra, method = "pearson"))

#ii) Test Spearmana
cat("\nZadanie1_bii: Test korelacji Spearmana\n")
print(cor.test(analiza, algebra, method = "spearman"))

#iii) Test Kendalla
cat("\nZadanie1_biii: Test korelacji Kendalla\n")
print(cor.test(analiza, algebra, method = "kendall"))

#zadanie 2 – losowanie X i Y ~ N(0,1)

#a) Losowanie 100 realizacji pary (X, Y)
set.seed(123)
x <- rnorm(100)
y <- rnorm(100)

#b) Wyznaczenie współczynnika korelacji i testowanie hipotezy H0: rho(X,Y)=0
#i) Pearson
cat("\nZadanie2_bi (Pearson):\n")
print(cor.test(x, y, method = "pearson"))

#ii) Spearman
cat("\nZadanie2_bii (Spearman):\n")
print(cor.test(x, y, method = "spearman"))

#iii) Kendall
cat("\nZadanie2_biii (Kendall):\n")
print(cor.test(x, y, method = "kendall"))

#c) Testowanie hipotezy H0: rho(X,Y)=0 za pomocą testu permutacyjnego (B = 1000)
perm_test <- function(x, y, method, B = 1000) {
  obs <- cor(x, y, method = method)
  perms <- replicate(B, cor(x, sample(y), method = method))
  pval <- mean(abs(perms) >= abs(obs))
  return(list(stat = obs, p_value = pval))
}

#i) Pearson
cat("\nZadanie2_ci (Permutacja Pearson):\n")
print(perm_test(x, y, "pearson"))

#ii) Spearman
cat("\nZadanie2_cii (Permutacja Spearman):\n")
print(perm_test(x, y, "spearman"))

#iii) Kendall
cat("\nZadanie2_ciii (Permutacja Kendall):\n")
print(perm_test(x, y, "kendall"))

#d) Wyznaczanie przedziałów ufności i testowanie hipotezy za pomocą bootstrappingu (R = 1000)
library(boot)
data_xy <- data.frame(x, y)

boot_cor <- function(data, indices, method) {
  cor(data[indices, 1], data[indices, 2], method = method)
}

#i) Pearson
cat("\nZadanie2_di (Bootstrap Pearson):\n")
boot_p <- boot(data_xy, function(d, i) boot_cor(d, i, "pearson"), R = 1000)
print(boot.ci(boot_p, type = "perc"))

#ii) Spearman
cat("\nZadanie2_dii (Bootstrap Spearman):\n")
boot_s <- boot(data_xy, function(d, i) boot_cor(d, i, "spearman"), R = 1000)
print(boot.ci(boot_s, type = "perc"))

#iii) Kendall
cat("\nZadanie2_diii (Bootstrap Kendall):\n")
boot_k <- boot(data_xy, function(d, i) boot_cor(d, i, "kendall"), R = 1000)
print(boot.ci(boot_k, type = "perc"))

#e) Sporządzenie wykresu rozrzutu zmiennych X i Y
plot(x, y, main = "Zadanie2_e: Rozrzut X i Y", xlab = "X", ylab = "Y")


#zadanie 3 – korelacja X i V = 2X + Y

v <- 2*x + y

#a) Korelacje i testy
cat("\nZadanie3_ai:\n")
print(cor.test(x, v, method = "pearson"))

cat("\nZadanie3_aii:\n")
print(cor.test(x, v, method = "spearman"))

cat("\nZadanie3_aiii:\n")
print(cor.test(x, v, method = "kendall"))

#b) Wykres
plot(x, v, main = "Zadanie3_b: Rozrzut X i V", xlab = "X", ylab = "V")


#zadanie4  – analiza danych mieszkań

#a) Wczytanie danych
dane <- read.csv("mieszkania.csv", sep = ";")
dane$cena_m2 <- dane$Cena / dane$Metraz

#b) Macierz korelacji Pearsona
cat("\nZadanie4_b:\n")
print(cor(dane[, c("Metraz", "Pokoje", "Cena", "cena_m2")], method = "pearson"))

#c) Macierz Spearmana
cat("\nZadanie4_c:\n")
print(cor(dane[, c("Cena", "Metraz", "cena_m2", "Pokoje")], method = "spearman"))

#d) Macierz Kendalla
cat("\nZadanie4_d:\n")
print(cor(dane[, c("Cena", "Metraz", "cena_m2", "Pokoje")], method = "kendall"))

#e) Testy hipotezy: korelacja między Metrażem a ceną za m2
cat("\nZadanie4_ei:\n")
print(cor.test(dane$Metraz, dane$cena_m2, method = "pearson"))

cat("\nZadanie4_eii:\n")
print(cor.test(dane$Metraz, dane$cena_m2, method = "spearman"))

cat("\nZadanie4_eiii:\n")
print(cor.test(dane$Metraz, dane$cena_m2, method = "kendall", exact = FALSE ))


#zadanie 5 – wzrost i waga z N(0,1)


set.seed(123)
X <- rnorm(100)
Y <- rnorm(100)

#a) Dane: H i W
H <- 170 + 10*X
W <- 70 + 15*Y

#b) Średnie i odchylenia standardowe
cat("\nZadanie5_b:\n")
cat("Średnia H =", mean(H), ", SD H =", sd(H), "\n")
cat("Średnia W =", mean(W), ", SD W =", sd(W), "\n")

#c) Korelacje
cat("\nZadanie5_ci:\n")
print(cor(H, W, method = "pearson"))

cat("\nZadanie5_cii:\n")
print(cor(H, W, method = "spearman"))

cat("\nZadanie5_ciii:\n")
print(cor(H, W, method = "kendall"))

#d) Estymatory gęstości
plot(density(H), main = "Zadanie5_d: Gęstość H i W", xlab = "Wartość")
lines(density(W), col = "blue")
legend("topright", legend = c("H (wzrost)", "W (waga)"), col = c("black", "blue"), lty = 1)

#e) Testy normalności
cat("\nZadanie5_ei:\n")
print(shapiro.test(H))

cat("\nZadanie5_eii:\n")
print(shapiro.test(W))

#f) Wykres rozrzutu
plot(H, W, main = "Zadanie5_e: Rozrzut H i W", xlab = "Wzrost (H)", ylab = "Waga (W)")
