# lista10

# Wczytywanie danych
waga1 <- read.csv2("waga1.csv", stringsAsFactors = FALSE, sep = ";")

# Konwersja kolumn i usuwanie brakujących danych
waga1$plec <- as.numeric(waga1$plec)
waga1$Waga_po <- as.numeric(gsub(",", ".", waga1$Waga_po))
waga1$Waga_przed <- as.numeric(gsub(",", ".", waga1$Waga_przed))
waga1$Wzrost <- as.numeric(gsub(",", ".", waga1$Wzrost))

waga1 <- waga1[complete.cases(waga1[, c("plec", "Waga_po", "Waga_przed", "Wzrost")]), ]

# zadanie1
#a)
# i) test Z
n <- 1000
x <- 385
p0 <- 0.4
p_hat <- x/n
se <- sqrt(p0*(1-p0)/n)
z <- (p_hat - p0)/se
p_value <- 2 * (1 - pnorm(abs(z)))
cat("Zadanie1_a_i)\nZ =", z, "p-value =", p_value, "\n\n")

# ii) prop.test
cat("Zadanie1_a_ii)\n")
prop.test(x, n, p = p0, correct = FALSE)

# b)
# i) test Z
n1 <- 520; x1 <- 220
n2 <- 480; x2 <- 165
p1 <- x1/n1; p2 <- x2/n2
p_pool <- (x1 + x2)/(n1 + n2)
se_pool <- sqrt(p_pool*(1 - p_pool)*(1/n1 + 1/n2))
z <- (p1 - p2)/se_pool
p_value <- 2 * (1 - pnorm(abs(z)))
cat("Zadanie1_b_i)\nZ =", z, "p-value =", p_value, "\n\n")

# ii) prop.test
cat("Zadanie1_b_ii)\n")
prop.test(c(x1, x2), c(n1, n2), correct = FALSE)

# c)
# i) test Z
mean_w <- 166; var_w <- 100; n_w <- 520
mean_m <- 174; var_m <- 121; n_m <- 480
se_diff <- sqrt(var_w/n_w + var_m/n_m)
z <- (mean_w - mean_m)/se_diff
p_value <- 2 * (1 - pnorm(abs(z)))
cat("Zadanie1_c_i)\nZ =", z, "p-value =", p_value, "\n\n")

# zadanie2
# a) test Z
n_female <- sum(waga1$plec == 1)
n_total <- nrow(waga1)
p0 <- 0.5
p_hat <- n_female/n_total
se <- sqrt(p0*(1 - p0)/n_total)
z <- (p_hat - p0)/se
p_value <- 2 * (1 - pnorm(abs(z)))
cat("Zadanie2_a)\nZ =", z, "p-value =", p_value, "\n\n")

# b) prop.test
cat("Zadanie2_b)\n")
prop.test(n_female, n_total, p = 0.5, correct = FALSE)

# zadanie3
# a) test Z
waga_k <- waga1$Waga_po[waga1$plec == 1]
waga_m <- waga1$Waga_po[waga1$plec == 0]

mean_k <- mean(waga_k)
var_k <- var(waga_k)
n_k <- length(waga_k)

mean_m <- mean(waga_m)
var_m <- var(waga_m)
n_m <- length(waga_m)

se_diff <- sqrt(var_k/n_k + var_m/n_m)
z <- (mean_k - mean_m)/se_diff
p_value <- 2 * (1 - pnorm(abs(z)))

cat("Zadanie3_a)\nZ =", z, "p-value =", p_value, "\n\n")

# b) t.test
cat("Zadanie3_b)\n")
t.test(waga_k, waga_m, var.equal = FALSE)

# zadanie4
# a) test Z
waga_k_70 <- sum(waga1$plec == 1 & waga1$Waga_po > 70)
n_k <- sum(waga1$plec == 1)
waga_m_70 <- sum(waga1$plec == 0 & waga1$Waga_po > 70)
n_m <- sum(waga1$plec == 0)
p1 <- waga_k_70/n_k; p2 <- waga_m_70/n_m
p_pool <- (waga_k_70 + waga_m_70)/(n_k + n_m)
se_pool <- sqrt(p_pool*(1 - p_pool)*(1/n_k + 1/n_m))

z <- (p1 - p2)/se_pool
p_value <- 2 * (1 - pnorm(abs(z)))
cat("Zadanie4_a)\nZ =", z, "p-value =", p_value, "\n\n")

# b) test proporcji (poprawiony)
cat("Zadanie4_b)\n")
prop.test(c(waga_k_70, waga_m_70), c(n_k, n_m), correct = FALSE)

# c) bootstrap
library(boot)
boot_func <- function(data, indices) {
  sample_data <- data[indices, ]
  prop_k <- mean(sample_data$Waga_po[sample_data$plec == 1] > 70)
  prop_m <- mean(sample_data$Waga_po[sample_data$plec == 0] > 70)
  return(prop_k - prop_m)
}
set.seed(123)
boot_res <- boot(waga1, boot_func, R = 1000)
cat("Zadanie4_c)\n")
print(boot.ci(boot_res, type = "perc", conf = 0.95))

# zadanie5
wzrost_k <- waga1$Wzrost[waga1$plec == 1]
wzrost_m <- waga1$Wzrost[waga1$plec == 0]
mean_diff <- mean(wzrost_m) - mean(wzrost_k)
se_diff <- sqrt(var(wzrost_m)/length(wzrost_m) + var(wzrost_k)/length(wzrost_k))
z <- (mean_diff - 5)/se_diff
p_value <- 2 * (1 - pnorm(abs(z)))
cat("Zadanie5\nZ =", z, "p-value =", p_value, "\n\n")

# zadanie6
waga1$change <- waga1$Waga_po - waga1$Waga_przed
n_gained <- sum(waga1$change > 0)
n_total <- nrow(waga1)
p_hat <- n_gained/n_total
se <- sqrt(0.8 * 0.2 / n_total)
z <- (p_hat - 0.8)/se
p_value <- 2 * (1 - pnorm(abs(z)))
cat("Zadanie6\nZ =", z, "p-value =", p_value, "\n\n")

# zadanie7
# a)
taller_k <- sum(waga1$plec == 1 & waga1$Wzrost > 170)
n_k <- sum(waga1$plec == 1)
taller_m <- sum(waga1$plec == 0 & waga1$Wzrost > 170)
n_m <- sum(waga1$plec == 0)
p_pool <- (taller_k + taller_m)/(n_k + n_m)
se_pool <- sqrt(p_pool*(1 - p_pool)*(1/n_k + 1/n_m))
z <- (taller_k/n_k - taller_m/n_m)/se_pool
p_value <- 2 * (1 - pnorm(abs(z)))
cat("Zadanie7_a)\nZ =", z, "p-value =", p_value, "\n\n")

# b) bootstrap
boot_diff_prop <- function(data1, data2, R) {
  diff_props <- numeric(R)
  for (i in 1:R) {
    samp1 <- sample(data1$Wzrost, replace = TRUE)
    samp2 <- sample(data2$Wzrost, replace = TRUE)
    diff_props[i] <- mean(samp1 > 170) - mean(samp2 > 170)
  }
  diff_props
}
set.seed(123)
female <- subset(waga1, plec == 1)
male <- subset(waga1, plec == 0)
boot_diffs <- boot_diff_prop(female, male, 1000)
ci <- quantile(boot_diffs, c(0.01, 0.99))
cat("Zadanie7_b)\n98% CI:", ci, "\n")
