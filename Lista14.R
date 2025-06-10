# lista14

#zadanie 1 – model regresji ceny za m2

#a) regresja krokowa (wsteczna)
dane <- read.csv("mieszkania.csv", sep = ";")
dane$cena_m2 <- dane$Cena / dane$Metraz
dane$Pietro_1 <- ifelse(dane$Pietro > 4, 1, 0)

# utwórz zmienne zero-jedynkowe (indykatory) dla dzielnicy
dane$dzielF <- ifelse(dane$Dzielnica == "Fabryczna", 1, 0)
dane$dzielP <- ifelse(dane$Dzielnica == "Psie Pole", 1, 0)
dane$dzielSr <- ifelse(dane$Dzielnica == "Śródmieście", 1, 0)
dane$dzielSt <- ifelse(dane$Dzielnica == "Stare Miasto", 1, 0)

model_full <- lm(cena_m2 ~ Metraz + Pietro_1 + dzielF + dzielP + dzielSr + dzielSt, data = dane)
model_step <- step(model_full, direction = "backward")

cat("\nZadanie1_a:\n")
summary(model_step)

#b) oszacowanie średniej ceny m2 dla dwóch mieszkań
newdata <- data.frame(
  Metraz = c(80, 65),
  Pietro_1 = c(1, 0),
  dzielF = c(1, 0),
  dzielP = c(0, 0),
  dzielSr = c(0, 0),
  dzielSt = c(0, 0) )

cat("\nZadanie1_b:\n")
predict(model_step, newdata, interval = "confidence")

#c) reszty
cat("\nZadanie1_c:\n")
resid1 <- residuals(model_step)
print(head(resid1))

#d) test normalności reszt
cat("\nZadanie1_d:\n")
print(shapiro.test(resid1))


#zadanie 2 – model regresji dla całkowitej ceny (Y)

#a) regresja krokowa (wsteczna) dla zmiennej Cena (nie cena za m2)
model2_full <- lm(Cena ~ Metraz + Pietro_1 + dzielF + dzielP + dzielSr + dzielSt, data = dane)
model2_step <- step(model2_full, direction = "backward")

cat("\nZadanie2_a:\n")
summary(model2_step)

#b) prognozy dla dwóch mieszkań
cat("\nZadanie2_b:\n")
predict(model2_step, newdata, interval = "confidence")

#c) reszty
cat("\nZadanie2_c:\n")
resid2 <- residuals(model2_step)
print(head(resid2))

#d) test normalności reszt
cat("\nZadanie2_d:\n")
print(shapiro.test(resid2))


#zadanie 3 bakterie

#a) wykres rozrzutu masy względem czasu
bakt <- read.csv("bakteria.csv", sep = ";", dec = ",")
colnames(bakt) <- c("T", "Y")  # nadaj poprawne nagłówki

#ane są liczbowe
bakt$T <- as.numeric(bakt$T)
bakt$Y <- as.numeric(bakt$Y)

# sprawdzenie poprawności danych
cat("\nZadanie3_a: Diagnostyka danych\n")
print(str(bakt))
print(summary(bakt))
print(head(bakt))

plot(bakt$T, bakt$Y, main = "Zadanie3_a: Masa bakterii względem czasu", xlab = "Czas (T)", ylab = "Masa (Y)")

#b) regresja liniowa: masa = a + b*t
cat("\nZadanie3_b:\n")
model_lin <- lm(Y ~ T, data = bakt)
summary(model_lin)

#c) regresja liniowa: log(Y) = a + b*t
cat("\nZadanie3_c:\n")
model_log <- lm(log(Y) ~ T, data = bakt)
summary(model_log)

#d) oszacowanie masy bakterii (regresja wykładnicza)
bakt$Y_exp <- exp(predict(model_log))
cat("\nZadanie3_d: Szacowana masa bakterii (Y_exp)\n")
print(head(bakt[, c("T", "Y", "Y_exp")]))
