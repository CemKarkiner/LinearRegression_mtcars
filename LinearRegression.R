# mtcars veri kümesini yükle
data(mtcars)


# --- 1. Korelasyon Hesapla ---
correlation <- cor(mtcars$hp, mtcars$mpg)
print(paste("Korelasyon katsayısı (hp vs mpg):", round(correlation, 3)))

# --- 2. Dağılım Grafiği (Scatter Plot) ve Regresyon Doğrusu ---
plot(mtcars$hp, mtcars$mpg,
     main = "Motor Gücü vs Yakıt Tüketimi",
     xlab = "Motor Gücü (hp)",
     ylab = "Yakıt Tüketimi (mpg)",
     pch = 19, col = "steelblue")
abline(lm(mpg ~ hp, data = mtcars), col = "red", lwd = 2)

# --- 3. Regresyon Modeli Kur ---
model <- lm(mpg ~ hp, data = mtcars)
summary(model)  # model özeti, katsayılar ve p-değerleri

# --- 4. Regresyon Denklemi ---
intercept <- coef(model)[1]
slope <- coef(model)[2]
print(paste("Regresyon Denklemi: mpg =", round(intercept, 4), "+", round(slope, 4), "* hp"))

# --- 5. Varsayım Kontrol Grafikleri
par(mfrow = c(2, 2))
plot(model)

# --- 6. Tahmin: 110 hp için mpg değeri ---
new_hp <- data.frame(hp = 110)
prediction <- predict(model, newdata = new_hp, interval = "confidence")
print("110 hp için tahmin:")
print(prediction)