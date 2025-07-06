########### Schritt 1: Daten vorbereiten (mit mtcars)

set.seed(123)
data(mtcars)

# Ziel: mpg vorhersagen anhand von Gewicht (wt)
X <- mtcars$wt
Y <- mtcars$mpg

# Aufteilung: 70 % Training, 30 % Kalibrierung
n <- length(Y)
idx_train <- sample(1:n, size = floor(0.7 * n))
idx_cal <- setdiff(1:n, idx_train)

X_train <- X[idx_train]
Y_train <- Y[idx_train]
X_cal <- X[idx_cal]
Y_cal <- Y[idx_cal]


########### Schritt 2: Modell trainieren (lineare Regression)
model <- lm(Y_train ~ X_train)


########### Schritt 3: Residuen (Nonkonformitätswerte) berechnen
# Vorhersagen auf Kalibrierungssatz
Y_pred_cal <- predict(model, newdata = data.frame(X_train = X_cal))

# Nonkonformitätswerte (absolute Fehler)
nonconformity_scores <- abs(Y_cal - Y_pred_cal)


########### Schritt 4: Schwelle berechnen (90 %-Abdeckung)
alpha <- 0.1
q_hat <- quantile(nonconformity_scores, probs = 1 - alpha)


########### Schritt 5: Vorhersage + Intervall für neuen Punkt
x_new <- 3.0
y_hat <- predict(model, newdata = data.frame(X_train = x_new))

# Konfidenzintervall (Conformal)
interval <- c(y_hat - q_hat, y_hat + q_hat)

#Ausgabe
cat("Vorhersage:", round(y_hat, 2), "\n")
cat("90%-intervall:", round(interval[1], 2), "-", round(interval[2], 2), "\n")



