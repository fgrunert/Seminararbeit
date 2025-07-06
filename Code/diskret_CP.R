require(rpart)

########### Schritt 1: Daten vorbereiten
set.seed(123)
data(iris)

# Shuffle
iris <- iris[sample(1:nrow(iris)), ]

# Eingabe-Features und Zielvariable
X <- iris[, 1:4]
Y <- iris$Species

# Aufteilung: 60% Training, 20% Kalibrierung, 20% Test
n <- nrow(iris)
n_train <- floor(0.6 * n)
n_cal <- floor(0.2 * n)

X_train <- X[1:n_train, ]
Y_train <- Y[1:n_train]

X_cal <- X[(n_train + 1):(n_train + n_cal), ]
Y_cal <- Y[(n_train + 1):(n_train + n_cal)]

X_test <- X[(n_train + n_cal + 1):n, ]
Y_test <- Y[(n_train + n_cal + 1):n]

########### Schritt 2: Modell trainieren (Decision Tree)
model <- rpart(Y_train ~ ., data = data.frame(X_train, Y_train = Y_train), method = "class")


########### Schritt 3: Nonkonformitätswerte berechnen (auf Kalibrierungsdaten)
# Vorhersagen mit Klassenwahrscheinlichkeiten
probs_cal <- predict(model, newdata = X_cal, type = "prob")

# Nonkonformitätswert: 1 - Wahrscheinlichkeit der wahren Klasse
nonconformity_scores <- numeric(n_cal)
for (i in 1:n_cal) {
  true_class <- Y_cal[i]
  nonconformity_scores[i] <- 1 - probs_cal[i, true_class]
}

########### Schritt 4: Quantil für gewählte Schwelle berechnen (90 %-Abdeckung)
alpha <- 0.1  
q_hat <- quantile(nonconformity_scores, probs = 1 - alpha)


########### Schritt 5: Prediction Sets für Testdaten erzeugen
# Vorhersagen auf Testdaten
probs_test <- predict(model, newdata = X_test, type = "prob")

# Prediction Sets konstruieren
prediction_sets <- apply(probs_test, 1, function(p_row) {
  # Alle Klassen mit (1 - p) <= q_hat  ⇔  p >= 1 - q_hat
  names(p_row)[which(p_row >= 1 - q_hat)]
})

# Ausgabe
print(prediction_sets[[1]])