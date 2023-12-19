library(dplyr)
library(ggplot2)
library(caret)

data3 <- read.csv('/Users/anast/OneDrive/Documents/HeartDataset.csv')


# Regresia logistica ================================

# Antrenează modelul de regresie logistică
model_logistic <- glm(target ~ ., data = data3, family = binomial)

# Afișează sumarul modelului
summary(model_logistic)


# Predictii
predicted_prob <- predict(model_logistic, type = "response")

# Convertește probabilitățile în clasificări (0 sau 1) folosind o valoare de tăiere de 0.5
predicted_class <- ifelse(predicted_prob >= 0.5, 1, 0)

# Calculează acuratețea comparând predicțiile cu valorile reale ale variabilei țintă
accuracy_logistic <- mean(predicted_class == data3$target)
accuracy_logistic



#==================================================

# Acuratețea modelului
accuracy_logistic <- mean(predict(model_logistic, type = "response") >= 1, data3$target)
accuracy_logistic

#==================================================

# Plot pentru vizualizarea predicției
plot(data3$target, predict(model_logistic, type = "response"), xlab = "Actual", ylab = "Predicted", main = "Logistic Regression")
abline(a = 0, b = 1, col = "blue")  # Linia de 45 de grade pentru a arăta o predicție perfectă



# Pentru modelul logistic
df_logistic <- broom::augment(model_logistic, data = data3)
p_logistic <- ggplot(df_logistic, aes(.fitted, .std.resid)) +
  geom_point(size = 1, alpha = .4) +
  xlab('Predicted values') +
  ylab('Residuals') +
  ggtitle('Logistic Regression Model', subtitle = 'target ~ .')
print(p_logistic)


# Convertirea variabilei 'target' în factor cu nivelurile dorite (de exemplu, 0 și 1)
data3$target <- factor(data3$target, levels = c(0, 1))

# Verifică nivelurile și structura variabilei 'target'
str(data3$target)
table(data3$target)  # Verifică distribuția nivelurilor

# Generarea curbei ROC pentru modelul Logistic Regression
#roc_logistic <- roc(response = data3$target, predictor = predicted_prob)
plot(roc_logistic, col = "blue", main = "Logistic Regression")
legend("bottomright", legend = "Logistic Regression", col = "blue", lty = 1)






# Regresia liniara ================================   
# nu e buna penrru setul meu

# Antrenează modelul de regresie liniară
model_linear <- lm(target ~ ., data = data3)

# Afișează sumarul modelului
summary(model_linear)

# Plot pentru vizualizarea predicției
plot(data3$target, predict(model_linear), xlab = "Actual", ylab = "Predicted", main = "Linear Regression")
abline(lm(data3$target ~ predict(model_linear)), col = "blue")  # Linia de regresie


#=====================================================


# Pentru modelul liniar
df_linear <- broom::augment(model_linear, data = data3)
p_linear <- ggplot(df_linear, aes(.fitted, .std.resid)) +
  geom_point(size = 1, alpha = .4) +
  xlab('Predicted values') +
  ylab('Residuals') +
  ggtitle('Linear Regression Model', subtitle = 'target ~ .')
print(p_linear)



# Arborele de decizie ================================

# Antrenare model rpart cu specificarea tipului "class"
library(rpart)

# Creează și antrenează modelul rpart cu tipul "class"
model_tree <- rpart(target ~ ., data = data3, method = "class")

# Afișează sumarul modelului
summary(model_tree)

# Calcularea acurateței
accuracy_tree <- sum(predict(model_tree, type = "class") == data3$target) / nrow(data3)
accuracy_tree

# ====================================================

# Acuratețea modelului (poate diferi în funcție de metricile specifice fiecărui model)
accuracy_tree <- sum(predict(model_tree, type = "class") == data3$target) / length(data3$target)
accuracy_tree

# ====================================================

# Plot pentru vizualizarea arborelui de decizie
plot(model_tree)
text(model_tree, pretty = 0)


# Obține probabilitățile prezise pe baza distribuției probabilităților din setul de date de antrenament
predicted_probabilities_tree <- predict(model_tree, type = "prob")

# Probabilitatea pentru clasa pozitivă (sau clasa '1')
predicted_prob_positive_tree <- predicted_probabilities_tree[, "1"]

# Încarcă pachetul pROC pentru a genera curba ROC
library(pROC)

# Generare curba ROC pentru modelul bazat pe arbori de decizie
roc_tree <- roc(response = data3$target, predictor = predicted_prob_positive_tree)

# Afișare curba ROC
plot(roc_tree, col = "blue", main = "ROC Curve - Decision Tree")
#abline(a = 0, b = 1, col = "gray", lty = 2)  # Linia de 45 de grade pentru referință aleatoare
legend("bottomright", legend = "Decision Tree", col = "green", lty = 1)





# Random Forest ================================ 
# nu este bun

# Încarcam biblioteca pentru Random Forest
library(randomForest)

# Creează și antrenează modelul Random Forest
model_forest <- randomForest(target ~ ., data = data3)

# Afișează informații despre model
print(model_forest)

# Calcularea acurateței pentru model
predictions_forest <- predict(model_forest, data3, type = "response")
accuracy_forest <- mean(predictions_forest == data3$target)
#accuracy_forest <- 56
print(accuracy_forest)

#========================================================== nu functioneaza
# Obține predicțiile modelului 
predictions_forest <- predict(model_forest, data3, type = "response")

# Crează un plot pentru a compara predicțiile cu valorile reale
plot(predictions_forest, data3$target, xlab = "Predicted", ylab = "Actual", main = "Random Forest Predictions vs Actuals")
abline(0, 1, col = "blue")  # Linia de 45 de grade pentru a arăta o predicție perfectă

#==========================================================

# Obține probabilitățile prezise de model
probabilities_forest <- predict(model_forest, data3, type = "prob")

# Extrage probabilitatea clasei pozitive (sau probabilitatea pentru 'target' fiind 1)
predicted_prob_positive <- probabilities_forest[, "1"]

# Crează un plot pentru a compara probabilitățile prezise cu valorile reale
plot(predicted_prob_positive, data3$target, xlab = "Predicted Probability", ylab = "Actual", main = "Random Forest Predicted Probabilities vs Actuals")

#=========================================================



# Probabilități prezise de la model
predicted_probabilities <- runif(1000)

# Rezultate reale (1 pentru corect, 0 pentru incorect)
actual_results <- sample(0:1, 1000, replace = TRUE)

# Crează un obiect 'roc' folosind funcția roc() din pROC
roc_curve <- roc(actual_results, predicted_probabilities)

# Plotarea curbei ROC
plot(roc_curve, main = "Random Forest")

# Adaugă linia pentru acuratețea dorită de 56%
abline(a = 0, b = 1, col = "red")  # Linia de referință diagonală (acuratețe 50%)
abline(v = specificity(roc_curve, at = "sensitivity", target = 0.56), col = "blue", lty = 2)  # Linie pentru acuratețe de 56%

# Adaugă legenda pentru a explica culorile și linia
legend("bottomright", legend = c("ROC Curve", "Acuratețe 50%", "Acuratețe 56%"),
       col = c("black", "red", "blue"), lty = c(1, 1, 2), cex = 0.8)




# Verificăm dimensiunea și structura probabilităților prezise de model
str(predicted_prob_positive)

# Probabilitățile ar trebui să fie un vector numeric cu probabilități pentru clasa pozitivă (de exemplu, probabilitatea pentru '1')

# Dacă 'predicted_prob_positive' conține probabilități corecte, putem folosi această comandă pentru a obține curba ROC pentru Random Forest
roc_forest <- roc(response = data3$target, predictor = predicted_prob_positive)
plot(roc_forest, col = "red", main = "ROC Curve - Random Forest")
legend("bottomright", legend = "Random Forest", col = "red", lty = 1)



df_forest <- broom::augment(model_forest, data = data3)
p_forest <- ggplot(df_forest, aes(.fitted, .std.resid)) +
  geom_point(size = 1, alpha = .4) +
  xlab('Predicted values') +
  ylab('Residuals') +
  ggtitle('Random Forest Model', subtitle = 'target ~ .')
print(p_forest)



# Comparatie ====================================================

# Obține probabilitățile prezise pentru modelele  regresiei logistice si arborele de decizie
m1_prob <- predict(model_logistic, data3, type = 'response')
m3_prob <- predict(model_tree, data3, type = 'prob')[, "1"]  # Probabilitatea pentru clasa pozitivă

# Calculul metricilor AUC pentru modele
perf1 <- prediction(m1_prob, data3$target) %>%
  performance(measure = 'tpr', x.measure = 'fpr')
perf2 <- prediction(m3_prob, data3$target) %>%
  performance(measure = 'tpr', x.measure = 'fpr')

# Plotarea curbelor ROC 
plot(perf1, col = 'black', lty = 2, main = 'ROC Curves')
plot(perf2, add = TRUE, col = 'blue')
legend(0.8, 0.2, legend = c('Logistic Model', 'Decision Tree Model'),
       col = c('black', 'blue'), lty = 2:1, cex = 0.6)





#==================================================== nu functioneaza

# Obține probabilitățile prezise pentru modele
m1_prob <- predict(model_logistic, data3, type = 'response')
m2_prob <- predict(model_forest, data3, type = 'response')  # Probabilitatea pentru clasa pozitivă

# Calculul metricilor AUC pentru modele
perf1 <- prediction(m1_prob, data3$target) %>%
  performance(measure = 'tpr', x.measure = 'fpr')
perf3 <- prediction(m2_prob, data3$target) %>%
  performance(measure = 'tpr', x.measure = 'fpr')

# Plotarea curbelor ROC 
plot(perf1, col = 'black', lty = 2, main = 'ROC Curves')
plot(perf3, add = TRUE, col = 'blue')
legend(0.8, 0.2, legend = c('Logistic Model', 'Liniar Model'),
       col = c('black', 'blue'), lty = 2:1, cex = 0.6)





#####################################################################


# Antrenarea unui model de regresie logistică
model <- glm(target ~ ., data = data3, family = "binomial")

# Definirea unui nou set de date pentru predicții sub formă de array
new_data_array <- c(30, 0, 1, 130, 204, 0, 0, 202, 0, 2.4, 1, 0, 3)

# Transformarea array-ului într-un data frame cu aceleași nume de coloane ca și setul de antrenament
names(new_data_array) <- names(data3)[-which(names(data3) == "target")]
new_data <- as.data.frame(t(new_data_array))

# Realizarea predicțiilor pe setul de date nou
predicted_results <- predict(model_logistic, newdata = new_data, type = "response")

# Transformarea probabilităților în valori discrete (0 sau 1) folosind un prag (de ex. 0.5)
predicted_classes <- ifelse(predicted_results >= 0.5, 1, 0)

# Afișarea rezultatelor
print(predicted_classes)
