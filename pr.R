library(openintro)
library(tidyverse)
library(base)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(gapminder)
library(corrplot)


data <- read.csv('/Users/anast/OneDrive/Documents/HeartDataset.csv')
head(data)

#transformare=====================================

# Curățarea datelor: eliminarea valorilor lipsă
set_de_date_curatat <- na.omit(data)

# Normalizarea datelor
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

set_de_date_normalizat <- as.data.frame(lapply(set_de_date_curatat, normalize))

#==================================================

glimpse(data)

dim(data)

#=================================================================
#corelatia dintre variabile

correlation_matrix <- cor(data)

corrplot(correlation_matrix, method="color")

#=================================================================

data %>%   
  distinct(age)

summary(data)

data %>% 
  count(age, fbs)

#contingency_table <- table(data$age, data$sex)
#View(contingency_table)

data_male <- data %>%
  dplyr::filter(sex == '1')

data_female <- data %>%
  dplyr::filter(sex == '0')

med <- median(data$chol)

data_updated <- data %>%
  mutate(chol_var = if_else (chol < med, 'below median', 'above median'))

data_new_updated <- data %>%
  mutate(fbs_cat = if_else(fbs == '0','no','yes'))

ggplot(data = data, aes(x = age, fill = chol)) +
  geom_bar(position = "dodge", stat = "count", fill = "#959DFF", color = "#959DFF", alpha = 0.5) +
  labs(title = "Count of Ages",
       x = "Age",
       y = "Count") +
  theme_minimal()

age_type <- data %>%
  mutate(age_type = case_when(
   age <= 40 ~ "young",
   age >= 41 & age <= 59 ~ "middle",
   age >= 60 ~ "old"
 ))

ggplot(data = data, aes(x=age, y=chol, color=NULL))+
  geom_point()

# Calculează proporțiile conjuncte
joint_proportions <- prop.table(table(data$sex, data$trestbps))

# Convertim rezultatul într-un dataframe
joint_proportions_df <- as.data.frame(joint_proportions)

# Setăm numele coloanelor
colnames(joint_proportions_df) <- c("Sex", "Tensiune arteriala", "Proportion")

# Afisăm tabelul cu proporțiile conjuncte
print(joint_proportions_df)

# Calculează proporțiile condiționale
conditional_proportions <- prop.table(table(data$sex, data$age), margin = 2)

# Convertim rezultatul într-un dataframe
conditional_proportions_df <- as.data.frame(conditional_proportions)

# Setăm numele coloanelor
colnames(conditional_proportions_df) <- c("Sex", "Age", "Proportion")

# Afisăm tabelul cu proporțiile condiționale
print(conditional_proportions_df)

# Pentru proporțiile conjuncte
ggplot(joint_proportions_df, aes(x = rownames(joint_proportions_df), y = Proportion, fill = rownames(joint_proportions_df))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Joint Proportions", x = "Sex and RestBS", y = "Proportion") +
  theme_minimal()

# Pentru proporțiile condiționale
ggplot(conditional_proportions_df, aes(x = rownames(conditional_proportions_df), y = Proportion, fill = rownames(conditional_proportions_df))) +
 geom_bar(stat = "identity", position = "dodge") +
labs(title = "Conditional Proportions (on columns)", x = "Sex and Age", y = "Proportion") +
theme_minimal()

##########################################################################


# Calculează numărul de bărbați și femei din setul de date
counts <- table(data$sex)

# Calculează procentul pentru fiecare categorie
percent <- prop.table(counts) * 100

# Creează un pie chart pentru distribuția procentuală a bărbaților și femeilor
pie_data <- data.frame(
  sex = names(counts),
  count = counts,
  percent = percent
)

palette <- c("#B48ACC", "#7A86F9")

ggplot(pie_data, aes(x = "", y = percent, fill = sex)) +
  geom_bar(stat = "identity", width = 1) +
  geom_text(aes(label = paste0(round(percent, 1), "%")), position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = palette) + # Setarea  culorilor definite mai sus
  coord_polar("y", start = 0) +
  labs(
    fill = "Sex",
    x = NULL,
    y = NULL,
    title = "Distribuția procentuală a bărbaților și femeilor"
  ) +
  theme_void() +
  theme(legend.position = "bottom")



heart_disease <- subset(data, target == 1)

# Calculează numărul de bărbați și femei cu boli de inimă
counts <- table(heart_disease$sex)

# Calculează procentul pentru fiecare categorie (bărbați și femei)
percent <- prop.table(counts) * 100

# Creează un pie plot pentru distribuția în procente a bărbaților și femeilor cu boli de inimă
pie_data <- data.frame(
  sex = names(counts),
  count = counts,
  percent = percent
)

# Definirea paletelor de culori pentru bărbați și femei
palette <- c("#F48ACC", "#7A86F9") # Culori pentru bărbați și femei

ggplot(pie_data, aes(x = "", y = percent, fill = sex)) +
  geom_bar(stat = "identity", width = 1) +
  geom_text(aes(label = paste0(round(percent, 1), "%")), position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = palette) + # Setarea paletelor de culori definite mai sus
  coord_polar("y", start = 0) +
  labs(
    fill = "Sex",
    x = NULL,
    y = NULL,
    title = "Distribuția în procente a bărbaților și femeilor cu boli de inimă"
  ) +
  theme_void() +
  theme(legend.position = "bottom")





# Crează histograma pentru variabila 'age'
ggplot(data, aes(x = age)) +
  geom_histogram(binwidth = 5, fill = "#1f77b4", color = "white") +
  labs(
    x = "Age",
    y = "Frequency",
    title = "Distribution of Age"
  ) +
  theme_minimal()

# Crează grafic de bare pentru variabila 'sex'
ggplot(data, aes(x = factor(sex))) +
  geom_bar(fill = "#aec7e8", color = "black") +
  labs(
    x = "Sex",
    y = "Count",
    title = "Distribution of Gender"
  ) +
  theme_minimal()


####################################################################


# Histograma cu binwidth de 3
ggplot(data, aes(x = thalach)) +
  geom_histogram(binwidth = 3, fill = "blue", color = "white", alpha = 0.3) +
  labs(title = "Histogram of max cardiac rithm", x = "Ritmul cardiac maxim atins", y = "Frecventa") +
  theme_minimal()

# Construiește un boxplot pentru variabila price
ggplot(data, aes(y = chol)) +
  geom_boxplot() +
  labs(title = "Boxplot of Cholesterol", y = "Cholesterol") +
  theme_minimal()

# Creează un nou set de date care exclude valorile extreme mari
chol_limit <- filter(data, chol < 500)

# Construiește un boxplot pentru variabila price, folosind setul de date redus
ggplot(chol_limit, aes(y = chol)) +
  geom_boxplot() +
  labs(title = "Boxplot of Cholesterol ( < 500 )", y = "Cholesterol") +
  theme_minimal()

compare_boxplots <- function(data, chol_limit) {
  # Crează primul boxplot
  plot1 <- ggplot(data, aes(y = chol)) +
    geom_boxplot() +
    labs(title = "Boxplot of Cholesterol", y = "Cholesterol") +
    theme_minimal()
  
  # Crează al doilea boxplot
  plot2 <- ggplot(chol_limit, aes(y = chol)) +
    geom_boxplot() +
    labs(title = "Boxplot of Cholesterol ( < 500 )", y = "Cholesterol") +
    theme_minimal()
  
  # Afișează ploturile într-o singură imagine
  grid.arrange(plot1, plot2, ncol = 2)
}

# Apelul funcției cu seturile de date specifice
compare_boxplots(data, chol_limit)

#prescurtat

# Compară cele două boxplot-uri
grid.arrange(
  ggplot(data, aes(y = chol)) +
    geom_boxplot() +
    labs(title = "Boxplot of Cholesterol", y = "Cholesterol") +
    theme_minimal(),
  
  ggplot(chol_limit, aes(y = chol)) +
    geom_boxplot() +
    labs(title = "Boxplot of Cholesterol ( < 250 )", y = "Cholesterol") +
    theme_minimal(),
  ncol = 2
)

# Create faceted histogram
ggplot(data, aes(x = chol)) +
  geom_histogram(binwidth = 10, fill = "red", color = "red", alpha = 0.3) +
  labs(title = "Glicemia",
       x = "Glicemia",
       y = "Frecventa") +
  facet_wrap(~ sex, scales = "free_x", ncol = 2) +
  theme_minimal()

#Histograma cu 4 ploturi

data_filtered <- data %>%
  filter(trestbps < 200)    #nu e bine

gg <- data_filtered %>%
  ggplot()

gg <- gg +
  aes(x = trestbps) +
  facet_wrap(~ cp, scales = "free_x") 

gg <- gg +
  geom_histogram(binwidth = 7, fill = "blue", color = "black", alpha = 0.5) +
  labs(title = "Tipul durerii in piept (intensitatea)",
       x = "Tensiunea arteriala",
       y = "Frecventa")

print(gg)


#Histograma cu 4 ploturi    actuala

data_filtered1 <- data %>%
  filter(target == 1)

gg <- data_filtered1 %>%
  ggplot()

gg <- gg +
  aes(x = target) +
  facet_wrap(~ cp, scales = "free_x") 

gg <- gg +
  geom_histogram(binwidth = 7, fill = "blue", color = "black", alpha = 0.5) +
  labs(title = "Tipul durerii in piept (intensitatea)",
       x = "Target",
       y = "Frecventa")

print(gg)




#lab3 =============================================================

data1 <- data.frame(data$oldpeak)

data1 <- data1 %>%
  mutate(exist = if_else(data$oldpeak %in% c("0", "1.6", "1.8", "2.6", "3"), TRUE, FALSE))

print(data1)



#===============================================================

#rm(gap1)

gap1 <- data.frame(data$restecg)
gap1 <- gap1 %>%
  mutate(target = if_else(data$target %in% c("1"), TRUE, FALSE))

gap1_summary <- gap1 %>%
  group_by(data.restecg) %>%
  summarise(media = mean(target, na.rm = TRUE),
            mediana = median(target, na.rm = TRUE))

print(gap1_summary)

ggplot(gap1_summary, aes(x = mediana, y = data.restecg, fill = data.restecg)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Boxplot ",
       x = "target",
       y = "ecg")

#nu e finisat mai sus


# Creează un data frame
date <- data.frame(
  Grup = rep(c("Tensiunea arteriala", "Colesterol"), each = 5),
  Valoare = c(data$age, data$trestbps)
)

# Creează un boxplot comparativ
ggplot(date, aes(x = Grup, y = Valoare, fill = Grup)) +
  geom_boxplot() +
  labs(title = "Boxploturi comparative", x = "Grup", y = "Valoare")


#sau


date %>%
  group_by(Grup) %>%
  ggplot(aes(x = Grup, y = Valoare, fill = Grup)) +
  geom_boxplot() +
  labs(title = "Boxploturi comparative", x = "Grup", y = "Valoare")



ggplot(data, aes(x = factor(target), y = thalach, fill = factor(target))) +
  geom_boxplot() +
  labs(title = "Box Plot of Maximum Heart Rate by Target",
       x = "Target",
       y = "Maximum Heart Rate") +
  scale_fill_manual(values = c("#D7BDE2", "#B5A1FA")) +
  theme_minimal()




#ex 2

# dispersie
spread_summary <- data %>%
  group_by(age) %>%
  summarise(
    sd_target = sd(target, na.rm = TRUE),
    IQR_target = IQR(target, na.rm = TRUE),
    num_patients = n()
  )

print(spread_summary)

# Frecventa ritmului cardiac maxim atins
ggplot(data, aes(x = thalach, fill = target)) +
  geom_density(alpha = 0.3) +
  labs(title = "Density Plot of target by max cardiac rithm",
       x = "Ritmul cardiac maxim atins") +
  scale_fill_discrete(name = "Target")



ggplot(data, aes(x = age, fill = target)) +
  geom_density(alpha = 0.3) +
  labs(title = "Density Plot of target by age",
       x = "Age") +
  scale_fill_discrete(name = "Target")

#  theme_minimal()     # nu functioneaza

# sau

ggplot(age_type, aes(x = age_type, fill = target)) +
  geom_bar(alpha = 0.3, fill = "green") +
  labs(title = "BarPlot of target by age",
       x = "Age") +
  scale_fill_discrete(name = "Target")



#ex 3

data2 <- data %>%
  filter(sex == "1")

# centru și dispersie
center_and_spread <- data2 %>%
  summarise(
    mean_lifeExp = mean(restecg, na.rm = TRUE),
    median_lifeExp = median(restecg, na.rm = TRUE),
    sd_lifeExp = sd(restecg, na.rm = TRUE),
    IQR_lifeExp = IQR(restecg, na.rm = TRUE)
  )

print(center_and_spread)


#ex 4


ggplot(data, aes(x = thalach, fill = sex)) +
  geom_boxplot(alpha = 0.3) +
  labs(title = "Density Plot al ritmului cardiac maxim atins dupa sex",
       x = "Ritmul cardiac maxim")


ggplot(age_type, aes(x = age_type, fill = target)) +
  geom_density(alpha = 0.3) +
  labs(title = "BarPlot of target by age",
       x = "Age") +
  scale_fill_discrete(name = "Target")



#====================================================

data_log <- data %>%
  mutate(log_chol = log(chol))

# Grafic de densitate pentru variabila transformată "log_chol"
ggplot(data_log, aes(x = log_chol)) +
  geom_density() +
  labs(title = "Density Plot of Log-Transformed Cholesterol",
       x = "Log Cholesterol")


#====================================================

#ex 5

# Filter out observations
gap_ca <- data %>%
  filter(target == "1")

# Create a new variable named is_outlier using mutate()
gap_ca <- gap_ca %>%
  mutate(is_outlier = if_else(chol > 500, TRUE, FALSE))

# Use the is_outlier variable to filter out the observations flagged as outliers
gap_ca_no_outlier <- gap_ca %>%
  filter(!is_outlier)

# Create a boxplot of the distribution of life expectancy with the outlier removed
ggplot(gap_ca_no_outlier, aes(x = "", y = chol)) +
  geom_boxplot() +
  labs(title = "Boxplot of Cholesterol for Target 1 (Outliers Removed)",
       x = "",
       y = "Cholesterol")

