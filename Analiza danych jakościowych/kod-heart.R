
library(ggplot2)
library(gplots)
library(gridExtra)
library(vcd)
library(lmtest)

dane <- read.csv("C:/Users/ŁosiowaAga/Desktop/ADJ/heart/heart_failure_clinical_records_dataset.csv")
summary(dane)

dane$anaemia <- as.factor(dane$anaemia)
dane$diabetes <- as.factor(dane$diabetes)
dane$high_blood_pressure <- as.factor(dane$high_blood_pressure)
dane$sex <- as.factor(dane$sex)
dane$smoking <- as.factor(dane$smoking)
dane$DEATH_EVENT <- as.factor(dane$DEATH_EVENT)

summary(dane)


# Tworzenie wykresów
p1 <- ggplot(dane, aes(x = anaemia, fill = as.factor(DEATH_EVENT))) +
  geom_bar(position = "dodge", color = "white") +
  scale_fill_manual(values = c("grey", "blue")) +
  ggtitle("Wykres w zależności od obecności anemii i DEATH_EVENT")

p2 <- ggplot(dane, aes(x = diabetes, fill = as.factor(DEATH_EVENT))) +
  geom_bar(position = "dodge", color = "white") +
  scale_fill_manual(values = c("grey", "blue")) +
  ggtitle("Wykres w zależności od obecności cukrzycy i DEATH_EVENT")

p3 <- ggplot(dane, aes(x = smoking, fill = as.factor(DEATH_EVENT))) +
  geom_bar(position = "dodge", color = "white") +
  scale_fill_manual(values = c("grey", "blue")) +
  ggtitle("Wykres w zależności od palenia tytoniu i DEATH_EVENT")

p4 <- ggplot(dane, aes(x = sex, fill = as.factor(DEATH_EVENT))) +
  geom_bar(position = "dodge", color = "white") +
  scale_fill_manual(values = c("grey", "blue")) +
  ggtitle("Wykres w zależności od płci i DEATH_EVENT")

p5 <- ggplot(dane, aes(x = high_blood_pressure, fill = as.factor(DEATH_EVENT))) +
  geom_bar(position = "dodge", color = "white") +
  scale_fill_manual(values = c("grey", "blue")) +
  ggtitle("Wykres w zależności od nadciśnienia i DEATH_EVENT")

p6 <- ggplot(dane, aes(x = age, fill = as.factor(DEATH_EVENT))) +
  geom_histogram(position = "dodge", bins = 30, alpha = 0.7, color = "white") +
  scale_fill_manual(values = c("grey", "blue")) +
  ggtitle("Histogram w zależności od wieku i DEATH_EVENT")

p7 <- ggplot(dane, aes(x = creatinine_phosphokinase, fill = as.factor(DEATH_EVENT))) +
  geom_histogram(position = "dodge", bins = 30, alpha = 0.7, color = "white") +
  scale_fill_manual(values = c("grey", "blue")) +
  ggtitle("Histogram w zależności od poziomu CPK i DEATH_EVENT")

p8 <- ggplot(dane, aes(x = ejection_fraction, fill = as.factor(DEATH_EVENT))) +
  geom_histogram(position = "dodge", bins = 30, alpha = 0.7, color = "white") +
  scale_fill_manual(values = c("grey", "blue")) +
  ggtitle("Histogram w zależności od frakcji wyrzutowej i DEATH_EVENT")

p9 <- ggplot(dane, aes(x = platelets, fill = as.factor(DEATH_EVENT))) +
  geom_histogram(position = "dodge", bins = 30, alpha = 0.7, color = "white") +
  scale_fill_manual(values = c("grey", "blue")) +
  ggtitle("Histogram w zależności od liczby płytek krwi i DEATH_EVENT")

p10 <- ggplot(dane, aes(x = serum_creatinine, fill = as.factor(DEATH_EVENT))) +
  geom_histogram(position = "dodge", bins = 30, alpha = 0.7, color = "white") +
  scale_fill_manual(values = c("grey", "blue")) +
  ggtitle("Histogram w zależności od poziomu kreatyniny we krwi i DEATH_EVENT")

p11 <- ggplot(dane, aes(x = serum_sodium, fill = as.factor(DEATH_EVENT))) +
  geom_histogram(position = "dodge", bins = 30, alpha = 0.7, color = "white") +
  scale_fill_manual(values = c("grey", "blue")) +
  ggtitle("Histogram w zależności od poziomu sodu we krwi i DEATH_EVENT")

p12 <- ggplot(dane, aes(x = time, fill = as.factor(DEATH_EVENT))) +
  geom_histogram(position = "dodge", bins = 30, alpha = 0.7, color = "white") +
  scale_fill_manual(values = c("grey", "blue")) +
  ggtitle("Histogram w zależności od czasu obserwacji i DEATH_EVENT")

# Wyświetlenie wykresów
grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, ncol = 3)


#Analiza tablic kontyngencji: 

#1 
tab_kontyngencji_anaemia <- table(dane$DEATH_EVENT, dane$anaemia)
rownames(tab_kontyngencji_anaemia) <- c("Brak zgonu", "Zgon")
colnames(tab_kontyngencji_anaemia) <- c("Brak anemii", "Anemia")
print(tab_kontyngencji_anaemia)
balloonplot(tab_kontyngencji_anaemia, dotcolor = "gray80")
assocstats(tab_kontyngencji_anaemia)
#Nie ma zależności

#2
tab_kontyngencji_high_blood_pressure <- table(dane$DEATH_EVENT, dane$high_blood_pressure)
rownames(tab_kontyngencji_high_blood_pressure) <- c("Brak zgonu", "Zgon")
colnames(tab_kontyngencji_high_blood_pressure) <- c("Brak obecności nadciśnienia", "Obecność nadciśnienia")
print(tab_kontyngencji_high_blood_pressure)
balloonplot(tab_kontyngencji_high_blood_pressure, dotcolor = "gray80")
assocstats(tab_kontyngencji_high_blood_pressure)
#Nie ma zależności

#3
tab_kontyngencji_diabetes <- table(dane$DEATH_EVENT, dane$diabetes)
rownames(tab_kontyngencji_diabetes) <- c("Brak zgonu", "Zgon")
colnames(tab_kontyngencji_diabetes) <- c("Cukrzyca", "Brak cukrzycy")
print(tab_kontyngencji_diabetes)
balloonplot(tab_kontyngencji_diabetes, dotcolor = "gray80")
assocstats(tab_kontyngencji_diabetes)
#Nie ma zależności

#4 
tab_kontyngencji_sex <- table(dane$DEATH_EVENT, dane$sex)
rownames(tab_kontyngencji_sex) <- c("Brak zgonu", "Zgon")
colnames(tab_kontyngencji_sex) <- levels(dane$sex)
print(tab_kontyngencji_sex)
balloonplot(tab_kontyngencji_sex, dotcolor = "gray80")
assocstats(tab_kontyngencji_sex)
#Nie ma zależności 

#5
tab_kontyngencji_smoking <- table(dane$DEATH_EVENT, dane$smoking)
rownames(tab_kontyngencji_smoking) <- c("Brak zgonu", "Zgon")
colnames(tab_kontyngencji_smoking) <- levels(dane$smoking)
print(tab_kontyngencji_smoking)
balloonplot(tab_kontyngencji_smoking, dotcolor = "gray80")
assocstats(tab_kontyngencji_smoking)

#6
tab_kontyngencji_creatinine <- table(dane$DEATH_EVENT, cut(dane$creatinine_phosphokinase, breaks = c(0, 200, 400, 600, 800, Inf)))
rownames(tab_kontyngencji_creatinine) <- c("Brak zgonu", "Zgon")
colnames(tab_kontyngencji_creatinine) <- c("0-200", "201-400", "401-600", "601-800", "800+")
print(tab_kontyngencji_creatinine)
balloonplot(tab_kontyngencji_creatinine, dotcolor = "gray80")
assocstats(tab_kontyngencji_creatinine)
# Nie ma zależności 

#7 
tab_kontyngencji_ejection <- table(dane$DEATH_EVENT, cut(dane$ejection_fraction, breaks = c(0, 20, 40, 60, 80, Inf)))
rownames(tab_kontyngencji_ejection) <- c("Brak zgonu", "Zgon")
colnames(tab_kontyngencji_ejection) <- c("0-20", "21-40", "41-60", "61-80", "80+")
print(tab_kontyngencji_ejection)
balloonplot(tab_kontyngencji_ejection, dotcolor = "gray80")
assocstats(tab_kontyngencji_ejection)
#możliwa zależność

#8
tab_kontyngencji_platelets <- table(dane$DEATH_EVENT, cut(dane$platelets, breaks = c(0, 100000, 200000, 300000, 400000, Inf)))
rownames(tab_kontyngencji_platelets) <- c("Brak zgonu", "Zgon")
colnames(tab_kontyngencji_platelets) <- c("0-100,000", "100,001-200,000", "200,001-300,000", "300,001-400,000", "400,000+")
print(tab_kontyngencji_platelets)
balloonplot(tab_kontyngencji_platelets, dotcolor = "gray80")
assocstats(tab_kontyngencji_platelets)
#nie ma zależności

#9 
tab_kontyngencji_serum_creatinine <- table(dane$DEATH_EVENT, cut(dane$serum_creatinine, breaks = c(0, 1, 2, 3, 4, Inf)))
rownames(tab_kontyngencji_serum_creatinine) <- c("Brak zgonu", "Zgon")
colnames(tab_kontyngencji_serum_creatinine) <- c("0-1", "1-2", "2-3", "3-4", "4+")
print(tab_kontyngencji_serum_creatinine)
balloonplot(tab_kontyngencji_serum_creatinine, dotcolor = "gray80")
assocstats(tab_kontyngencji_serum_creatinine)
# jest zależność


#10
tab_kontyngencji_serum_sodium <- table(dane$DEATH_EVENT, cut(dane$serum_sodium, breaks = c(0, 130, 135, 140, 145, Inf)))
rownames(tab_kontyngencji_serum_sodium) <- c("Brak zgonu", "Zgon")
colnames(tab_kontyngencji_serum_sodium) <- c("0-130", "131-135", "136-140", "141-145", "145+")
print(tab_kontyngencji_serum_sodium)
balloonplot(tab_kontyngencji_serum_sodium, dotcolor = "gray80")
assocstats(tab_kontyngencji_serum_sodium)
#jest zależność

#11
tab_kontyngencji_time <- table(dane$DEATH_EVENT, cut(dane$time, breaks = c(0, 50, 100, 150, 200, 250, 300, Inf)))
rownames(tab_kontyngencji_time) <- c("Brak zgonu", "Zgon")
colnames(tab_kontyngencji_time) <- c("0-50", "51-100", "101-150", "151-200", "201-250", "251-300", "300+")
print(tab_kontyngencji_time)
balloonplot(tab_kontyngencji_time, dotcolor = "gray80")
assocstats(tab_kontyngencji_time)
# jest zależność


#12
tab_kontyngencji_age <- table(dane$DEATH_EVENT, cut(dane$age, breaks = c(40, 50, 60, 70, 80, 90, 100, Inf)))
rownames(tab_kontyngencji_age) <- c("Brak zgonu", "Zgon")
colnames(tab_kontyngencji_age) <- c("40-50", "51-60", "61-70", "71-80", "81-90", "91-100", "100+")
print(tab_kontyngencji_age)
balloonplot(tab_kontyngencji_age, dotcolor = "gray80")
assocstats(tab_kontyngencji_age)


#Iloraz szans dla zmiennych istotnych:

#serum_creatinine
print(tab_kontyngencji_serum_creatinine)

prop_serum_creatinine <-prop.table(tab_kontyngencji_serum_creatinine,2)
prop_serum_creatinine

szansa_serum_creatinine=prop_serum_creatinine/(1-prop_serum_creatinine) # szanse na śmierć
szansa_serum_creatinine
szansa_serum_creatinine[,1]/szansa_serum_creatinine[,2] # ilorazy szans 
szansa_serum_creatinine[,2]/szansa_serum_creatinine[,3]
szansa_serum_creatinine[,3]/szansa_serum_creatinine[,4]
szansa_serum_creatinine[,4]/szansa_serum_creatinine[,5]
szansa_serum_creatinine[,1]/szansa_serum_creatinine[,5]


#serum_sodium
print(tab_kontyngencji_serum_sodium)

prop_serum_sodium <-prop.table(tab_kontyngencji_serum_sodium,2)
prop_serum_sodium

szansa_serum_sodium=prop_serum_creatinine/(1-prop_serum_creatinine) # szanse na śmierć
szansa_serum_sodium
szansa_serum_sodium[,1]/szansa_serum_sodium[,2] # ilorazy szans 
szansa_serum_sodium[,2]/szansa_serum_sodium[,3]
szansa_serum_sodium[,3]/szansa_serum_sodium[,4]
szansa_serum_sodium[,4]/szansa_serum_sodium[,5]
szansa_serum_sodium[,1]/szansa_serum_sodium[,5]
szansa_serum_sodium[,1]/szansa_serum_sodium[,3]



#age
print(tab_kontyngencji_age)
prop_age <-prop.table(tab_kontyngencji_age,2)
prop_age

szansa_age=prop_age/(1-prop_age) # szanse na śmierć
szansa_age
szansa_age[,1]/szansa_age[,2] # ilorazy szans 
szansa_age[,2]/szansa_age[,3] # ilorazy szans 
szansa_age[,3]/szansa_age[,4] # ilorazy szans 
szansa_age[,4]/szansa_age[,5] # ilorazy szans 


#time
print(tab_kontyngencji_time)
prop_time <-prop.table(tab_kontyngencji_age,2)
prop_time

szansa_time=prop_time/(1-prop_time) # szanse na śmierć
szansa_time
szansa_time[,1]/szansa_time[,2]
szansa_time[,2]/szansa_time[,3]
szansa_time[,3]/szansa_time[,4]
szansa_time[,4]/szansa_time[,5]
szansa_time[,1]/szansa_time[,5]


#ejection
print(tab_kontyngencji_ejection)
prop_ejection <-prop.table(tab_kontyngencji_ejection,2)
prop_ejection

szansa_ejection=prop_ejection/(1-prop_ejection) # szanse na śmierć
szansa_ejection
szansa_ejection[,1]/szansa_ejection[,2]
szansa_ejection[,2]/szansa_ejection[,1]
szansa_ejection[,2]/szansa_ejection[,3]
szansa_ejection[,3]/szansa_ejection[,4]
szansa_ejection[,1]/szansa_ejection[,4]




###########################
#Modele GLM

logit1 <- glm(DEATH_EVENT ~.,dane,family=binomial)
summary(logit1)

logit2 <- glm(DEATH_EVENT ~ age + ejection_fraction  + serum_creatinine + time + serum_sodium, dane, family=binomial)
summary(logit2)

logit3 <- glm(DEATH_EVENT ~ age + ejection_fraction  + serum_creatinine + time, dane, family=binomial)
summary(logit3)

logit4 <- glm(DEATH_EVENT ~ age + ejection_fraction  +  time, dane, family=binomial)
summary(logit4)

logit1.1 <-  glm(DEATH_EVENT ~.^2 ,dane,family=binomial)# interakcje rzędu 2.
summary(logit1.1)

logit2.1 <- glm(DEATH_EVENT ~ (age + ejection_fraction  + serum_creatinine + time + serum_sodium)^2, dane, family=binomial)
summary(logit2.1)

logit2.2 <- glm(DEATH_EVENT ~ (ejection_fraction:time) + (ejection_fraction:serum_sodium), dane, family=binomial)
summary(logit2.2)

logit2.3 <- glm(DEATH_EVENT ~ (ejection_fraction:time) + (ejection_fraction:serum_sodium) + ejection_fraction, dane, family=binomial)
summary(logit2.3)

logit1.3 <-   glm(DEATH_EVENT ~.^3 ,dane,family=binomial)# interakcje rzędu 3.
summary(logit1.3)



wyniki<- AIC(logit1,logit2,logit3, logit4,logit1.1,logit2.1,logit2.2, logit2.3,logit1.3)
wyniki

summary(logit2)


logitNULL <- glm(DEATH_EVENT ~ 1 , family = binomial , dane)
lrtest(logitNULL,logit2)
anova(logitNULL,logit2,test="Chisq")

anova(logit1,logit2,test="Chisq")
anova(logit2,logit2.1,test="Chisq")
anova(logit2,logit3,test="Chisq")


# model z samymi zmiennymi jakościowymi:
logitwplyw <- glm(DEATH_EVENT ~ anaemia + diabetes + high_blood_pressure + sex + smoking,dane,family=binomial)
summary(logitwplyw)


#Predykcja na zbiorze testowym
set.seed(2)
sample_indices <- sample(1:nrow(dane), 180)
train_data <- dane[sample_indices, ]
test_data <- dane[-sample_indices, ]

logit2 <- glm(DEATH_EVENT ~ age + ejection_fraction  + serum_creatinine + time + serum_sodium, data = train_data, family=binomial)
predictions <- predict(logit2, newdata = test_data, type = "response")
predicted_labels2 <- ifelse(predictions > 0.5, 1, 0)
results_table2 <- data.frame(Actual = as.factor(test_data$DEATH_EVENT), Predicted = as.factor(predicted_labels2))
conf_matrix2 <- table(Przewidziane = results_table2$Predicted, Aktualne = results_table2$Actual)
conf_matrix2
accuracy2 <- sum(diag(conf_matrix2)) / sum(conf_matrix2)
print(paste("Dokładność modelu:", round(accuracy2, 4)))


logit1 <- glm(DEATH_EVENT ~.,family=binomial,data = train_data)
predictions <- predict(logit1, newdata = test_data, type = "response")
predicted_labels1 <- ifelse(predictions > 0.5, 1, 0)
results_table1 <- data.frame(Actual = as.factor(test_data$DEATH_EVENT), Predicted = as.factor(predicted_labels1))
conf_matrix1 <- table(Przewidziane = results_table1$Predicted, Aktualne = results_table1$Actual)
conf_matrix1
accuracy1 <- sum(diag(conf_matrix1)) / sum(conf_matrix1)
print(paste("Dokładność modelu:", round(accuracy1, 4)))



logit3 <- glm(DEATH_EVENT ~ age + ejection_fraction  + serum_creatinine + time, data = train_data, family=binomial)
predictions <- predict(logit3, newdata = test_data, type = "response")
predicted_labels3 <- ifelse(predictions > 0.5, 1, 0)
results_table3 <- data.frame(Actual = as.factor(test_data$DEATH_EVENT), Predicted = as.factor(predicted_labels3))
conf_matrix3 <- table(Przewidziane = results_table3$Predicted, Aktualne = results_table3$Actual)
conf_matrix3
accuracy3 <- sum(diag(conf_matrix3)) / sum(conf_matrix3)
print(paste("Dokładność modelu:", round(accuracy3, 4)))

logit2.1 <- glm(DEATH_EVENT ~ (age + ejection_fraction  + serum_creatinine + time + serum_sodium)^2, data = train_data, family=binomial)
predictions <- predict(logit2.1, newdata = test_data, type = "response")
predicted_labels2.1 <- ifelse(predictions > 0.5, 1, 0)
results_table2.1 <- data.frame(Actual = as.factor(test_data$DEATH_EVENT), Predicted = as.factor(predicted_labels2.1))
conf_matrix2.1 <- table(Przewidziane = results_table2.1$Predicted, Aktualne = results_table2.1$Actual)
conf_matrix2.1

accuracy2.1 <- sum(diag(conf_matrix2.1)) / sum(conf_matrix2.1)
print(paste("Dokładność modelu:", round(accuracy2.1, 4)))

logit2.3 <- glm(DEATH_EVENT ~ (ejection_fraction:time) + (ejection_fraction:serum_sodium) + ejection_fraction, dane, family=binomial)
predictions <- predict(logit2.3, newdata = test_data, type = "response")
predicted_labels2.3 <- ifelse(predictions > 0.5, 1, 0)
results_table2.3 <- data.frame(Actual = as.factor(test_data$DEATH_EVENT), Predicted = as.factor(predicted_labels2.3))
conf_matrix2.3 <- table(Przewidziane = results_table2.3$Predicted, Aktualne = results_table2.3$Actual)
conf_matrix2.3

accuracy2.3 <- sum(diag(conf_matrix2.3)) / sum(conf_matrix2.3)
print(paste("Dokładność modelu:", round(accuracy2.3, 4)))


wyniki2 <- c(accuracy1,"logit1",accuracy2,"logit2",accuracy3,"logit3",accuracy2.1,"logit2.1") 
wyniki2



