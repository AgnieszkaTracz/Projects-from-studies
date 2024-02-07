
#wczytanie danych
getwd()
penguins <- read.csv("penguins_size.csv", stringsAsFactors = TRUE)

#sprawdzenie podstawowych parametrów dotyczących danych
str(penguins)
summary(penguins)

#mamy 344 obserwacje 7 zmiennych
#do klasyfikacji użyjemy 6 cech

#dzięki funkcji summary widzimy, ile brakujących wartości jest
#w każdej kolumnie

#liczymy liczbę NA w każdym wierszu
na_in_row <- rowSums(is.na(penguins))

#zmienna j przechowuje numery wierszów, w których jest co najmniej jeden NA
j <- which(na_in_row > 0)

na_in_row[j]
#jak widać, w 8 wierszach jest jeden NA, natomiast w 2 wierszach jest aż 5 NA

#usunięcie wierszów, w których jest co najmniej jeden NA
penguins_set <- na.omit(penguins)
  
summary(penguins_set)

#pozbyliśmy się brakujących danych
#jednak w 1 wierszym zmienna sex ma wartość ".", co nie ma większego sensu
#usuwamy ten wiersz

d=which(penguins_set$sex == "." )
penguins_set <- penguins_set[-d, ]
summary(penguins_set)


#Podział danych na zbiór uczący i testowy w proporcji odpowiednio: 70% i 30%


RNGversion("3.5.2"); set.seed(123)
train_sample <- sample(333,230)


penguins_set_train <- penguins_set[train_sample,]
penguins_set_test <- penguins_set[-train_sample,]


#Sprawdzamy proporcje klas w utworzonych zbiorach 

#proporcje w wyjściowym zbiorze
prop.table(table(penguins_set$species))

#proporcje w zbiorze uczącym
prop.table(table(penguins_set_train$species))

#proporcje w zbiorze testowym
prop.table(table(penguins_set_test$species))


#proporcje we wszystkich zbiorach są zbliżone




#MODEL 1: DRZEWO DECYZYJNE

#Budowa modelu
library(C50)
model <- C5.0(penguins_set_train[-1], penguins_set_train$species)

#postawowe informacje o modelu
model
summary(model)

#graficzna ilustracja drzewa
plot(model)


#predykcja dla danych testowych 
penguins_set_pred <- predict(model, penguins_set_test)

#ocena modelu
library(gmodels)

CrossTable(penguins_set_test$species,penguins_set_pred, prop.chisq = FALSE, 
           prop.c = FALSE,prop.r = FALSE, 
           dnn = c('actual default', 'predicted default'))


(45+20+33)/103 #Dokładność wynosi około 95,14%


#MODYFIKACJA MODELU PRZY UŻYCIU BOOSTINGU

#parametr trials = 10

model_boost10 <- C5.0(penguins_set_train[-1], penguins_set_train$species, 
                      trials = 10)

#podstawowe informacje o modelu 
model_boost10
summary(model_boost10)


#predykcja
penguins_set_pred_boost10 <- predict(model_boost10, penguins_set_test)

#ocena modelu
CrossTable(penguins_set_test$species,penguins_set_pred_boost10, prop.chisq = FALSE, prop.c = FALSE,
           prop.r = FALSE, dnn = c('actual default', 'predicted default'))


(46+22+34) / 103 #Dokładność uległa poprawie: wynosi około 99%



#MODEL 2: NAIWNY KLASYFIKATOR BAYESA

#Budowa modelu
library(e1071)
Bayesmodel <- naiveBayes(penguins_set_train, penguins_set_train$species)

#predykcja
penguins_set_pred_Bayesmodel <- predict(Bayesmodel, penguins_set_test)

# Ocena modelu
CrossTable(penguins_set_test$species,penguins_set_pred_Bayesmodel, 
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, 
           dnn = c('actual default', 'predicted default'))




(46+23+34) / 103 #W tym przypadku dokładność jest 100%



#MODEL 3: KNN

library(class)

#Algorytm KNN wymaga odpowiedniego przygotowania danych

# w zbiorze danych są dwie zmienne jakościowe: płeć oraz wyspa
# zmieniamy je na wartości numeryczne za pomocą dummy coding


penguins_knn <- penguins_set

#kodowanie zmiennej sex
penguins_knn$MALE <- ifelse(penguins_set$sex == "MALE", 1, 0 )

#kodowanie zmiennej island
penguins_knn$Biscoe <- ifelse(penguins_set$island == "Biscoe", 1, 0 )
penguins_knn$Dream <- ifelse(penguins_set$island == "Dream", 1, 0 )


#usunięcie zmiennych sex i island
penguins_knn <- penguins_knn[,-c(2,7)]


#normalizacja zmiennych ilościowych

normalize <- function(x) {
  return( (x- min(x) ) / (max(x) - min(x) ) )
}
 
penguins_knn_n <- as.data.frame(lapply(penguins_knn[2:8], normalize))


#tworzenie zbioru treningowe i testowego                             
penguins_knn_n_train <- penguins_knn_n[train_sample,]
penguins_knn_n_test <- penguins_knn_n[-train_sample,]

#k=18

#predykcja za pomocą KNN dla k całkowitych z przedziału [1,30]

y = NULL
for(i in 1:30){
  knnmodel_pred <- knn(penguins_knn_n_train ,penguins_knn_n_test, 
                       penguins_set_train$species, k = i )
  
  w <- CrossTable(penguins_set_test$species, knnmodel_pred, prop.chisq = FALSE,
                  prop.c = FALSE,prop.r = FALSE, 
                  dnn = c('actual default', 'predicted default'))
  
  #interesuje nas tabela z liczbą poprawnie/błędnie sklasyfikowanych próbek
  #bez zbędnych proporcji etc.
  u = w$t

  #liczymy dokładność
  acc = sum(diag(u))/103
  y[i] = acc  
  }
x = 1:30

# Liczba w 1. kolumnie macierzy M oznacza k — ilość sąsiadów
# natomiast liczba kolumnie 2. oznacza dokładność uzyskaną dla takiego k
M = cbind(x, y)
M

#wniosek: dla mniejszych k skuteczność jest większa
#dobrym pomysłem będzie wzięcie np. k = 18
#(branie bardzo małych k nie jest wskazane)

knnmodel_pred <- knn(penguins_knn_n_train ,penguins_knn_n_test, 
                     penguins_set_train$species, k = 1 )

CrossTable(penguins_set_test$species, knnmodel_pred, prop.chisq = FALSE,
           prop.c = FALSE,prop.r = FALSE, 
           dnn = c('actual default', 'predicted default'))