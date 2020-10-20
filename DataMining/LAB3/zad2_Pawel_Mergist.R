##############################################################
# ZADANIE: #2 Stworz model klasyfikacji dla zbioru danych: wines dane i opis dostepne pod
# adresem: http://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/
##############################################################

# Analiza danych ze zbioru wine-quality
#
#
# Ponizej beda trzy sekcje z uzyciem klasyfikatorow:
#  C5.0
#  rpart
#  NaiveBayes
# 
#  Przeprowadzam eksperymenty uzywajac wszystkich atrybutow, potem ich ilosc zmniejszam tak
#  aby zapewnic maksymalna sprawnosc i minimalna zlozonosc.
#
#  Dla kazdego eksperymentu rysuje drzewo (jesli implementacja pozwala), krzywe ROC i Lift. 
#  Dodatkowo zbieram wszystkie wyniki i opisy w ramce results.
#
#  Kod mozna uruchomic w calosci, przejrzec diagramy oraz wykresy od konca, a takze porownac z ramka results.
#  Ramke mozna posortowac np. wedlug BalancedAccuracy i porownac jaka sprawnosc mialy modele. 
#
#  Stosunkowo najprostszy i najlepszy okazal sie klasyfikator C5.0 uzywajacy 2 atrybutow - 
#  ramka resutls try_num = 5 i AUC: 0.994 na zbiorze testowym. 
#
#  Wartosci AUC - nastepnym razem zbiore takze w ramce results - w tej wersji dostepne da na wykresie ROC.
#


# Zrodla: O rpart - wyswietlanie drzew, http://www.milbo.org/rpart-plot/prp.pdf


library(gmodels) #bib. do analizy wynik???w
library(Hmisc) #bib. do analizy wynik???w
library(party) #bib. zawieraj???cca ctree()
library(caret) #bib. do reprezentacji graficznej
library(rpart) #bib. zawieraj???ca rpart()
library(rpart.plot) 
library(e1071) #bib. zawieraj???ca klasyfik. Bayesa
library(C50) #bib. zawieraj???ca klasyfik. C5.0
library(pROC)

install.packages('ROCR')
library(ROCR)

options(digits=3)


set.seed(71)

# ZALADUJ DANE:

download.file('http://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv', 'wine_red.csv');
download.file('http://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv', 'wine_white.csv');

##
## FUNKCJE POMOCNICZE
##

# Funkcja do podzialu na zbior treningowy i testowy
split_train_test <- function(wines, ratio=0.7)
{
  train_sample = wines
  dim(wines)
  ## z przykladu
  set.seed(123)
  nmbOfWines = dim(wines)[1]
  train_sample = sample(nmbOfWines, nmbOfWines*ratio)
  
  train = wines[train_sample,]
  test = wines[-train_sample,]
  return (list("train"= train, "test" = test))
}
# Pokazanie tablicy pomylek i statystyk modelu
model_stats <- function(mod, test)
{
  pred<-predict(mod, test, type='class')
  cm = confusionMatrix(test$class, pred)
  
  print (cm$table)
  print (cm$overall)
  print (cm$byClass) 
  
  return (cm)
}

# Narysowanie krzywych ROC I lift 
draw_roc_lift <- function (rpTree, test, text, pred_result_type='prob')
{
  predSexProb = predict(rpTree,test, type=pred_result_type)

    # Zebranie prawd. w dataframe
  df_p = data.frame(predSexProb)

  # Plot the performance of the model applied to the evaluation set as
  # an ROC curve.
  pred <- prediction(df_p$W, test$class)
  perf <- performance(pred,"tpr","fpr")
  #plot(perf, main="ROC curve", colorize=T)
  
  # Zwykly roc - czytelniejszy na poczatku :)
  roc(test$class, df_p$R, plot=TRUE, print.auc=TRUE)
  legend("bottomright", legend=text)
  
  # And then a lift chart
  perf <- performance(pred,"lift","rpp")
  auc <- performance(pred,"auc")
  
  title = sprintf("lift curve - %s", text)
  
  plot(perf, main=title, colorize=T)
}


# Zwraca nowy numer eksperymentu
trys = 0
next_try <- function()
{
  trys <<- trys+1
}
  
# Zebranie matrycy pomylek w dataframe
store_cm <- function (results, cm, desc)
{
  desc = append(desc, cm$byClass)
  desc = append(desc, cm$overall)
  results = rbind(results, desc) 
  
  return (results)
  
}

#
# Przypisz kategorie W-WHITE R - RED
#
wineRed_ds = read.table("wine_red.csv", header = TRUE, sep=";", na.strings= "*")
wineRed_ds$class = 'R'

wineWhite_ds = read.table("wine_white.csv", header = TRUE, sep=";", na.strings= "*")
wineWhite_ds$class = 'W'

wines_orginal = data.frame()

wines_orginal = rbind(wines_orginal,wineRed_ds)
wines_orginal = rbind(wines_orginal,wineWhite_ds)

wines_orginal$class = factor(wines_orginal$class)

#hist(wines)

#
# I. Eksploracja z algorytem C5.0
#

#
# 1. C5.0 - Proba - wszytkie atrybuty 
#

train_test = split_train_test(wines_orginal, 0.7)

train = train_test$train
test = train_test$test

#  WSZYSTKIE ATRYBUTY
mod_c5 <- C5.0(class ~ ., data = train,trials = 10, rules = TRUE)
summary(mod_c5)

# Pokaze tablice pomylek i statystyki
cm = model_stats(mod_c5, test)
draw_roc_lift(mod_c5, test, "C5.0 - Wszysktie atrybuty - TEST")

desc = c(try_num=next_try(),desc='Wszytkie atrybuty:', tree_type = 'C5.0', type='test')
desc= append(desc, cm$byClass)
desc = append(desc, cm$overall)
results = data.frame(t(desc),stringsAsFactors = FALSE)



# WNIOSKI: Czesc atrybutow ma mniejsze znaczenie w drzewie. Atrybut quality nie jest wykorzystywany.
#          W nastepnych proboba zrezygnuje z czesci atrybutow

#
# 2.  Redukuje pokolei atrytuty z summary(mod_c5) z najmniejszym wykorzystaniem
#      i obliczam maciez pomylek oraz statystyki modelu dla zbioru testowego.
#
wines = subset(wines_orginal, select=-c(quality, density, alcohol, free.sulfur.dioxide, fixed.acidity, pH, citric.acid))

train_test = split_train_test(wines, 0.7)
train = train_test$train
test = train_test$test

mod_c5 <- C5.0(class ~ ., data = train,trials = 10, rules = TRUE)
summary(mod_c5)

# Pokaze tablice pomylek i statystyki
cm = model_stats(mod_c5, test)
draw_roc_lift(mod_c5, test, "C5.0 - 4 atrybuty - TEST")
results = store_cm(results, cm, c(try_num=next_try(),desc='5 atrybutow', tree_type = 'C5.0', type='test') )

View(results)

# WNIOSKI: Statystyki modelu spadly o ok 1% procent, zmniejsze ilosc parametrow, 
#           aby sprawdzic czy moze wystarczy ich jeszce mniej.


# A. Jeden atrybut: chlorides

wines = subset(wines_orginal, select= c(chlorides, class))


train_test = split_train_test(wines, 0.7)
train = train_test$train
test = train_test$test

mod_c5 <- C5.0(class ~ ., data = train,trials = 10, rules = TRUE)
summary(mod_c5)

# Pokaze tablice pomylek i statystyki
cm = model_stats(mod_c5, test)
results = store_cm(results, cm, c(try_num=next_try(),desc='1 atrybut - chlorides', tree_type = 'C5.0', type='test') )
draw_roc_lift(mod_c5, test, "C5.0 - 1 atrybut: chlorides - TEST")

# B. Jeden atrybut: total.sulfur.dioxide

wines = subset(wines_orginal, select= c(total.sulfur.dioxide, class))


train_test = split_train_test(wines, 0.7)
train = train_test$train
test = train_test$test

mod_c5 <- C5.0(class ~ ., data = train,trials = 10, rules = TRUE)
summary(mod_c5)

# Pokaze tablice pomylek i statystyki
cm = model_stats(mod_c5, test)
results = store_cm(results, cm, c(try_num=next_try(),desc='1 atrybut - total.sulfur.dioxide', tree_type = 'C5.0', type='test') )
draw_roc_lift(mod_c5, test, "C5.0 - 1 atrybut: total.sulfur.dioxide - TEST")


# C. 2 - atrybuty chlorides, total.sulfur.dioxide

wines = subset(wines_orginal, select= c(chlorides, total.sulfur.dioxide, class))
#wines = subset(wines_orginal, select= c(chlorides, class))


train_test = split_train_test(wines, 0.7)
train = train_test$train
test = train_test$test

mod_c5 <- C5.0(class ~ ., data = train,trials = 10, rules = TRUE)
summary(mod_c5)

# Pokaze tablice pomylek i statystyki
cm = model_stats(mod_c5, test)
results = store_cm(results, cm, c(try_num=next_try(),desc='2 atrybuty - chlorides, total.sulfur.dioxide, class', tree_type = 'C5.0', type='test') )
draw_roc_lift(mod_c5, test, "C5.0 - 2 atrybuty: chlorides, total.sulfur.dioxide - TEST")

View(results)

# WNIOSKI: Z 2.A i 2.B wynika ze tyle pojedynczy atrybut zmniejsza istotnie sprawnosc modelu na danych testowych.
#          Z tego wynika, ze o kolorze wina decytuje glownie zawartosc 2 substancji: chlorides, total.sulfur.dioxide.
#          Aby uzyskac wysoka i Czulosc (Sensitivity, TPR) i Swoistosc (Specificity, TNR) > 0.8 trzeba 
#          rozwac oba z ww. atrybutow.
#  
# 

#
# II. Budowanie drzwa uzyciem rpart 
# 

#
#  1. Uzycie wszystkich atrybutow o
#
wines = subset(wines_orginal, select= c(chlorides, total.sulfur.dioxide, class))
#wines = subset(wines_orginal, select= c(chlorides, class))

train_test = split_train_test(wines, 0.7)
train = train_test$train
test = train_test$test

#myFormula = class ~ total.sulfur.dioxide + alcohol
rpTree <- rpart(class ~ .,  method="class", data=train)

#rpTree <- rpart(myFormula,  method="class", data=train)
rpTree
plot(rpTree)

rpart.plot(rpTree)
summary(rpTree)

# Pokaze tablice pomylek i statystyki
cm = model_stats(rpTree, test)
draw_roc_lift(rpTree, test, "Wszytkie atrybuty - zb. testowy")

results = store_cm(results, cm, c(try_num=next_try(),desc='2 atrybuty - chlorides, total.sulfur.dioxide, class', tree_type = 'rpTree', type='test') )

View(results)

#
# rPart PRUNNING
#

#wines = subset(wines_orginal, select= c(chlorides, total.sulfur.dioxide, class))
#wines = subset(wines_orginal, select= c(chlorides, class))
wines = wines_orginal

train_test = split_train_test(wines, 0.7)
train = train_test$train
test = train_test$test

#myFormula = class ~ total.sulfur.dioxide + alcohol
rpTree <- rpart(class ~ .,  method="class", data=train)

rpart.plot(rpTree)
summary(rpTree)

# Pokaze tablice pomylek i statystyki
cm = model_stats(rpTree, test)
results = store_cm(results, cm, c(try_num=next_try(),desc='Wszystkie - przed przycinaniem', tree_type = 'rpTree', type='test') )

#View(results)

# prunning
summary(rpTree)
rpTree$cptable[,"xerror"]
min(rpTree$cptable[,"xerror"])
which.min(rpTree$cptable[,"xerror"])
rpTree.cp=rpTree$cptable[3,"CP"]
rpTree.cp
#?prune

# Przeglad drzew przycietych dla roznych wartosci parametru zlozonosci. Zebranie przy okazji maciezy pomylek dla porownania.

for (c in rpTree$cptable[,"xerror"])
{
  text = sprintf("%s, %s, cp=%.2f, %s","Przycinanie","rpart", c, "test")
  print (text)
  
  #pRpTree<- prune(rpTree, cp = rpTree.cp)
  pRpTree<- prune(rpTree, cp = c)
  
  rpart.plot(pRpTree, main=text)
  
  cm = model_stats(pRpTree, test)
  results = store_cm(results, cm, c(try_num=next_try(),desc=text, tree_type = 'rpTree-prunned', type='test') )
  
  # Dla drzewa z jednym wezlem jest tylko jedna klasa i nie narysuja sie wykresy ROC i lift
  if (c < 1) 
  {
    draw_roc_lift(pRpTree, test, text)
  }
  
  #cm = model_stats(pRpTree, train)
  #results = store_cm(results, cm, c(try_num=next_try(),desc=text, tree_type = 'rpTree-prunned', type='train') )

}


View(results)


# 
# III. Dla porowniania - klasyfikator NaiveBayes
#

#
# 1. Proba dla wszystkich zmiennych obajniajacych
# 

wines = wines_orginal

train_test = split_train_test(wines, 0.7)
train = train_test$train
test = train_test$test

nbClasif <- naiveBayes(class ~ ., data=train)
print(nbClasif)

#cm = confusionMatrix(nbClasifTable, mode="prec_recall")

# Pokaze tablice pomylek i statystyki
cm = model_stats(nbClasif, test)
draw_roc_lift(nbClasif, test, "NaiveBayes - wszystkie atrybuty - TEST", pred_result_type='raw')

results = store_cm(results, cm, c(try_num=next_try(),desc='Wszystkie atrybuty', tree_type = 'NaiveBayes', type='test') )


#
# 2. Proba dla 2 attrybutow, ktore poprzednio dawaly najlepsze wyniki
# 

nbClasif <- naiveBayes(class ~ chlorides + total.sulfur.dioxide, data=train)
print(nbClasif)

# Pokaze tablice pomylek i statystyki
cm = model_stats(nbClasif, test)
draw_roc_lift(nbClasif, test, "NB - 2 atrybuty: chlorides,total.sulfur.dioxide-TEST", pred_result_type='raw')

results = store_cm(results, cm, c(try_num=next_try(),desc='2 atrybuty: chlorides, total.sulfur.dioxide', tree_type = 'NaiveBayes', type='test') )

View(results)

# WNIOSKI: Dla wszystkich atrybutow, sprawnosc klasyfikatora jest zblizona do C5.0 i drzewa rpart. Widac to 
#          Na wykresach ROC i podniesienia. Natomiast dla 2 atrybotow, ktore dla drzew dawaly stosunkowo dobra sprawnosc,
#          NB ma wyrazny spadek skutecznosci dla czesci probek testowych, widac do na ostatnim wykresie podniesienia.


# KONIEC #



