##
## MSI - LABORATORIUM 1
## PAWEL MERGIST
##

# ZAD 10

# ODP: Walidacja krzyzowa na zbiorze treningowym sprawila ze model 
#      jest mniej przetrenowany (wykres ROC dla zbioru treningowego).
#      Dzieki temu lepiej uogolnia (ROC zb testowy), wartosci czulosci i specyficznosc dla obu zbiorow sa zblizone.

#install.packages("caret")


library(caret)
library(pROC)
library(e1071)

options(digits=3)

set.seed(12345)

data(cats, package = "MASS")

nmbOfCats = dim(cats)[1]
trIndx = sample(nmbOfCats, nmbOfCats*0.7)
catsTr = cats[trIndx, ]
catsTst = cats[-trIndx, ]


# a) 
# Zbior TRENINGOWY

#tuned <- tune.svm(Sex~., data = catsTr, gamma = c(0.1,1,5,50,500), cost = c(1,50,100))

tuned <- tune.svm(Sex~., data = catsTr, gamma = 2^(-4:10), cost = 2^(-2:10))

# Najlepszy performace dla:
print("BEST PERFORMACE GAMMA, COST")
tuned$best.parameters$gamma
tuned$best.parameters$cost

(summary(tuned))

# Wykorzystaj w modelu i pokaz - tune nie zwraca prawdopodobienstw potrzebnych do ROC
m1 <- svm(Sex~., data = catsTr, kernel='radial',cross=10, gamma=tuned$best.parameters$gamma, cost=tuned$best.parameters$cost, probability=TRUE)
#m1 <- tuned$best.model
plot(m1, cats)

# Zbior TRENINGOWY - matryca pomylek
cm_svm_catsTr <- confusionMatrix(factor(catsTr$Sex), fitted(m1))
print(cm_svm_catsTr$table)
print(cm_svm_catsTr$byClass)

# ROC
predSexProb = predict(m1,catsTr,probability=TRUE)
df = data.frame(attr(predSexProb, "probabilities"))

roc(catsTr$Sex, df$F, plot=TRUE, print.auc=TRUE)
legend("bottomright", legend="Zbior TRENINGOWY: catsTR")

# b)

# Zbior TESTOWY:
pred = predict(m1, catsTst)

# Zbior TRENINGOWY - matryca pomylek
cm_svm_catsTst <- confusionMatrix(factor(catsTst$Sex), pred)
print(cm_svm_catsTst$table)
print(cm_svm_catsTst$byClass)

# ROC
predSexProb = predict(m1,catsTst,probability=TRUE)
df = data.frame(attr(predSexProb, "probabilities"))

roc(catsTst$Sex, df$F, plot=TRUE, print.auc=TRUE)
legend("bottomright", legend="Zbior TESTOWY: catsTst")

