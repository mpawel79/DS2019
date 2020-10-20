##
## MSI - LABORATORIUM 1
## PAWEL MERGIST
##


# ODP: a) czynnosci ponizej
#      b) Statystyki czulosc i precyzja sa rozne w zaleznosci od proporcji wielkosci zb. treningowego do testowego.
#         Na dopasowanie sie modelu ma wplyw takze ziarno oraz ilos podzialow w walidacji krzyzowej.
#
#         Ogolnie przy wprowadzeniu zbioru testowego kt. model 'nie widzial', pojawily sie FP i FN ktore obnizyly powyzsze statystyki
#         Spadla tez wartosc AUC z 0.844, do  0.741
#
#      c) optymaly model to taki w ktorym spada obciazenie, a nie rosnie juz wariancja (odchylenie)
#         wtedy tez blad powinnien byc najmniejszy
#


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

m1 <- svm(Sex~., data = catsTr, kernel='radial', gamma=1, cost=10, cross =3, probability=TRUE)
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

