##
## MSI - LABORATORIUM 1
## PAWEL MERGIST
##

#install.packages("caret")
#install.packages("pROC")

library(caret)
library(pROC)
library(e1071)


#
# Zadanie 1
#

# W ramach poznania klasyfikatora zapoznaj si?? z dokumentem svm.pdf za????czonym w pliku z ??wiczeniami.
#  a) Zastan??w si?? jaki wp??yw ma sta??a C na wyniki klasyfikatora?

# ODP: C - skaluje naruszanie warunku yi(wT x + b)) >= 1 w ktorym plaszczyzna separujemy wektory (dane) na dwa zbiory

#  b) Przyjmijmy, ??e mamy zbi??r treningowy o rozmiarze m x p (m liczba przypadk??w
#  treningowych, p liczba cech). W ilu wymiarowej przestrzeni b??d?? realizowane obliczenia dla j??da RBF?

# ODP: W przestrzeni o jednym wymiarze wiecej

#  c) Czy SVM jest klasyfikatorem liniowym? Odpowiadaj?? na pytanie pos??u?? si?? powy??szym rysunkiem.

# ODP: TAK - separatorem jest plaszczyzna. Wspolczynki w formule rozdzielajacej sa stalymi. 



# 
# Zadanie 2
#

#  a) Jakiego rodzaju zmienne wyst??puj?? w zbiorze?
#  b) Jakie oceniasz mo??liwo???? skutecznej klasyfikacj?? p??ci na podstawie dw??ch pozosta??ych zmiennych?

# ODP: a) Sa 3 zmienne plec (Sex) - zmienna typu - kategoria, oraz 2 zmienne nominalne Hwt [g], Bwt [kg] - pkt 1. i 3.
# ODP: b) W okolicach Btw 3.0 kg i Hwt: 14 g  zmienia sie plec - pkt 2. 

# wgranie zbioru do cats
data(cats, package = "MASS")

# 1. Przeglad danych:
head(cats)
summary(cats)
boxplot(cats)

# 2. porownanie par
pairs(cats)

# 3. Wykres - #prosty plot z podzialem na klasy z plci (Sex)
attach(cats)
plot(Bwt ~ Hwt , pch=unclass(Sex))

xyplot( Bwt ~ Hwt , data = cats, groups = cats$Sex,
        panel=function(...){
          panel.xyplot(...)
          panel.grid(...)
        })

# 4. Sprawdzenie zaleznosci liniowej pomiedzy Sex a Bwt, Hwt

#install.packages('Hmisc')
library("Hmisc")

dmy <- dummyVars(" ~ .", data = cats)
trsf <- data.frame(predict(dmy, newdata = cats))
head(trsf)
cor.test(trsf)
trsf <- subset(trsf, select = -c(SexFactor.F, SexFactor.M, SexNum))

res2 <-rcorr(as.matrix(trsf))#, type=c("spearman"))
res2

# ODP:  Sa pewne zaleznosci pomiedzy plcia a waga serca i ciala

#
# Zadanie 3
#
# W tym kroku dla zbioru ???cats??? sprawd?? :
# 1. jak zachowa si?? prosty klasyfikator oparty o regresj?? logistyczn?? 
# 2. i narysuj granic?? decyzyjn??.

# https://machinelearningmastery.com/mcnemars-test-for-machine-learning/
# https://www.youtube.com/watch?v=xugjARegisk - ROC and AUC, Clearly Explained

# ODP: 1 - wykonane na TYLKO NA zbiorze treningowym
# AIC: 138.24 
#  Balanced Accuracy : 0.7723   
#            Reference
# Prediction  F  M
# F 31 16
# M 13 84
#

# ODP: 2
data(cats, package = "MASS")

# 1. Model regresji logistycznej
mdl = glm(Sex~., data = cats, family = binomial)

summary(mdl)

# CONFUSION MATRIX
cats$response = predict(mdl, cats, type = "response")

cats$predSex[ which(cats$response >= 0.5)] = 'M'
cats$predSex[ which(cats$response < 0.5)] = 'F'


TP = sum(cats$Sex == 'M' & cats$predSex == 'M')
TN = sum(cats$Sex == 'F' & cats$predSex == 'F')
FP = sum(cats$Sex == 'M' & cats$predSex == 'F')
FN = sum(cats$Sex == 'F' & cats$predSex == 'M')

cm_glm <- confusionMatrix(factor(cats$Sex),factor( cats$predSex ))

cm_glm$table

# ROC - wykres dla wszystkich poziomow prawdopodobienstwa
roc(cats$Sex, mdl$fitted.values, plot=TRUE, print.auc=TRUE)
legend("bottomright", legend=c("Logisitic Regression"))



#fourfoldplot(cm$table)

# 2. Wspolczynniki prostej (plaszczyzny) granicy decyzyjnej
slope <- coef(mdl)[3]/(-coef(mdl)[2])
intercept <- coef(mdl)[1]/(-coef(mdl)[2]) 

xyplot( Bwt ~ Hwt , data = cats, groups = cats$Sex,
        panel=function(...){
          panel.xyplot(...)
          panel.abline(intercept , slope)
          panel.grid(...)
        })


#
# Zadanie 4
#
# ODP: Modele maja podobne wskazniki, ktory 'lepszy' moze zdecydowac ilosc TP i FP w zaleznosci od potrzeb. 
# Bazujac na tablicy pomylekmodel glm vs svm cost =1 -> narazie gorzej GLM
#
# GLM # Prediction  F  M
# F 31! 16
# M 13 84
#
# SVM Reference
# Prediction  F  M
# F 33* 14
# M 16 81
#
# GLM
# 1. Model regresji logistycznej
print(cm_glm$overall)


# SVM
data(cats, package = "MASS")

m1 <- svm(Sex~., data = cats, kernel="linear", cost=1, probability=TRUE)
plot(m1,cats)
summary(m1)
cats$predSex = predict(m1,cats)
cats$predSexProb = predict(m1, cats, probability = TRUE)

# CONFUSION MATRIX
cm_svm_c1 <- confusionMatrix(factor(cats$Sex),factor( cats$predSex ))

print(cm_svm_c1$overall)

# POROWNANIE: glm vs svm, tablica pomylek i ogolne wskazniki

cm_glm$table
cm_glm$overall
cm_glm$byClass

cm_svm_c1$table
cm_svm_c1$overall
cm_svm_c1$byClass

