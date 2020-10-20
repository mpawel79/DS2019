##
## MSI - LABORATORIUM 1
## PAWEL MERGIST
##

#install.packages("caret")

library(caret)
library(e1071)



# 
# Zadanie 5
# 

# Przeprowad?? eksperyment z r????nymi warto??ciami sta??ej C w klasyfikatorze SVM. 
# Sprawd?? warto??ci {1,50, 100, 10e6}. Pr??wnaj wyniki dla r????nych warto??ci C.

# ODP: Czulosc (Sensitivity) - nie poprawilo sie (plot: 'Kernel: Linear,GAMMA')
# Przy wartosci C=10e6 Sensitivity wyraznie spada.

# KERNEL - LINEAR - rozne C
# 1. Wgraj zbior
data(cats, package = "MASS")

## Conf matrix dla idelnego przypadku - wszystko dobrze porownane - potrzebne aby stworyc dataframe do wynikow
cm_svm_c1 <- confusionMatrix(factor(cats$Sex),factor( cats$Sex ))

# zbierz wniki we frame M=0, C=0 idealny przypadek
res_c = append(c(M=0,C=0), cm_svm_c1$byClass)
res_c = append(res_c, cm_svm_c1$overall)
results = data.frame(t(res_c))


# 2. Zbierz dane dla roznych C
COST = c(1,50, 100, 10e6)

for (cost in COST)
{
  print (cost)

  m2 <- svm(Sex~., data = cats, kernel="linear", cost=cost)
  
  cats$predSex1 = predict(m2,cats)
  
  # CONFUSION MATRIX
  cm_svm_c2 <- confusionMatrix(factor(cats$Sex),factor( cats$predSex1 ))
  print(cm_svm_c2$table)

  res_c2 = append(c(M=1,C=cost), cm_svm_c2$byClass)
  res_c2 = append(res_c2, cm_svm_c2$overall)
  res_c2
  results = rbind(results, res_c2)
  
}

#attach(results)
plot(results$Sensitivity , xaxt= "n", ylim=c(0,1.0),  xlab="Cost param", main='Kernel - LINEAR', col=results$M + 3)
axis(1, at=1:length((results$C)),labels=results$C)

## KONIEC - KERNEL LINEAR

#
# KERNEL - RBF
#
data(cats, package = "MASS")
m1 <- svm(Sex~., data = cats, kernel="linear", cost=1)
plot(m1, cats)
text(x=10,y=10, label=paste("cost=", 0.1))
paste("cost=", 0.1)
GAMMA = 0.1

COST = c(1,50, 100, 10e6)

for (c in COST)
{
  print (c)
  
  data(cats, package = "MASS")
  m2 <- svm(Sex~., data = cats, kernel="radial", gamma=0.1, cost=c)
  plot(m2, cats, main = "SVN cost=" + c)
  
  cats$predSex1 = predict(m2,cats)
  
  # CONFUSION MATRIX
  cm_svm_c2 <- confusionMatrix(factor(cats$Sex),factor( cats$predSex1 ))
  print(cm_svm_c2$table)
  
  res_c2 = append(c(M=2,C=c), cm_svm_c2$byClass)
  res_c2 = append(res_c2, cm_svm_c2$overall)
  res_c2
  results = rbind(results, res_c2)
  
#  plot(m2, cats)
  
}

#attach(results)
plot( results$Sensitivity, ylim=c(0,1.), xaxt= "n", xlab="C param. Blue-Linear, Light blue-RBF", main='Kernel: Linear,GAMMA', col=results$M+3)
axis(1, at=1:length((results$C)),labels=results$C)

## KONIEC - KERNEL RBF


#
# Funkcja confusionMatrix() zwraca szereg interesuj??cych statystyk. Przenalizuj nast??puj??ce kwestie:
#
# https://www.dataschool.io/simple-guide-to-confusion-matrix-terminology/
#
# a) Jak zmienia si?? warto???? Accuracy w zale??no??ci od zmian sta??ej C?
plot( results$Accuracy, ylim=c(0,1.0), xaxt= "n", xlab="C param. Blue-Linear, Light blue-RBF", main='Kernel: Linear,GAMMA', col=results$M+3)
axis(1, at=1:length((results$C)),labels=results$C)

# ODP: Accuracy - Stosunek "trafien i chybien do wszystkich" (TP+TN)/total - spada dla 10e6

# b) Jak zmienia si?? warto???? Balanced Accuracy? Jak zdefiniowana jest ta statystyka?

plot( results$Balanced.Accuracy, ylim=c(0,1.0), xaxt= "n", xlab="C param. Blue-Linear, Light blue-RBF", main='Kernel: Linear,GAMMA', col=results$M+3)
axis(1, at=1:length((results$C)),labels=results$C)

# ODP: Ballanced Accuracy = (Czulosc + Precyzja)/2, czyli 'dokladnosc trafienia z obu klas' przez dwa.
#      Miara ta z rownowazy jesli klasyfikacja bedzie faworyzowac jedna klase




# c) Co m??wi?? nam warto??ci statystyk Sensitivity, Specificity, Negative Prediction Value, Positive Prediction Value?

# ODP:
# za https://pl.wikipedia.org/wiki/Tablica_pomy%C5%82ek
# i https://en.wikipedia.org/wiki/Confusion_matrix

# Sensitivity - czulosc - stosunek TPR = TP/(TP+FN)
# Specificity - swoistosc - stosunek TNR = TN/(TN+FP) 
# Precision - precyzja PPV = TP/(TP+FP) precyzja  PPV ??? warto???? predykcyjna dodatnia, sprawnosc przypisywania do poprawnej klasy
# (NPV) = TN/(TN+FN) precyzja przypisywania do alternatywnej klasy - (nie znalazlem polskiej definicji)


plot( results$Pos.Pred.Value, ylim=c(0,1.0), xaxt= "n", xlab="C param. Blue-Linear, Light blue-RBF", main='Kernel: Linear,GAMMA', col=results$M+3)
axis(1, at=1:length((results$C)),labels=results$C)

plot( results$Neg.Pred.Value, ylim=c(0,1.0), xaxt= "n", xlab="C param. Blue-Linear, Light blue-RBF", main='Kernel: Linear,GAMMA', col=results$M+3)
axis(1, at=1:length((results$C)),labels=results$C)


# d) Zastan??w si?? nad przydatno??ci?? statystyk z c) w kontek??cie klasyfikacji binarnej
# np. detekcji zdarze?? dotycz??cych niesp??acalno??ci kredyt??w lub wykrycia nowotworu? 
# Kt??re ze statystyk chcieliby??my optymalizowa???

# ODP: Dla nowotorow:

# Zalezy nam, aby zweryfikowac nawet blednie wskazanego pacjeta jako chorego (FP), zeby nikogo 'nie przegapic',
# wiec model zaakceptujemy model o najnizszej swoistosci - (specificity - TNR) - dopuszczajac w wyrazeniu TNR = TN/(TN+FP) wiecej FP.
# Jednoczesnie zalezy nam na tym aby model by jak najlepszy wiec o czulosci (Sensitivity) TPR = TP/(TP+FN) jak najwiekszej.

# TNR - male, TPR -> duze, ale takze FP/N najnajmniejsze
# 
#  Niewyplacalnosc kredytow:
#  Tu zalezly nam aby najwiecej zaoszczedzic, wiec jakosc modelu trzeba powiazac z dlugiem: TP = Suma(TP * dlug) + Suma(FP) * dlug
#
# 

