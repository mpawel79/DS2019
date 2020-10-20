##
## MSI - LABORATORIUM 1
## PAWEL MERGIST
##

#install.packages("caret")

library(caret)
library(e1071)

options(digits=3)

# 
# Zadanie 6  
# 

## 'Opakowna' funckcja svm - dadaje do data.frame results conf matrix z wykonania svm

my_svm <- function(results, method_num, cats_data, kernel, gamma, COST, wts )
{
  
  for (c in COST)
  {
    print (c)
    
    m2 <- svm(Sex~., data = cats_data, kernel=kernel, gamma=gamma, cost=c, class.weights = wts)
   
    plot(m2, cats, main = "Method:" + method_num +" SVN cost=" + c )
    
    predSex = fitted(m2)
  
    # CONFUSION MATRIX
    cm_svm_c2 <- confusionMatrix(factor(cats_data$Sex),factor( predSex ))
    
    print((cm_svm_c2$table))
    
    plot(m2, cats)
    
    res_c2 = append(c(M=method_num,C=c, wts), cm_svm_c2$byClass)
    res_c2 = append(res_c2, cm_svm_c2$overall)
    res_c2
    results = rbind(results, res_c2)
    
    #  plot(m2, cats)
 
  }
  
  return (results)
}

# ladowanie zbioru cats
data(cats, package = "MASS")

# Tworze conf matrix dla idealnego przypadku aby zainicjowac data.frame resutls w ktorym bede zbieral
# conf matrix z poszczegolnych prob
cm_svm_c1 <- confusionMatrix(factor(cats$Sex),factor( cats$Sex ))

# zbiez wniki we frame M=0, C=0 idealny przypadek - tylko aby stworzyc data frame 
# na wyniki z svm skladowane w results 
res_c = append(c(M=0,C=0, c(wM=1,wF=1)), cm_svm_c1$byClass)
res_c = append(res_c, cm_svm_c1$overall)
results = data.frame(t(res_c))


# Sprawdzam ile jest osobnikow danej plci
table(cats$Sex)

# ODP: W rozkladzie - prawie 2 krotnie wiecej rekordow dotyczy samcow. 

# ODP: Parametr class.weights - parametr ktorym mozemy zwiekszyc czulosc dla danej klasy.
#      Dla jadra liniowego przesuwa on przestrzen tak aby 'lapac' wiecej elementow z klasy o wyzszej wadze.
#      Dla jadra RBF to przesuniecie jest wiekszym obszarem 'wokol' elementu z danej klasy
#
# Obliczam ile jest M i F w zbiorze, ustawiam parametry jak w poleceniu

numberOfM = sum(cats$Sex == 'M')
numberOfF = sum(cats$Sex == 'F')

wF=1
wM=numberOfF/numberOfM
wts=c(M=wM, F=wF)
wts

COST = c(50)
# 1 proba dla ustawionych parametrow.
# Przekazuje results, nr metody - method_num, data, kernel, gamma, COST, weights )
results = my_svm(results, 1 , cats, 'linear', 0, COST, wts)

# Przegladam ostatni wiersz z results
last = dim(results)[1]
results$Accuracy[last]
results$Precision[last]


# Zwiekszam wM o 0.1

wts=c(M=wM+0.1, F=wF)
wts

results = my_svm(results, 1 , cats, 'linear', 0, COST, wts)

last = dim(results)[1]
results$Accuracy[last]
results$Precision[last]


# ODP: Zwiekszenie wagi parametru powoduje zwiekszenie Accuracy z  0.729   0.743.
# Zwiekszyla sie oczywiscie wartosc True positive dla klasy M, ale takze False Positive dla M i F
# PRZED:
#  M     F 
#  0.485 1.000 
#         Reference
#Prediction  F  M
#          F 39  8
#          M 31 66
#
#PO:
#     M     F 
#     0.585 1.000 
#         Reference
#Prediction  F  M
#          F 37 10
#          M 27 70

#
# Dodatkowe proby zeby porownac tendecje przy zmianie parametru
#
#COST = c(1,50, 100, 10e6)

for (i in 1:10)
{
wts=c(M=wM + 0.1 * i, F=wF)
print (wts)
results = my_svm(results, 2, cats, 'linear', 0, COST, wts)
}

for (i in 1:10)
{
  wts=c(M=wM + 0.1 * i, F=wF)
  print (wts)
  results = my_svm(results, 3, cats, 'radial', 0.01, COST, wts)
}


#attach(results)
#usuniecie pierszego wiersza - kt slyzyl do stworzenia dataframe na wyniki
results = results[-c(1),]

plot(results$Accuracy ~ results$wM ,col=results$M, ylim=c(0,1.0),xlab="wM param. ", main="Red-Linear, Green-RBF")#, xaxt= "n", ylim=c(0,1.0),  xlab="wM param. Red-Linear, Green-RBF", main='Kernel - LINEAR', col=results$M )


plot(results$Accuracy  , xaxt= "n", ylim=c(0,1.0),  xlab="wM param. Red-Linear, Green-RBF", main='Kernel - LINEAR', col=results$M )
axis(1, cex.axis=0.6, las=3, at=1:length((results$M)),labels=sprintf("wM=%.2f", results$wM))


