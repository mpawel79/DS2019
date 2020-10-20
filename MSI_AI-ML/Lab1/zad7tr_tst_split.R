##
## MSI - LABORATORIUM 1
## PAWEL MERGIST
##

#install.packages("caret")

library(caret)
library(pROC)
library(e1071)

options(digits=3)

#set.seed(881234)

# 
# Zadanie 7
# 

# ODP: Niezaleznie od ziarna najczesciej najlepszy podel powstaje dla c=10 i gamma=<0.1, 1> : 
#      ma najw. czulosc, specyficznosc i jednoczesnie zdefiniowana ponizej funcja kosztu jest najnizsza.
# 
#                  label cost       Sensitivity       Specificity    Pos.Pred.Value    Neg.Pred.Value
# test, c=10.00 , g=1.00  108 0.823529411764706 0.888888888888889 0.823529411764706 0.888888888888889
# test, c=10.00 , g=0.10  162 0.736842105263158              0.92             0.875 0.821428571428571
#
#      !!! Jednak, aby bazowac na aktualnych danych uczacych, nalezaloby wyliczyc wiecej modeli, lepiej dobrac gamma,
#          a moze nawet metoda glosowania ustalac wynik predycji.
#      
# Ponizej podaje 'przypadek biznesowy', obliczam maciez pomylek i zwiazane z nimi metryki wartosc oczekiwana i prezentuje na wykresach.
#


# PRZYPADEK BIZNESOWY:
#
# W schronisku w klakach jest po 5 kotkow. Jesli kotek M trafi do klatki z F przybedzie 30 kotkow, 
# jesli kotek F trafi do klatki z M przybedzie 6 kotkow.

# Z SIECI: ciekawie o SVM i rysowaniu ROC https://stats.stackexchange.com/questions/37795/roc-curve-for-discrete-classifiers-like-svm-why-do-we-still-call-it-a-curve

cost_mx = matrix(c(0, 6, 30, 0), ncol=2, byrow=TRUE)
cost_mx


EX <- function(conf_mx, cost_mx)
{
  sum(conf_mx * cost_mx)
}

## 'Opakowana' funckcja predict  - dodaje do data.frame results conf matrix z wykonania svm

my_predict <- function(try_num, type, cats_data, m1, c,g )
{
  text = sprintf("%s, c=%.2f , g=%.2f",type, c,g)
  print (text)
  
  predSex = predict(m1,cats_data)
  
  predSexProb = predict(m1,cats_data,probability=TRUE)
  
  print(head(levels(predSexProb)))
  
  
  df = data.frame(attr(predSexProb, "probabilities"))
  
  roc(cats_data$Sex, df$F, plot=TRUE, print.auc=TRUE)
  legend("bottomright", legend=text)
  
  # CONFUSION MATRIX
  cm_svm_c1 <- confusionMatrix(factor(cats_data$Sex),factor( predSex ))
  
  
  print((cm_svm_c1$table))
  
  ex = EX(cm_svm_c1$table, cost_mx)
  
  #print(cm_svm_c1$byClass)
  res_c = c(try_num=try_num,type, C=c, M=1,F=1, gamma=as.numeric(g), label=text, ex)
  res_c = append(res_c, cm_svm_c1$byClass)
  res_c = append(res_c, cm_svm_c1$overall)
}
  
## 'Opakowana' funckcja svm 

my_svm <- function(results, try_num, type, cats_data,  cats_test_data,  kernel, GAMMA, COST, wts )
{
  print(wts)
  
  for (g in GAMMA)
  {
    
  for (c in COST)
  {
    text = sprintf("%s, c=%.2f , g=%.2f",type, c,g)
    print (text)
    
    m1 <- svm(Sex~., data = cats_data, kernel=kernel, gamma=g, cost=c,cross=3, class.weights = wts, probability=TRUE)
   
    #
    plot(m1, cats)
  
    
    res_c1 = my_predict(try_num,'train', cats_data, m1, c, g)
    
    results = rbind(results, res_c1)
    
    res_c1 = my_predict(try_num,'test', cats_test_data, m1, c, g)
    
    results = rbind(results, res_c1)
    
    try_num = try_num + 1
    #  plot(m1, cats)
 
  }
    
  }
  
  return ( results)
}

# ladowanie zbioru cats
data(cats, package = "MASS")





# Tworze conf matrix dla idealnego przypadku aby zainicjowac data.frame results w ktorym bede zbieral
# conf matrix z poszczegolnych prob
cm_svm_c1 <- confusionMatrix(factor(cats$Sex),factor( cats$Sex ))

# zbiez wniki we frame M=0, C=0 idealny przypadek - tylko aby stworzyc data frame 
# na wyniki z svm skladowane w results 
res_c = c(try_num=0,type='type', C=0, M=1,F=1,  gamma=0, label="text", cost=0)
res_c = append(res_c, cm_svm_c1$byClass)
res_c = append(res_c, cm_svm_c1$overall)
results = data.frame(t(res_c),stringsAsFactors = FALSE)

#results = rbind(results, res_c)

GAMMA = c(0.1,1,5,50,500)
COST = c(10)

wts = c(M=1, F=1)

# TRAIN
try_num = 1

# Warto zrobic wiele prob zeby sie upewnic czy modele dla pewnego gamma bedzie najczesciej optymalny
#for(i in seq(1,50,1))
{
  # przygotowanie do nauki
  nmbOfCats = dim(cats)[1]
  trIndx = sample(nmbOfCats, nmbOfCats*0.7)
  #trIndx = sample(nmbOfCats, 3000)
  
  catsTr = cats[trIndx, ]
  catsTst = cats[-trIndx, ]
  
  hist(cats, freq = TRUE)
  hist(catsTr, freq= TRUE)
  hist(catsTst, freq= TRUE)
  
results = my_svm(results, try_num, 'type', catsTr, catsTst, 'radial', GAMMA, COST, wts)
}
# SHOW RESULTS
#usuniecie pierszego wiersza - kt sluzyl do stworzenia dataframe na wyniki
results = results[-c(1),]

# TRAIN RESULTS ONLY
res = results[which(results$type == 'train'),]

plot(res$Accuracy ,pch=1, xaxt= "n", ylim=c(0,1.0),ylab="",  xlab="", main='Kernel - RBF, o-train, x-test', col=1 )
axis(1, cex.axis=0.6, las=3, at=1:length((res$gamma)),labels= res$gamma)
par(new=TRUE)

plot(res$Sensitivity,pch=1  , xaxt= "n", ylim=c(0,1.0),ylab="",    xlab="", main='', col=2 )
par(new=TRUE)

plot(res$Specificity ,pch=1  , xaxt= "n", ylim=c(0,1.0),ylab="",    xlab="Black-Accuracy,red-Sensitivity,green-Specificity", main='', col=3 )
par(new=TRUE)


# TEST RESULTS ONLY
res = results[which(results$type == 'test'),]

plot(res$Accuracy ,pch=4, xaxt= "n", ylim=c(0,1.0),ylab="",  xlab="", main='', col=1 )
axis(1, cex.axis=0.6, las=3, at=1:length((res$gamma)),labels= res$gamma)
par(new=TRUE)

plot(res$Sensitivity,pch=4  , xaxt= "n", ylim=c(0,1.0),ylab="",    xlab="", main='', col=2 )
par(new=TRUE)

plot(res$Specificity ,pch=4  , xaxt= "n", ylim=c(0,1.0),ylab="",    xlab="Black-Accuracy,red-Sensitivity,green-Specificity", main='', col=3 )

#
# Wskazniki z tablicy pomylek dla roznych gamma z testow
#
head(res[order(res$Pos.Pred.Value, decreasing = TRUE),c(7:12)], n=10)

res1 = res[which(res$g ==1.0),]
#res1

head(res1[order(res$Pos.Pred.Value, decreasing = TRUE),c(7:12)])
