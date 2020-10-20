##
## MSI - LABORATORIUM 1
## PAWEL MERGIST
##

# ZAD 11
# 

# Zeby sprawdzic zachowanie modelu na zbiorze walidacyjnym wystarczy uruchomic caly kod.
# W katalogu biezacym musza byc pliki:
# 
# - defaults.csv
# - defaults_valid.csv
# - svm_opitimal_g=0.10_c=1.00_tr=10883.rds
#
#
# ODP: Wyniki na zbiorze testowym sa jak ponizej:
#      
#
#
#  Reference
#Prediction    F    T
#F 1080  139
#T  152  186
#Sensitivity          Specificity       Pos Pred Value       Neg Pred Value            Precision 
#0.877                0.572                0.886                0.550                0.886 
#Recall                   F1           Prevalence       Detection Rate Detection Prevalence 
#0.877                0.881                0.791                0.694                0.783 
#Balanced Accuracy 
#0.724 

# RESULTAT NA ZBIORZE TESTOWYM:
#
#  Dobrze widac zaleznosc pomiedzy maciaza pomylek parametrami modelu na zbiorze testowym
#
#  
#  
# Model: svm_opitimal_g=0.10_c=1.00_tr=10883.rds
# 

TRAIN = FALSE

#
# model_roc(m1, clientsTst, "Zbior TESTOWY")
# Reference
#Prediction   F   T
#F 547  54
#T  96 106
#Sensitivity          Specificity       Pos Pred Value       Neg Pred Value            Precision 
#0.851                0.662                0.910                0.525                0.910 
#Recall                   F1           Prevalence       Detection Rate Detection Prevalence 
#0.851                0.879                0.801                0.681                0.748 
#Balanced Accuracy 
#0.757 
#
#

library(caret)
library(pROC)
library(e1071)

library("ROCR")

#setwd("~/Documents/Studia DS/MSI/svm")

options(digits=3)

set.seed(71)



#
# 1. Przeglad
#

preview_data <- function(clients)
{

#NA nans
clients[is.na(clients)]
clients[is.nan(as.matrix(clients))]


head(clients)
hist(clients$LIMIT_BAL)
hist(clients$AGE)
hist(clients$MARRIAGE)
hist(clients$SEX)

}

#
# 2. Nominalne na DUMMIES
# 
preprocess_data <- function(clients)
{
  #remove with nas
  clients <- na.omit(clients)
  
  # dummy values, one hot encoding
  clients$SEX = factor(clients$SEX)
  
  clients$EDUCATION = factor(clients$EDUCATION)
  clients$MARRIAGE = factor(clients$MARRIAGE)
  
  # encone dummys https://amunategui.github.io/dummyVar-Walkthrough/
  dmy <- dummyVars(" ~ .", data = clients)
  trsf <- data.frame(predict(dmy, newdata = clients))
  #head(trsf)
  
  clients = trsf
  #head(clients)

#
# Ustawinei default.payment.next.month jako TARGET
# 
# zmiana dlugiej nazwy kolumn w R https://stackoverflow.com/questions/6081439/changing-column-names-of-a-data-frame
colnames(clients)[which(names(clients) == "default.payment.next.month")] <- "DEF"

# przypisanie tekstowej kategorii
clients$Target[which(clients$DEF == 0)] = 'F'
clients$Target[which(clients$DEF == 1)] = 'T'

# ustawinie jako czynnik
clients$Target <- factor(clients$Target)

# usuniecie columny ID i DEF (orginalnie default.payment.next.month)
clients <- subset(clients, select = -c(ID, DEF))

return(clients)
}


#
#  3. Przygotowanie zbiorow uczacych, testowych
# 

train_test_split<-function(clients, f=0.7)
{
  
  # Klasyczny podzial 7:3
  
  nmbOfclients = dim(clients)[1]
  trIndx = sample(nmbOfclients, nmbOfclients * f)
  
  clientsTr = clients[trIndx, ]
  clientsTst = clients[-trIndx, ]
    
  return(list("train"=clientsTr, "test"=clientsTst))

}

#
# Podzial na klasy z preferencja rzadkiej target , T - default , F - not default
#

train_test_split_class<-function(clients, class_T=0.7, class_F=0.3, max_T=-1, max_F=-1,max_test_set = -1)
{
  

# check on class T only
clients_Tonly = clients[which(clients$Target == 'T'), ]
clients_Tonly


nubOfT  = dim(clients_Tonly)[1]
nubOfT

nubofTsamples = if (max_T > 0) max_T else nubOfT*class_T

dim(clients_Tonly)[1]
clients_TonlyIndexes = as.numeric(rownames(clients_Tonly))
clients_TonlyIndexes
clientsTr_TonlyIndexes = sample(clients_TonlyIndexes, nubofTsamples)
clientsTr_TonlyIndexes

clientsTrT = clients[clientsTr_TonlyIndexes, ]

clientsTestT = clients_Tonly[-clientsTr_TonlyIndexes, ]


# CLASS F
clients_Fonly = clients[which(clients$Target == 'F'), ]
clients_Fonly

nubOfF  = dim(clients_Fonly)[1]
nubOfF
#print(sprintf("#"))
nubofFsamples = if (max_F > 0) max_F else nubOfF*class_F

dim(clients_Fonly)[1]
clients_FonlyIndexes = as.numeric(rownames(clients_Fonly))
#clients_FonlyIndexes
clientsTr_FonlyIndexes = sample(clients_FonlyIndexes, nubofFsamples)

clientsTrF = clients[clientsTr_FonlyIndexes, ]
clientsTestF = clients_Fonly[-clientsTr_FonlyIndexes, ]


clientsTr = rbind(clientsTrT, clientsTrF)
clientsTst = rbind(clientsTestT, clientsTestF)

# restrict test size to max_test_set
clientsTst_Indexes = as.numeric(rownames(clientsTst))
clientsTst_Indexes = if (max_test_set > 0) sample(clientsTst_Indexes, max_test_set) else clientsTst_Indexes
clientsTst = clientsTst[clientsTst_Indexes,]

return(list("train"=clientsTr, "test"=clientsTst))

}

my_train<-function(clientsTr)
{


# a) 
# Zbior TRENINGOWY

# 
# Uczenie SVM
#

G = c(0.05,0.1,0.5,1)  
C = c(1,1.5,10)
  
#G = c(0.1)  
#C = c(1)

train_size= dim(clientsTr)[1]


# Najlepszy performace dla:
print("TUNE SVN")

tuned <- tune.svm(Target~., data = clients, gamma = G, cost = C, tunecontrol=tune.control(sampling = "cross") )
m1 <- tuned$best.model
model_name_tune = sprintf("%s_g=%.2f_c=%.2f_tr=%d.rds","tune_svm_cross10", tuned$best.parameters$gamma,tuned$best.parameters$cost, train_size)
saveRDS(m1, file=model_name_tune) 

(summary(tuned))

# Wykorzystaj w modelu z svm - tune nie zwraca prawdopodobienstw potrzebnych do ROC
m1 <- svm(Target~., data = clientsTr, kernel='radial', gamma=tuned$best.parameters$gamma, cost=tuned$best.parameters$cost, probability=TRUE, cross=10)
model_name_svm = sprintf("%s_g=%.2f_c=%.2f_tr=%d.rds","svm_opitimal", tuned$best.parameters$gamma,tuned$best.parameters$cost, train_size)
saveRDS(m1, file=model_name_svm) 

return (list("model"=m1, "name"=model_name_tune))

}


model_roc<-function(m1, clientsTst, legend_text="Zbior TRENINGOWY: clientsTR")
{
  
  pred = predict(m1, clientsTst)
  
  # Zbior TRENINGOWY - matryca pomylek
  cm_svm_clientsTst <- confusionMatrix(factor(clientsTst$Target), pred)
  print(cm_svm_clientsTst$table)
  print(cm_svm_clientsTst$byClass)
  
  # ROC
  predSexProb = predict(m1,clientsTst,probability=TRUE)
  df = data.frame(attr(predSexProb, "probabilities"))
  
  roc(clientsTst$Target, df$T, plot=TRUE, print.auc=TRUE)

  legend("bottomright", legend=legend_text)
}



main <- function(arg="", argn=0)
{
  
  clients = read.csv("./defaults.csv", sep = ";", header = TRUE)
  
  #clients = clients[10000,]
  
  preview_data(clients)
  
  clients = preprocess_data(clients)
  
  res  = train_test_split_class(clients, max_test_set = 2000)
  
  #res  = train_test_split(clients)
  
  
  clientsTr = res$train
  clientsTst = res$test
  
  
  dim(clients)
  dim(clientsTr)
  dim(clientsTst)
  
  
  res <- my_train(clientsTr)
  m1 = res$model
  model_name=res$name
  
  model_roc(m1, clientsTr, "Zbior TRENINGOWY")
  
  clientsTst <- na.omit(clientsTst)
  model_roc(m1, clientsTst, "Zbior TESTOWY")
  
  
  # Zbior TRENINGOWY - matryca pomylek
  cm_svm_clientsTr <- confusionMatrix(factor(clientsTr$Target), fitted(m1))
  print(cm_svm_clientsTr$table)
  print(cm_svm_clientsTr$byClass)
  
  # ROC
  predSexProb = predict(m1,clientsTr,probability=TRUE)
  
  df = data.frame(attr(predSexProb, "probabilities"))
  head(df)
  roc(clientsTr$Target, df$T, plot=TRUE, print.auc=TRUE)
  legend("bottomright", legend="Zbior TRENINGOWY: clientsTR")
  
  # b)
  
  # Zbior TESTOWY:
  #clientsTst = smpl
  
  clientsTst <- na.omit(clientsTst)
  
  pred = predict(m1, clientsTst)
  pred
  dim(pred)
  # Zbior TESTOWY - matryca pomylek
  clientsTst$Target

  
  cm_svm_clientsTst <- confusionMatrix(factor(clientsTst$Target), pred)
  print(cm_svm_clientsTst$table)
  print(cm_svm_clientsTst$byClass)
  
  # ROC
  predSexProb = predict(m1,clientsTst,probability=TRUE)
  df = data.frame(attr(predSexProb, "probabilities"))
  
  roc(clientsTst$Target, df$T, plot=TRUE, print.auc=TRUE)
  legend("bottomright", legend="Zbior TESTOWY: clientsTst")
  
  # proba
  
  return (model_name)
}




main_validate<-function(arg="", argn=0)
{


    clients = read.csv("./defaults.csv", sep = ";", header = TRUE)
    
    preview_data(clients)
    
    clients = preprocess_data(clients)
    
    # Podziel na train test, ale zachowaj 0.7 z klasy default == T w clientsTr, 
    # i zostaaw 0.3 default == T na testy po nauczeniu w clientsTst, do tego dolacz 2000 przypadkow do classTst z defalut == F
    #res = train_test_split_class(clients, class_T=0.7, class_F=0.3, max_T=500, max_F=500,max_test_set = 1000)
    res = train_test_split_class(clients, class_T=0.8, class_F=0.3, max_T=-1, max_F=-1,max_test_set = 2000)
    
    #res = train_test_split_class(clients, max_test_set = 2000)
    
    #res  = train_test_split(clients)
    
    clientsTr = res$train
    clientsTst = res$test
    
    dim(clients)
    dim(clientsTr)
    dim(clientsTst)
    

    
    if (TRAIN == TRUE)
    {
      res = my_train(clientsTr)
      m1 = res$model
      model_name = res$name 
    }else
    {
      model_name = "tune_svm_cross10_g=0.10_c=1.00_tr=10883.rds"
      model_name = "svm_opitimal_g=0.10_c=1.00_tr=10883.rds"
      m1 <- readRDS(file=model_name)
    }
  
    
    # CHECK ON CLASS T only
    print("SPRAWDZENIE NA KLASIE DEFAULT=TRUE")
    clientsTst_class_T = clientsTst[which(clientsTst$Target == 'T'), ]
    dim(clientsTst_class_T)
    pred = predict(m1, clientsTst_class_T)
    table(pred)
    #table(clientsTst_class_T$Target)    
    
    
    print("ROC i TABLICA POMYLEK DLA - Zbior TRENINGOWY")
    model_roc(m1, clientsTr, "Zbior TRENINGOWY")
    
    print("ROC i TABLICA POMYLEK DLA - Zbior TESTOWY")
    clientsTst <- na.omit(clientsTst)
    model_roc(m1, clientsTst, "Zbior TESTOWY")
    
    #
    # Porownianie ze zbiorem WALIDACYJNYM 
    #
    def_w = read.csv("./defaults_valid.csv", sep = ";", header = TRUE)
    
    #def_w[,25] = as.factor(def_w[,25])
    #defaultsResults = predict(svm.fit, def_w)
    
    #preview_data(def_w)
    
    def_w = preprocess_data(def_w)
    
    
    #def_w[,25] = as.factor(def_w[,25])
    defaultsResults = predict(m1, def_w)
    #ctab = table(defaultsResults, def_w$default.payment.next.month)
    ctab = table(defaultsResults, def_w$Target)
    
    cm = confusionMatrix(ctab)
    print(cm)
    
    
    print("ROC i TABLICA POMYLEK DLA - Zbior WALIDACYJNY")
    def_w <- na.omit(def_w)
    model_roc(m1, def_w, "Zbior WALIDACYJNY")
    
    
}

main_validate()
