
##########################################################
#zadanie wykrycie reguł pokazujących jakie zdarzenie prowadzą do hipoglikemii.

#Przykład wykrycia reguł sekwencyjnych ze zbioru   diab_trans.

#https://archive.ics.uci.edu/ml/datasets/Diabetes
#pobranie danych
download.file('http://staff.ii.pw.edu.pl/~gprotazi/dydaktyka/dane/diab_trans.data','diab_trans.data')
#wczytanie danych do ramki danych
diab.df <- read.csv("diab_trans.data", header=TRUE, stringsAsFactors = FALSE)
View(diab.df)

newdata <- mydata[ which(mydata$gender=='F'
                         & mydata$age > 65), ]

cbind(diab.df,)
diab.df$hipoStatus = "norm"

diab.df$hipoStatus[ which(diab.df$code == 'id_65' & diab.df$value < 70) ]  = 'below'
diab.df$hipoStatus[ which(diab.df$code == 'id_65' & diab.df$value > 120) ] = 'high'


diab.df$value[ which(diab.df$code == 'id_65' & diab.df$value < 70) ]  = 'below'
diab.df$value[ which(diab.df$code == 'id_65' & diab.df$value > 120) ] = 'high'

diab.df$value[ which(diab.df$code == 'id_65' & diab.df$value >=70 & diab.df$code == 'id_65' & diab.df$value <= 120  ) ] = 'norm'


diab.df[ which(diab.df$value == 100) ]

#zamiana kodów na tekst
diab.df[["code"]][diab.df[["code"]] == "id_33"] <- "Regular insulin dose"
diab.df[["code"]][diab.df[["code"]] == "id_34"] <- "NPH insulin dose"
diab.df[["code"]][diab.df[["code"]] == "id_35"] <- "UltraLente insulin dose"
diab.df[["code"]][diab.df[["code"]] == "id_48"] <- "Unspecified blood glucose measurement"
diab.df[["code"]][diab.df[["code"]] == "id_57"] <- "Unspecified blood glucose measurement"
diab.df[["code"]][diab.df[["code"]] == "id_58"] <- "Pre-breakfast blood glucose measurement"
diab.df[["code"]][diab.df[["code"]] == "id_59"] <- "Post-breakfast blood glucose measurement"
diab.df[["code"]][diab.df[["code"]] == "id_60"] <- "Pre-lunch blood glucose measurement"
diab.df[["code"]][diab.df[["code"]] == "id_61"] <- "Post-lunch blood glucose measurement"
diab.df[["code"]][diab.df[["code"]] == "id_62"] <- "Pre-supper blood glucose measurement"
diab.df[["code"]][diab.df[["code"]] == "id_63"] <- "Post-supper blood glucose measurement"
diab.df[["code"]][diab.df[["code"]] == "id_64"] <- "Pre-snack blood glucose measurement"
diab.df[["code"]][diab.df[["code"]] == "id_65"] <- "Hypoglycemic symptoms"
diab.df[["code"]][diab.df[["code"]] == "id_66"] <- "Typical meal ingestion"
diab.df[["code"]][diab.df[["code"]] == "id_67"] <- "More-than-usual meal ingestion"
diab.df[["code"]][diab.df[["code"]] == "id_68"] <- "Less-than-usual meal ingestion"
diab.df[["code"]][diab.df[["code"]] == "id_69"] <- "Typical exercise activity"
diab.df[["code"]][diab.df[["code"]] == "id_70"] <- "More-than-usual exercise activity"
diab.df[["code"]][diab.df[["code"]] == "id_71"] <- "Less-than-usual exercise activity"
diab.df[["code"]][diab.df[["code"]] == "id_72"] <- "Unspecified special event"


diab.df[["code"]][diab.df[["code"]] == "id_72"] 

#przykład zapisu danych do pliku - usunięcie wiersza nagłówka
write.table(diab.df, "diab_trans2.data", sep = "," , row.names = FALSE, col.names = FALSE )

#wczytanie danych w postaci transkacji 
diabSeq <- read_baskets(con = "diab_trans2.data", sep =",", info = c("sequenceID","eventID"))
View(as(diabSeq,"data.frame"))

#ustawienie parametrów
seqParam = new ("SPparameter",support = 0.5, maxsize = 4, mingap=600, maxgap =172800, maxlen = 3 )
patSeq= cspade(diabSeq,seqParam, control = list(verbose = TRUE, tidLists = FALSE, summary= TRUE))



#odkrycie reguł
seqRules = ruleInduction(patSeq,confidence = 0.8)

seqRules
inspect(head(seqRules))

rulesI = subset(seqRules, lhs(seqRules) %ein% c('Regular insulin dose'))

rulesI = subset(seqRules, lhs(seqRules) %ein% c('8','Regular insulin dose'))
inspect(rulesI)

length(seqRules)
#podsumowanie 
summary(seqRules)
#prezentacja przykładowych reguł
res = inspect(head(seqRules,100))



summary(res)

## ODP

#informacje związane z czasem 
?timeFrequency
timeSeq  = as(diab.df,"time_sek")
freqT = timeFrequency(timeSeq, "times")
freqT

##

#statystyki dotyczące wykrytych reguł
summary(seqRules)
str(seqRules)
#length(seqRules)

#pokazanie reguł
inspect(seqRules)

inspect(lhs(seqRules))
inspect(rhs(seqRules))

#lista elementów występujących w sekwencjach
seqRules@elements@items@itemInfo

#wszystkie sekwencje
allSeq <- c(rhs(seqRules),lhs(seqRules))
allSeq <- unique(allSeq)
inspect(allSeq)
str(allSeq)

##

rulesI = subset(seqRules, rhs(seqRules) %in% c('Hypoglycemic symptoms'))
inspect(rulesI)

spanT = timeFrequency(timeSeq, c("span"))
