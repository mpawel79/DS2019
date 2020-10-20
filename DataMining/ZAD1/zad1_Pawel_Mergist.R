##############################################################
#ZADANIE: Odkrywanie regu?? asocjacyjnych w zbiorze Groceries
##############################################################


#??adowanie bibiliotek
library(arules) #regu??y asocjacyjne
library(arulesViz) # wizualizacja regu??

#?Groceries
dataTR = data(Groceries)
dataTR = (Groceries)
summary(dataTR)
inspect(head(dataTR))

#
#1. Wst??pna analiza danych transakcyjnych - przegladam cos jest w danych
#

# a) cz??sto???? wyst??powania element??w, nominalnie 

freqTbl  = itemFrequency(dataTR, type = "absolute")

summary(freqTbl)

# najczesciej
head(sort(freqTbl,decreasing = TRUE),15)

# Mleko, wazykwa, woda wystepuje najczesciej, piwo w pierwszej 15-stce

# najrzadziej
head(sort(freqTbl,decreasing = FALSE),5)

# Najrzadziej akcesoria kuchenne i torby (ale nie tzw. shopping bags)


# b) cz??sto???? wyst??powania element??w, wzglednie 

freqTbl  = itemFrequency(dataTR, type = "relative")

summary(freqTbl)

# najczesciej
head(sort(freqTbl,decreasing = TRUE))

# najrzadziej
head(sort(freqTbl,decreasing = FALSE))

# Jeszcze czestotliwoc graficzne
itemFrequencyPlot(dataTR, type ="relative", support= 0.1, topN=10)
# odwpowiada to elmentowm o wsparciu powyzej 10%

#pokazanie element??w ze wsparciem > 10%
print(sort(freqTbl[freqTbl>0.1], decreasing=T))


# c) ile jest kategori produktow

print(length(freqTbl))

# d) obraz calosciowe ujecia transakcji i kategori 

image(head(Groceries,400))

# Wnioski: Z a-d wiem ile jest; jest kategori, czego jest najwiecej nominalnie i procentowo w tym zbiorze.
#          Spodziewam sie czesego wystepowania 'whole milk' i 'other vegetables'. Z d) wynika ze wystepuje czestrze
#          wystepowanie w transakcjach niektorych kategorii.
#          Sa nimi a) b), wartosci absolutne i wzgledne:
#      whole milk other vegetables       rolls/buns             soda           yogurt    bottled water 
#            2513             1903             1809             1715             1372             1087 
#       0.2555160        0.1934926        0.1839349        0.1743772        0.1395018        0.1105236 
#


#
# 2 Wykrywanie zbior??w cz??stych
#

# a) czestosc wystepowania kategori bedzie pokrywala sie z czestoscia wystepowania zbiorow jednoelementowych.
#    Sprawdzam wiec, jakie produkty sa kupowane razem dla 2,3,4 kategorii. Dla wygody przygotowuje liste i wykres ze wsparciem.

# 2 kategorie
aParam  = new("APparameter", "target" = 'frequent itemsets', "confidence" = 0.5, "support" =0.001, "minlen"= 2)
asets <-apriori(dataTR,aParam)

top_best_support = head(sort(asets, by="support", decreasing = TRUE), 5)
inspect(head(sort(asets, by="support", decreasing = TRUE),5))
plot(top_best_support, method = 'graph')

# --> Dla 2 kategori, najczestsze wystepowanie jest dla {other vegetables,whole milk} ok 7% transakcji tj. 736 razy 


# 3 kategorie
aParam  = new("APparameter", "target" = 'frequent itemsets', "confidence" = 0.5, "support" =0.001, "minlen"= 3)
asets <-apriori(dataTR,aParam)

top_best_support = head(sort(asets, by="support", decreasing = TRUE), 5)
inspect(head(sort(asets, by="support", decreasing = TRUE),5))
plot(top_best_support, method = 'graph')

# --> Dla 3 kategori, najczestrze wystepowanie jest dla {root vegetables,other vegetables,whole milk} ok 2% transakcji tj. 228 razy 


# 4 kategorie
aParam  = new("APparameter", "target" = 'frequent itemsets', "confidence" = 0.5, "support" =0.001, "minlen"= 4)
asets <-apriori(dataTR,aParam)

top_best_support = head(sort(asets, by="support", decreasing = TRUE), 5)
inspect(head(sort(asets, by="support", decreasing = TRUE),5))
plot(top_best_support, method = 'graph')

# --> Dla 4 kategori, najczestrze wystepowanie jest dla {root vegetables,other vegetables,whole milk,yogurt} jednak czestosc jest ponizej 1%.

# Wnioski: przeglad kategorii daje poglad jakie produkty wystepuja razem w liscie transakcji.
# Analiza regul powinna dac ciekawsze wnioski jak czesto produkty wystepuja w koszykach razem i co warunkuje ich wystepowanie.


#
# 3 Wykrywanie regul asocjacyjnych 
#
# Do wykrycia i zbania regul uzyje funcji apriori z biblioteki arules. 
# Interesuje mnie:
# a) przeglad regul wedlug ilosci elementow, ich wsparcia, pewnosci i podniesienia
# b) reguly zawierajace alkochole: canned beer and bottle beer, shopping bags
# c) mozliwie dlugie reguly, zawierajace wiele elementow, zeby zobaczyc co w nich jest i co warunkuja 
# d) reguly minimalne ale mozliwie najlepiej 'prowadzace' do swojego nastepnika

#a) przeglad regul wedlug ilosci elementow, ich wsparcia pewnosci i podniesienia

# 1. Znalezienie regul dla 'lagodnych' parametrow, aby uzyskac mozliwie duzy zbior do analizy. 
# Zaczynam od minimalnej dlugosci regul z 2 elementami, aby nie dublowac wynikow z analizy wsparcia powyzej
# Przetwarzanie nie powinno sie zmiescic w 10 sekundach
#?APparameter
aParam  = new("APparameter", "target" = "rules",  "confidence" = 0.001, "support" = 0.001, "minlen"= 2, "maxlen"=10, maxtime = 10) 
print(aParam)

# wykrywanie regul z aktualnymi parametrami
asets <-apriori(dataTR,aParam)

# 2. Przeglad regul z najwiekszym wsparciem
tmp = head(sort(asets, by="support", decreasing = TRUE), 15)
inspect(tmp)
plot(tmp, method = 'graph')
# --> Najczesciej wystepuja produkcty mleczne, warzywa i pieczywo

# 3. Sprawdzam parametr wiarygodnosc, i sortuje dodatkowow po po podniesieniu
tmp = head(sort(asets, by=c('confidence','lift', 'support'), decreasing = TRUE), 15)
inspect(tmp)
#plot(tmp, method = 'graph')  --> wykres malo czytelny dla wiecej niz 5 regul

#--> Wiedzac co klient ma w koszyku bedac jeszcze w sklepie mozna by mu wyswietlic podpowiedz (na stronie WWW, na telefonie lub 
#    reklamie obok polki) ze moglby dolozyc warzywa lub mleko

# Przegladam wedlug podniesienia, count od 5 do 15

tmp = head(sort(asets, by="lift", decreasing = TRUE), 5)
inspect(tmp)
plot(tmp, method = 'graph')

#--> 4. Wylaniaja sie ciekawe reguly, elementy reguly tworza grupy w ktory produkty sa silnie zwiazne, beda tworzyc jakis komplet:
#    Alkohole - od lekkich po mocne, cukier maka margaryna  - ciasto, ryz wazywa - azjatycka potrawa, pieczywo ser szynka - kanapka
#    Dobrze widac tez to na wykresie gdzie elementy tworza grupy a strzalki zbieraja sie do nastepnika


# b) reguly zawierajace alkohole: canned beer and bottle beer

aParam  = new("APparameter", "target" = "rules",  "confidence" = 0.001, "support" = 0.001, "minlen"= 2, "maxlen"=10, maxtime = 10) 
print(aParam)

#wykrywanie regul z aktulanymi parametrami
asets <-apriori(dataTR,aParam)

# analizuje 'canned beer', 
rulesInGivenConseq<- subset(asets, subset = rhs %in% "canned beer" & lift >=1.0)
tmp <- head(sort(rulesInGivenConseq, by=c( 'support','confidence','lift'), decreasing = TRUE), 10)
inspect(tmp)
plot(tmp, method = 'graph')

# --> {canned beer}, kupowane najczesciej jest z woda, i 'siateczka'

# analizuje 'canned beer' vs bottled 'bottled beer', 
rulesInGivenConseq<- subset(asets, subset = ( (rhs %in% "bottled beer") | (rhs %in% "canned beer")) & lift >=1.0)
tmp <- head(sort(rulesInGivenConseq, by=c( 'support','confidence','lift'), decreasing = TRUE), 15)
inspect(tmp)
plot(tmp, method = 'graph')

# --> {canned beer}, kupowane najczesciej jest z woda, i 'siateczka', z 'bottled beer' gama produktow jest szersza.
#  Mozna by ulozyc siateczki i wode blisko puszek z piwem     


# analizuje 'shopping bags' z jakimi produkatami sa 'siateczki'
# specjalnie uzywam  rhs - aby  strzalki pokazaly w kierunku 'shopping bags'
rulesInGivenConseq<- subset(asets, subset = rhs %in% "shopping bags" & lift >=1.0)
set_sorted_limited <- head(sort(rulesInGivenConseq, by="support", decreasing = TRUE), 15)
inspect(set_sorted_limited)
plot(set_sorted_limited, method = 'graph')

# -> Wsparcia (a takze srednice na wykresie) pokazuja na co zuzywane sa siateczki.
# Mozna je ulozyc blisko tych produktow, lub wymyslic inny sposob pakowania aby ich zuzycie zmniejszyc.


# c) mozliwie dlugie reguly, zawierajace wiele elementow, zeby zobaczyc co w nich jest i co warunkuja 
# Powiekszam parametr minlen od 2 do poki zwracane sa reguly
aParam  = new("APparameter", "target" = "rules",  "confidence" = 0.001, "support" = 0.001, "minlen"= 6, "maxlen"=10, maxtime = 10) 
print(aParam)

#wykrywanie regul z aktualanymi parametrami
asets <-apriori(dataTR,aParam)

# Przeglad regul z najwiekszym wsparciem, od 5 do 15 piewszych regul
tmp = head(sort(asets, by=c('confidence','lift', 'support'), decreasing = TRUE), 5)
inspect(tmp)
plot(tmp, method = 'graph')

#--> Reguly o najwiekszym zaufaniu i podniesieniu maja w nastepniku znane juz 3 gropy produktow (mleko, wazywa), 
#    ich wsparcie jest male, ale moga tworzyc sekwecje, ktora klientowi z takim asorytmentem dobrze podpowiedza dodatkowy produkt.
#    Czyli mozna poprzedniki tych regul uzyc jako warunek do rekomendacji nastepnikow

# d) reguly minimalne ale mozliwie najlepiej 'prowadzace' do swojego nastepnika

#
# 4. Analiza zbioru z uzyciem funkcji interesingMeasure
#
# Chce sprawdzic czy metryki z tej funkcji pozwola wyselekcjonowac ciekawe reguly
#
# a). Parametr improvement 
aParam  = new("APparameter", "target" = "rules",  "confidence" = 0.001, "support" = 0.001, "minlen"= 2, "maxlen"=10, maxtime = 10) 
print(aParam)

#wykrywanie regul z aktualanymi parametrami
asets <-apriori(dataTR,aParam)

#regu??y dla kt??rych wsp????czynnik poprawy jest wi??kszy od 0,01
resTbl <- interestMeasure(asets,"improvement", dataTR)
#resTbl
intres <- which(sapply(resTbl, function(x) {x > 0.1  && x <= 1 })==TRUE)
#length(intres)
#intres
intersRule <- asets[intres] 

tmp <-  head(sort(intersRule, by="lift", decreasing = TRUE),10)
inspect(tmp)
plot(tmp, method = 'graph')

# --> Uzycie wfunkcji interesingMeasure i z uzglednieniem parametru improvement daj te same efekty i prowadzi wnioski jak 
# w punkcje a) 4 - Produkty tworza grupy


#b) Znalezienie najbardziej skorelowanych regul opartych o wspolczynnik korelacji phi 
#   Chce miec prosta miare zeby znalezc jakie reguly sa z soba najbardziej skorelowane.

quality(asets) <- cbind(quality(asets), 
                        phi = interestMeasure(asets, measure = "phi", 
                                                      transactions = dataTR))
inspect(head(sort(asets, by="phi", decreasing = TRUE),15))

tmp <-  head(sort(asets, by="phi", decreasing = TRUE),10)
inspect(tmp)
plot(tmp, method = 'graph')


#--> Wyrazniej skorelowane sa alkohole, wazywa i owoce, maka oraz cukier. 
# Wyniki nie wprowdzaja do analizyw wiele wiecej niz poprzednie eksperymenty.


# Pozdrawiam,
# Pawel Mergist



## KONIEC ###

