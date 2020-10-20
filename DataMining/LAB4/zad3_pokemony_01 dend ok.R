#############
# Zadanie 1 #
#############

library(factoextra)

#pokemony
#opis: https://www.kaggle.com/abcsds/pokemon

# Find which two variables can define the type of a pokemon, if any. ??

#download.file('http://staff.ii.pw.edu.pl/~rbembeni/dane/Pokemon.csv','Pokemon.csv')
pokemon <- read.csv("Pokemon.csv")



remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}
# Spawdzenie nuly
str(pokemon)
sum(is.na(pokemon))
# -> Nie ma brakujacych wartosci liczbowych
#    W kolumnie Type.2 moze nie byc ustawiony typ - to nie jest blad, wynika z opisu danych
#

# Przeksztalczenie atrybotow na odpowiednie typy
str(pokemon)
dim(pokemon)
data  = pokemon[3:dim(pokemon)[2]]
data

data$Type.1 = factor(data$Type.1 )
data$Type.2 = factor(data$Type.2 )

str(data)
sum(is.na(data))
#
# 1. Sprawdzenie dendogramu
#

#wyliczenie macierzy odleg??o??ci/niepodobie??stwa
distM = dist(data, method='euclidean')
distT = as.matrix(distM)
dim(distT)
distT[1:5,1:5]
#wykonanie grupowania hierarchicznego
data.hc <- hclust(distM, method="complete")

# Pokazanie dendogramu

# UWAGA: - ilosc pokemonow jest bardzo duza wiec dendogram moze byc nie czytelny - 
# - chodzi o znalezienie rozgalezien na jeggo czubku

plot(data.hc, hang = -1, labels=pokemon$X.)
rect.hclust(data.hc, k=3, border=2:4)

# WNIOSEK: Zarysowuja sie 3 grupy - 2 duze i 1 mala

# Utworzenie grup
?cutree
data.hc.groups <- cutree(data.hc, k=3)

# Licznosci tych grup
table(data.hc.groups)
data.hc.groups


pokemon_data_only = pokemon[c("HP", 'Attack', 'Defense', 'Sp..Atk', 'Sp..Def', 'Speed')]
head(pokemon_data_only)

boxplot(pokemon_data_only, ylim = c(0,300), main = "All pokemons ")

boxplot(pokemon_data_only[which(data.hc.groups == 1),], ylim = c(0,300), main = "Group 1")

boxplot(pokemon_data_only[which(data.hc.groups == 2),], ylim = c(0,300), main = "Group 2")

boxplot(pokemon_data_only[which(data.hc.groups == 3),], ylim = c(0,300), main = "Group 3")

# WNIOSKI: Mediana parametrow pokemonow jest wieksza dla kolejnoej grupy. 

# 
# 2. Adnalize z uzyciem k-means
#

pokemon <- read.csv("Pokemon.csv")

# Przygotowanie danyh
row.has.na <- apply(pokemon, 1, function(x){any(is.na(x))})
row.has.na 
pokemon
sum(is.na(pokemon))
#pokemon = pokemon[!row.has.na,]


# Wyodrebnienie atrybutiow 
poke.atr <- pokemon[3:dim(pokemon)[2]]
head(poke.atr)

#Obenij Type.1 ,.2, i kolumne Legendary
numeric_cols = c("HP", 'Attack', 'Defense', 'Sp..Atk', 'Sp..Def', 'Speed')

poke.atr.numeric = poke.atr[numeric_cols]
sum(is.na(poke.atr.numeric))

#
# Metoda k-mean gorzej radzi sowbie z  wartosciami odstajacymi - przygotowuje 2 zestawy 
# bez i z wartosciami odstajacymi
#
poke.atr.numeric_noOUTLINERS = poke.atr.numeric
head(poke.atr.numeric)
for (i in numeric_cols)
{
  print (i)
  v = unlist(as.vector(poke.atr.numeric[i]))

  q1 = quantile(v, 0.25)    
  q3 = quantile(v, 0.75)    
  iqr = q3 - q1
  mn = q1 - 1.5 * iqr
  mx = q3 + 1.5 * iqr
  #print(q1)
  #print(q3)

  poke.atr.numeric_noOUTLINERS[which(poke.atr.numeric_noOUTLINERS[i] < mn),i] = mn
  poke.atr.numeric_noOUTLINERS[which(poke.atr.numeric_noOUTLINERS[i] > mx),i] = mx
  #x = poke.atr.numeric$HP
  #poke.atr.numeric_rm_outliners$HP = x[!x %in% boxplot.stats(x)$out]
  
}

# Porownanie rozkaldow przed i po odsianiu outlinerow
boxplot(poke.atr.numeric)
boxplot(poke.atr.numeric_noOUTLINERS)

sum(is.na(poke.atr.numeric_noOUTLINERS))


# Atrybuty kategoryczne -> hot encoding

library(caret)
dmy <- dummyVars(" ~ Type.1 + Type.2 + Legendary", data = poke.atr)
poke.atr.categorical = data.frame(predict(dmy, newdata = poke.atr))


merge_and_scale <- function(poke.atr.categorical, poke.atr.numeric)
{
  
  # polaczenie w jeden dataframe
  poke.atr_cat_num = cbind(poke.atr.categorical, poke.atr.numeric)
  str(poke.atr_cat_num)
  
  
  
  # SKALOWANIE
  poke.atr_scaled <- as.data.frame(lapply(poke.atr_cat_num, scale))
  poke.atr_scaled 
  
  row.has.na <- apply(poke.atr_scaled, 1, function(x){any(is.na(x))})
  row.has.na
  poke.atr_scaled_na = poke.atr_scaled[!row.has.na,]
  #poke.atr_scaled_na = poke.atr_scaled
  
  poke.atr_scaled_na[is.na(poke.atr_scaled_na)] <- 0
  
  sum(is.na(poke.atr_cat_num))
  sum(is.na(poke.atr_scaled_na))
  
  return (poke.atr_scaled_na)
  
  
}


# polaczenie w jeden dataframe
poke.atr_cat_num = cbind(poke.atr.categorical, poke.atr.numeric)
str(poke.atr_cat_num)


# SKALOWANIE
poke.atr_scaled <- as.data.frame(lapply(poke.atr_cat_num, scale))
poke.atr_scaled 

row.has.na <- apply(poke.atr_scaled, 1, function(x){any(is.na(x))})
row.has.na
poke.atr_scaled_na = poke.atr_scaled[!row.has.na,]
#poke.atr_scaled_na = poke.atr_scaled

poke.atr_scaled_na[is.na(poke.atr_scaled_na)] <- 0

sum(is.na(poke.atr_cat_num))
sum(is.na(poke.atr_scaled_na))

library(factoextra)
#distance <- get_dist(poke.atr_scaled[sample(1:10)])
#head(distance)
#fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

poke_atr.with_outliners = merge_and_scale(poke.atr.categorical, poke.atr.numeric)

poke_atr.with_NO_outliners = merge_and_scale(poke.atr.categorical, poke.atr.numeric_noOUTLINERS)

kmeans_cluster <- function(pokemon, poke.atr_scaled_na, title = "NOTITLE",centers = 4, iter.max = 20, nstart=5)
{
pokemon.kmeans = kmeans(poke.atr_scaled_na,centers, iter.max, nstart)
pokemon.kmeans
#pozyskanie informacji nt. grupowania
print(pokemon.kmeans)
print(pokemon.kmeans$iter)
print(pokemon.kmeans$centers)
print(pokemon.kmeans$cluster)
print(pokemon.kmeans$withinss)
print(pokemon.kmeans$size)

pokemon$cluster = pokemon.kmeans$cluster


fviz_cluster(pokemon.kmeans, data = poke.atr_scaled_na)

boxplot(poke.atr.numeric[which(pokemon.kmeans$cluster == 1),], ylim = c(0,300), main =title)

boxplot(poke.atr.numeric[which(pokemon.kmeans$cluster == 2),], ylim = c(0,300), main = title)

boxplot(poke.atr.numeric[which(pokemon.kmeans$cluster == 3),], ylim = c(0,300), main = title)

#fviz_cluster(pokemon.kmeans, data = df)


#fviz_cluster(pokemon.kmeans, data = poke.atr.numeric, repel=FALSE, labelsize=0, ellipse.type = "convex")

return (pokemon.kmeans)
}


# KLASTYRY Z OUTLINERAMI
k2 = kmeans_cluster(pokemon, poke_atr.with_outliners, title='poke_atr.with_outliners')

fviz_cluster(k2, data = poke_atr.with_outliners, repel=FALSE, labelsize=0, ellipse.type = "convex")

# # KLASTYRY Z BEZ OUTLINER'OW
k2 = kmeans_cluster(pokemon, poke.atr.numeric_noOUTLINERS, title='poke.atr.numeric_noOUTLINERS')

fviz_cluster(k2, data = poke.atr.numeric_noOUTLINERS, repel=FALSE, labelsize=0, ellipse.type = "convex")


#######################################################
# znajdowanie optymalnej liczby grup - metoda '??okcia'#
#######################################################

# Inicjalizuj ca??kowita sume bledu kwadratowego: wss
wss <- 0

# Od 1 do 15 grup
for (i in 1:15) {
  km.out <- kmeans(poke.atr.numeric_out, centers = i, nstart=20)
  # Zapisz ca??kowita sume bledu kwadratowego do zmiennej wss
  wss[i] <- km.out$tot.withinss
}

# Narysuj ca??kowita sume bledu kwadratowego wzgledem liczby grup
plot(1:15, wss, type = "b", 
     xlab = "Liczba grup", 
     ylab = "Suma bledu kwadratowego wewnatrz grup")

### pm komnt!!!! miejsce gdzie blad jest najmniejszy a licba grup juz istotnie nie przyrasta




##
## DB SCAN
##

poke.dbs = dbscan(poke_atr.with_outliners, eps=5.6, MinPts=5)
table(poke.dbs$cluster)

fviz_cluster(poke.dbs, data = poke.atr_scaled, repel=FALSE, labelsize=0, ellipse.type = "convex")

## WNIOSEK: KLASTROWANIE TA METODA NIE PRZYNOSI CIEKAWYCH EFEKTOW, PONIEWAZ TWORZY DUZO NIE ROWNYCH GRUP NP:
"
> table(poke.dbs$cluster)

0   1   2   3   4   5 
790   2   2   2   2   2 
> poke.dbs = dbscan(poke_atr.with_outliners, eps=0.6, MinPts=2)
> table(poke.dbs$cluster)

0   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18  19  20  21  22  23  24  25 
729   6   3  10   2   2   5   2   3   4   2   2   2   2   2   2   2   2   2   2   2   2   2   2   2   4 
> poke.dbs = dbscan(poke_atr.with_outliners, eps=0.6, MinPts=5)
> table(poke.dbs$cluster)

0   1   2 
790   5   5 
> poke.dbs = dbscan(poke_atr.with_outliners, eps=1.6, MinPts=5)
> table(poke.dbs$cluster)

0   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16 
512  12  24  48  10  21  51  11  24   8  12  18  28   5   6   5   5 
> fviz_cluster(poke.dbs, data = poke.atr_scaled, repel=FALSE, labelsize=0, ellipse.type = "convex"
"

