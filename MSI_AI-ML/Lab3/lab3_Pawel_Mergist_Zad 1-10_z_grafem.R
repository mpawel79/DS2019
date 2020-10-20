
#
# ZRODLA: GA: A Package for Genetic Algorithms in R.pdf
# Algorytmy geneczyczne i ich zastosowania. Davis E. Goldberg

# ZAD 1

#cities = read.csv("./cities.csv", header = TRUE, sep=",")
# View(cities)

## * FUNKCJE POMOCNICZE - RYSOWANIE TRASY
 ###
 data("eurodist", package = "datasets")
 D <- as.matrix(eurodist)
 D
 cities = D
 mds <- cmdscale(eurodist)
 mds
 
 draw_route <- function(route, title="")
 {
   mds <- cmdscale(eurodist)
   x <- mds[, 1]
   y <- -mds[, 2]
   plot(x, y, type = "n", asp = 1, xlab = "", ylab = "", main=title)
   abline(h = pretty(range(x), 10), v = pretty(range(y), 10),
          col = "light gray")
   
   D <- as.matrix(eurodist)
   
   print("Route:")
   print (colnames(cities)[route])
   
   print("Length:")
   print(totalDistance(route, D))
   #apply(route, 1, costFunction, D)
   
   #tour <- GA@solution[1, ]
   tour <- route
   tour <- c(tour, tour[1])
   n <- length(tour)
   arrows(x[tour[-n]], y[tour[-n]], x[tour[-1]], y[tour[-1]],
          length = 0.15, angle = 25, col = "steelblue", lwd = 2)
   text(x, y, labels(eurodist), cex=0.8)
 }
 
 
# ZAD 2
 
# Poznanie funkcji - TESTY 
 x <- 1:22
x
  embed ((x), 7)

t(  embed (rev(x), 5))

visitedCities = c('London','Rome','Berlin')
visitedCities
visitedCities = c(visitedCities, visitedCities[1])


route = embed(visitedCities, 2)[, 2:1]
route
cities[route]
cities["London", "Rome"]



# Implementacja - wyliczenie dystansu pomiedzy petla miast, start z pierwszego 
# i powrot do pierwszego
totalDistance = function(visitedCities, distances=cities) {
  visitedCities = c(visitedCities, visitedCities[1])
  route = embed(visitedCities, 2)[, 2:1]
  distancesSum = sum(distances[route])
  return(distancesSum)
}

totalDistance(c('London','Rome','Berlin'))

# ZAD 3
# Przeszukiwanie wszystkich kombinacji dla zadanej listy miast

permu <- function(perm,fun,current=NULL){
  for(i in 1: length(perm)){
    fix <- c(current,perm[i])
    #    calculated    elements;    fix    at    this    point
    rest <- perm[-i]  # elements yet to permutate
    #print ('.')
    #Call callback.
    if(!length(rest)){
      result <- fun(fix)
      if(result<bestResult){
        assign("bestResult", result, envir = .GlobalEnv)
        print(bestResult)
      }
    }
    if(length(rest)){
      result <- permu(rest, fun, fix)
    }
  }
}

# Globalna zmienna, wynik ma byc mniejszy od niej
#totalDistance(c('London','Rome','Berlin'))
#colnames(cities)[1:5]

citiesToVisit = colnames(cities)[1:9]
#citiesToVisit = c('London','Rome','Berlin')
bestResult <- 99999
permu(citiesToVisit, totalDistance)

print ("Cities to visit:")
citiesToVisit
print('Permutation bestResut:')
print(bestResult)

# ODP: Dla 9 miast min dystans znaleziony po paru minutach to  6678.55.
# Wiec maksymalna sensowna wartosc dla laptopa to 8.


# ZAD 4

# ilosc permutacji do sprawdzenia w funkcji
factorial(length(c('Milan','Rome','Berlin')))

factorial(1:10)
# MAX 11 MIAST
# Aproksymata zlozonosci...

# ODP: Przy 10 parametrach ilosc wynosi 3628800 kombinacji. Przyrost jest n!

# ZAD 5

#install.packages("GA")
library(GA)

# ZAD 6

costFunction <- function(cfVisitedCities)
{
 return(-totalDistance(cfVisitedCities))
}

# ZAD 7
# Zapoznalem sie dokumentacaj GA - Na podstawie opisU  z: Journal of Statistical Software
# April 2013, Volume 53, Issue 4. http://www.jstatsoft.org/

# ODP: 
# a) 1. Wartosci min max: musi byc zgodne z iloscia miast ktore faktycznie chcemy odwiedzic. 
#    2. Krzyzowanie moze 'przecinac' optymalne podsekwencje opozniajac najkrotszej drogi
#    3. Mutacja takze moze 'psuc' optymalne podsekwencje
# b) Tak, zastosowanie typu 'permutation' jest nie uniknione.
# c), d) - dosle rozwiaznanie jak wymysle.
# 

gaControl()

citiesToVisit = colnames(cities)[1:15]
citiesToVisit

pm = 0.2
pco = 0.8
elitism =0.01
popSize = 50


GA <- ga(type = "permutation", fitness = costFunction, min = 1, max =
       length(citiesToVisit), pmutation = pm, pcrossover = pco, elitism =
       elitism, popSize = popSize, maxiter = 100)

summary(GA)
plot(GA)
#GA@solution
# WYNIK GA
print (sprintf("***GA solution: %2.2f",  totalDistance(GA@solution[1,])))




#
# Obliczenie najkrotszej lub jakies drogi - DLA POROWNANIA TYLKO
#
print ("Cities to visit:")
citiesToVisit
text = sprintf("ADHOC distance: %2.2f",  totalDistance(citiesToVisit))
print (text)
draw_route(citiesToVisit, text)


# RANDOM ROUTE
r_route = sample(citiesToVisit)
text = sprintf("Random route: %2.2f",  totalDistance(r_route))
print (text)
print (r_route)
draw_route(r_route, text)


bestResult <- 99999
# ZAKOMENTOWNE JEZELI WIECEJ NIZ 9 MIAST
# permu(citiesToVisit, totalDistance)
#print (sprintf("Permutation check: %2.2f",  totalDistance(sample(bestResult))))

# WYNIK GA
text = sprintf("***GA solution: %2.2f",  totalDistance(GA@solution[1,]))
print (text)
r =  GA@solution[1, ]
print (colnames(cities)[r])
draw_route(r, text)
###



# ZAD 8


citiesToVisit = colnames(cities)[1:10]


ep = c(
pm = 0.2,
pco = 0.8,
elitism =0.01,
popSize = 100)
#mp = matrix()
#mp = append(mp, ep)
#mp = matrix(c(0.2, 0.8, 0.01, 100), byrow=TRUE, dimnames = list(c("pm", "pco", "elitism", "popSize" )))
set.seed(123)
mp = matrix(c(0.2, 0.8, 0.01, 5,  
              0.2, 0.8, 0.01, 20,
              0.2, 0.8, 0.01, 100,
              0.2, 0.8, 0.1, 30,
              0.2, 0.2, 0.5, 40,
              0.8, 0.8, 0.8, 80
), 
            ncol=4, byrow=TRUE,  dimnames = list(c(), c("pm",     "pco", "elitism", "popSize" )))

mp = cbind(mp, d = c(d=0))

mp[2,'d'] = 5
mp
for (i in seq(1, dim(mp)[1]))
{

    r = mp[i,]
  print (r)
  

GA <- ga(type = "permutation", fitness = costFunction, min = 1, max = length(cities), 
          pmutation = r["pm"], 
          pcrossover = r["pco"],
          elitism = r["elitism"], 
          popSize = r["popSize"])
         

#summary(GA)
# WYNIK GA
d = totalDistance(GA@solution[1,])
mp[i,"d"] = d
text = sprintf("d=%2f, pm=%2.0f, pco=%2.1f, elitism=%2.1f, popsize=%2.1f",d,r["pm"], r["pco"], r["elitism"], r["popSize"])
print (text)

#print (sprintf("***GA solution: %2.2f",  d)

plot(GA, main=text, ylim=c(-32000,-18000))

}

mp
# ODP: Najlepszy wynik dla pm pco elitism popSize: [3,] 0.2 0.8    0.01     100 18543.91 ***
#
#pm pco elitism popSize        d
#[1,] 0.2 0.8    0.01       5 18694.87 **
#[2,] 0.2 0.8    0.01      20 18801.45 *
#[3,] 0.2 0.8    0.01     100 18543.91 ***
#[4,] 0.2 0.8    0.10      30 21008.40
#[5,] 0.2 0.2    0.50      40 19500.31
#[6,] 0.8 0.8    0.80      80 19979.05


# ZAD 9


r = c(
  pm = 0.2,
  pco = 0.8,
  elitism =0.01,
  popSize = 100)

results = c()
distances=c()
for (i in seq(1:3))
{

  GA <- ga(type = "permutation", fitness = costFunction, min = 1, max = length(cities), 
         pmutation = r["pm"], 
         pcrossover = r["pco"],
         elitism = r["elitism"], 
         popSize = r["popSize"])

  results = append(results, GA@fitnessValue)
  distances = appen(distances,  totalDistance(GA@solution[1,]))
}

# ODP: Srednia fitnessValue w okolicach -21.5 tys., Wykres i podsumowanie ponizej 
hist(results)
summary(results)

#res1fitnesses = sapply(results, function(x){x@fitnessValue}) > hist(res1fitnesses)
#summary(res1fitnesses)


# ZAD 10

# ODP: Wynik jest jeszcze o ok 15% lepszy niz w przypadku bez dodatkowej optymalizacji.
#      Obliczenia i wyniki:
#
# Funkcja kosztu dla alg geneccznego szukajacego parametrow dla glownego AG do optymalizacji dystansu

citiesToVisit = colnames(cities)[1:10]

dCostFunction <- function(x)
{
  ga(type = "permutation", fitness = costFunction, min = 1, max = length(citiesToVisit), 
     pmutation = x[1], 
     pcrossover = x[2],
     elitism = x[3], 
     popSize = x[4])@fitnessValue
  
  
}

## Wyszukanie optymalnych parametrow
final = ga(type="real-valued", fitness=dCostFunction,
           min = c(0.01, 0.7, 0.01, 80), max = c(0.2, 0.9, 0.10,
                                                 90),
           popSize=5, maxiter = 10)
##
final@population 
final@fitness
params = final@solution


## 3. Optymalne parametry znalezione - zastosuje je

GA = ga(type = "permutation",
   fitness = costFunction, min = 1, max = length(citiesToVisit), pmutation = params[1],
   pcrossover = params[2],
   elitism = params[3],
   popSize = params[4], maxiter = 1500)

plot(GA)#, main=text, ylim=c(-32000,-18000))

GA@popSize

text = sprintf("d=%2.1f, pm=%0f, pco=%2.3f, elitism=%2.3f, popsize=%2.3f",totalDistance(GA@solution[1,]),GA@pmutation, GA@pcrossover, GA@elitism, GA@popSize)
print (text)

#print (sprintf("***GA solution: %2.2f",  d)

plot(GA, main=text)#, ylim=c(-32000,-18000))

### Pokazanie tras

#
# Obliczenie najkrotszej lub jakies drogi - DLA POROWNANIA TYLKO
#
print ("Cities to visit:")
citiesToVisit
text = sprintf("ADHOC distance: %2.2f",  totalDistance(citiesToVisit))
print (text)
draw_route(citiesToVisit, text)


# RANDOM ROUTE
r_route = sample(citiesToVisit)
text = sprintf("Random route: %2.2f",  totalDistance(r_route))
print (text)
print (r_route)
draw_route(r_route, text)


bestResult <- 99999
# ZAKOMENTOWNE JEZELI WIECEJ NIZ 9 MIAST
# permu(citiesToVisit, totalDistance)
#print (sprintf("Permutation check: %2.2f",  totalDistance(sample(bestResult))))

# WYNIK GA
text = sprintf("***GA solution: %2.2f",  totalDistance(GA@solution[1,]))
print (text)
r =  GA@solution[1, ]
print (colnames(cities)[r])
draw_route(r, text)






