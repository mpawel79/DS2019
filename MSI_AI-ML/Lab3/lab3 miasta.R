
# ZAD 1

cities = read.csv("./cities.csv", header = TRUE, sep=",")
# View(cities)


# ZAD 2


totalDistance = function(visitedCities, distances=cities) {
  visitedCities = c(visitedCities, visitedCities[1])
  route = embed(visitedCities, 2)[, 2:1]
  distancesSum = sum(distances[route])
  return(distancesSum)
}

totalDistance(c('London','Rome','Berlin'))

# ZAD 3

permu <- function(perm,fun,current=NULL){
  for(i in 1: length(perm)){
    fix <- c(current,perm[i])
    #    calculated    elements;    fix    at    this    point
    rest <- perm[-i]  # elements yet to permutate
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

bestResult <- 99999
totalDistance(c('London','Rome','Berlin'))

permu(c('Paris','Madrid','Berlin'), totalDistance)



# ZAD 4

# ilosc permutacji do sprawdzenia w funkcji
factorial(length(c('London','Rome','Berlin')))
factorial(24)
# MAX 11 MIAST
# Aproksymata zlozonosci...


# ZAD 5

#install.packages("GA")
library(GA)


costFunction <- function(cfVisitedCities)
{
 return(-totalDistance(cfVisitedCities))
}

# ZAD 6

pm = 0.2
pco = 0.8
elitism =0.30
popSize = 50

GA <- ga(type = "permutation", fitness = costFunction, min = 1, max =
       length(cities), pmutation = pm, pcrossover = pco, elitism =
       elitism, popSize = popSize)
summary(GA)


plot(GA)
GA@solution

colnames(cities)[GA@solution]

# ZAD 7
