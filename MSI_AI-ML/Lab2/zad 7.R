#
# ZAD 7
#

# W tym zadaniu trenuje i testuje na calym zbiorze podobnie jak dla zadania z AND, i XOR.

# a) bez skalowania uczenie jest niestabilne w najlepszym przypadku na 20 AUC bylo 0.79, zazwyczaj 0.5 i loss ok 30 dla calego zbioru
#    Wymaga wiele prob.
#
# Area under the curve: 0.6409

# b) ze skalowaniem rocess sie stabilizuje, w trybie online z AUC ok 0.84 za kazdym razem


#
# Funkcje pomocnicze
#
## Zrodla: #https://www.r-bloggers.com/how-to-build-your-own-neural-network-from-scratch-in-r/

sigmoid <-function(x){
  1.0 / (1.0 + exp(-x))
}
sigmoid_derivative <-function(x){
  x * (1.0  - x)
}



feedforward <- function(nn)
{
  nn$layer1 = sigmoid(nn$input %*% nn$weights1)
  nn$output = sigmoid(nn$layer1 %*% nn$weights2)
  nn
}

backprop <- function(nn) {
  
  ni = 0.06
  
  # obliczenie pochodnej z wyjscia: dL/dy * l1 * (y-output)
  d_weights2 <- (
    t(nn$layer1) %*%
      # `2 * (nn$y - nn$output)` is the derivative of the sigmoid loss function
      (2 * (nn$y - nn$output) *
         sigmoid_derivative(nn$output))
  )
  
  # obliczenie zmiany w warstwie ukrytej z uwzlgednieniem wartosc wejsciowy i bledu z warstwy wyjsciowej
  d_weights1 <- ( 2 * (nn$y - nn$output) * sigmoid_derivative(nn$output)) %*% 
    t(nn$weights2)
  d_weights1 <- d_weights1 * sigmoid_derivative(nn$layer1)
  d_weights1 <- t(nn$input) %*% d_weights1
  
  # modyfikacja wag
  nn$weights1 <- nn$weights1 + ni * d_weights1
  nn$weights2 <- nn$weights2 + ni * d_weights2
  
  nn
}

loss_function <- function(nn) {
  sum((nn$y - nn$output) ^ 2)# * 1.0/dim(nn$input)[1]
}

train_nn_batch <- function(nn, org_X, org_y)
{
  n <- 2000
  my_nn = nn
  
  # zapamietanie straty dla wszystkich krokow nauki 
  loss_df <- data.frame(
    iteration = 1:n,
    loss = vector("numeric", length = n)
  )
  

#  org_X = my_nn$X
#  org_y = my_nn$y

  for (i in seq_len(n)) {
    
    #batch
    s = sample(dim(org_X)[1], 20)
    # OFFLINE
    my_nn$X = org_X[s,]
    my_nn$y = org_y[s]
    
    #ONLINE
    my_nn$X = org_X
    my_nn$y = org_y
    my_nn$output = matrix(rep(0, times = length(my_nn$y)))

    my_nn <- feedforward(my_nn)
    my_nn <- backprop(my_nn)
    
    # skladowanie bledu
    loss_df$loss[i] <- loss_function(my_nn)
  }
  
  #my_nn$X = org_X
  #my_nn$y = org_y
  
  my_nn$loss_df = loss_df
  return (my_nn)
}

predict <- function(X,w1,w2)
{ 
  # dodanie jedynek do X 
  xb1 = c(rep(1, dim(X)[1])) 
  xb1
  X = cbind(X, xb1)
  X
  
  nn = list(input=X, weights1 = w1, weights2 = w2 )
  nn = feedforward(nn)
  nn$output
}

data(cats, package = "MASS")


X = data.matrix(cats[,-1],rownames.force = NA) 


## a) bez skalownania - zakomentowac apply
## b) ze skalowaniem

X = apply(X, MARGIN = 2, FUN = function(X) (X - min(X))/diff(range(X)))

y = data.matrix(cats[,1], rownames.force = NA) 
y[y[,1] == "F", ] <- 1
y = as.numeric((cats$Sex == 'F'))
y

##
#X = matrix( c(0,0, 1,0, 0,1, 1,1), nrow=4, ncol=2, byrow = TRUE)
# dodanie jedynek do X 

xb1 = c(rep(1, dim(X)[1])) 
xb1
X = cbind(X, xb1)
X
# Wyjscie AND
#y <- c(0, 0, 0, 1)
y

#t = sample(dim(X)[1]*.7, )
t = 1:dim(X)[1]
X = X[t,]
X

y = y[t]
y

# Wejscia - wyjcie
cbind(X, y)


# Wagi

# Warstwa wejsciowa
h_size = 10
w =  rand_vector <- runif(ncol(X) * h_size)
w1  <- matrix(
  rand_vector,
  nrow = ncol(X),
  ncol = h_size,
  byrow = TRUE
)

w1

# Warstwa ukryta
rand_vector <- matrix(runif(h_size), ncol = 1)
w2 = rand_vector
w2
X
nn = list(input=X, weights1 = w1, weights2 = w2,  y = y, output = matrix(rep(0, times = length(y), ncol = 1) ))
nn$input
nn$y

nn$weights1
nn$weights2

nn$input %*% nn$weights1

nn$layer1 = sigmoid(nn$input %*% nn$weights1)
dim(nn$layer1)
nn$output = sigmoid(nn$layer1 %*% nn$weights2)
nn$layer1 %*% nn$weights2
#nn = train_nn(nn)
nn = train_nn_batch(nn, X, y)



### draw
my_nn = nn

res = data.frame(
  "Predicted" = round(my_nn$output, 3),
  "Actual" = y
)
res

# plot the cost
library(ggplot2)
loss_df = my_nn$loss_df
ggplot(data = loss_df, aes(x = iteration, y = loss)) +
  geom_line()

###


## TEST NA CALYM ZBIORZE
library(pROC)
roc(res$Actual, res$Predicted, plot=TRUE, print.auc=TRUE)


