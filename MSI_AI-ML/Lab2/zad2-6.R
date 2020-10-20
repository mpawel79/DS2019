#
# Zad 2 - 5
#
# Funkcje pomocnicze
sigmoid <-function(x){
  1.0 / (1.0 + exp(-x))
}
sigmoid_derivative <-function(x){
  x * (1.0  - x)
}


# Zbior treningowy XOR
X = matrix( c(0,0, 1,0, 0,1, 1,1), nrow=4, ncol=2, byrow = TRUE)
# dodanie jedynek do X 
xb1 = c(rep(1, dim(X)[1])) 
xb1
X = cbind(X, xb1)
X
# Wyjscie XOR
y <- c(0, 1, 1, 0)

# Wejscia - wyjcie
cbind(X, y)

# Wagi

# Warstwa wejsciowa
w =  rand_vector <- runif(ncol(X) * nrow(X))
w1  <- matrix(
  rand_vector,
  nrow = ncol(X),
  ncol = nrow(X),
  byrow = TRUE
)

w1

# Warstwa ukryta
rand_vector <- matrix(runif(4), ncol = 1)
w2 = rand_vector
w2
X
nn = list(input=X, weights1 = w1, weights2 = w2,  y = y, output = matrix(rep(0, times = 4), ncol = 1) )
nn$input
nn$y

nn$weights1
nn$weights2

# Zad 3 - feed forward
#testy
out_layer1 = nn$input %*% nn$weights1
out_layer1
output = out_layer1 %*% nn$weights2
output
# implementacja
feedforward <- function(nn)
{
  nn$layer1 = sigmoid(nn$input %*% nn$weights1)
  nn$output = sigmoid(nn$layer1 %*% nn$weights2)
  nn
}

# TEST
nn = feedforward(nn)
nn$output

loss_function <- function(nn) {
  sum((nn$y - nn$output) ^ 2)
}



# ZAD 4 
backprop2 <- function(nn) {
  
  ni = 0.001
  
  # obliczenie pochodnej z wyjcia: dL/dy * l1 * (y-output)
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
  nn$weights1 <- nn$weights1 + ni * d_weights1 * 1.0/dim(nn$input)[1]
  nn$weights2 <- nn$weights2 + ni * d_weights2 * 1.0/dim(nn$input)[1]
  
  nn
}
backprop <- function(nn) {
  
  # obliczenie pochodnej z wyjcia: dL/dy * l1 * (y-output)
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
  nn$weights1 <- nn$weights1 + d_weights1
  nn$weights2 <- nn$weights2 + d_weights2
  
  nn
}
## test
backprop(nn)

## Zad 3 - train
train_nn <- function(nn)
{
n <- 1500
my_nn = nn

loss_df <- data.frame(
  iteration = 1:n,
  loss = vector("numeric", length = n)
)

for (i in seq_len(1500)) {
  my_nn <- feedforward(my_nn)
  my_nn <- backprop(my_nn)
  
  # zapamietanie bledow
  loss_df$loss[i] <- loss_function(my_nn)
}

my_nn$loss_df = loss_df
return (my_nn)
}

#nn = feedforward(my_nn)
nn = train_nn(nn)

### Narysowanie bledu
my_nn = nn
data.frame(
  "Predicted" = round(my_nn$output, 3),
  "Actual" = y
)
##   Predicted Actual
## 1     0.017      0
## 2     0.975      1
## 3     0.982      1
## 4     0.024      0
#Above you can see that the predicted values are fairly close to the actual observed values.

#The plot below displays the result of the loss function as the model is trained. The objective of the model training is to minimize the result of the loss function (Y axis), and we can see that as we progress in iterations (X axis), the result of the loss function approaches zero.

# plot the cost
library(ggplot2)

ggplot(data = my_nn$loss_df, aes(x = iteration, y = loss)) +
  geom_line()

# ZAD 5

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

X = matrix( c(1,1), nrow=1, ncol=2, byrow = TRUE)
predict(X, nn$weights1, nn$weights2)

# ZAD 6

# Zbior treningowy AND
X = matrix( c(0,0, 1,0, 0,1, 1,1), nrow=4, ncol=2, byrow = TRUE)
# dodanie jedynek do X 
xb1 = c(rep(1, dim(X)[1])) 
xb1
X = cbind(X, xb1)
X
# Wyjscie AND
y <- c(0, 0, 0, 1)

# Wejscia - wyjcie
cbind(X, y)

# Wagi

# Warstwa wejsciowa
w =  rand_vector <- runif(ncol(X) * nrow(X))
w1  <- matrix(
  rand_vector,
  nrow = ncol(X),
  ncol = nrow(X),
  byrow = TRUE
)

w1

# Warstwa ukryta
rand_vector <- matrix(runif(4), ncol = 1)
w2 = rand_vector
w2
X
nn = list(input=X, weights1 = w1, weights2 = w2,  y = y, output = matrix(rep(0, times = 4), ncol = 1) )
nn$input
nn$y

nn$weights1
nn$weights2

nn = train_nn(nn)

# AND test
X = matrix( c(0,0, 1,0, 0,1, 1,1), nrow=4, ncol=2, byrow = TRUE)
X
print ("AND test - predict WYNIKI:")
predict(X, nn$weights1, nn$weights2)

##
## XOR
## 

# Zbior treningowy AND
X = matrix( c(0,0, 1,0, 0,1, 1,1), nrow=4, ncol=2, byrow = TRUE)
# dodanie jedynek do X 
xb1 = c(rep(1, dim(X)[1])) 
xb1
X = cbind(X, xb1)
X
# Wyjscie AND
y <- c(0, 1, 1, 0)

# Wejscia - wyjcie
cbind(X, y)

# Wagi

# Warstwa wejsciowa
w =  rand_vector <- runif(ncol(X) * nrow(X))
w1  <- matrix(
  rand_vector,
  nrow = ncol(X),
  ncol = nrow(X),
  byrow = TRUE
)

w1

# Warstwa ukryta
rand_vector <- matrix(runif(4), ncol = 1)
w2 = rand_vector
w2
X
nn = list(input=X, weights1 = w1, weights2 = w2,  y = y, output = matrix(rep(0, times = 4), ncol = 1) )
nn$input
nn$y

nn$weights1
nn$weights2

nn = train_nn(nn)

# XOR test
X = matrix( c(0,0, 1,0, 0,1, 1,1), nrow=4, ncol=2, byrow = TRUE)
X
print ("XOR test - predict WYNIKI:")
cbind(X, predict(X, nn$weights1, nn$weights2))

