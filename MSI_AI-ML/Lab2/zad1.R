#
#  ZADANIE 1
# 

perceptron <- function(w1,w2,w3, x1,x2)
{
  y <- w1*x1 + w2*x2 + w3
  #Pokazanie wyliczenia
  #t = sprintf("%2.2f * %2.2f + %2.2f * %2.2f + b%2.2f = %2.2f", w1,x1, w2,x2, w3 , y)
  
  #print (t)
  
  # Funckcja aktywacji
  if(y>0){
    1
  } else {
    0
  }
}

# Dla ulatwwienia sprawdzenie wyszytkich kombinacji dla danej operacji logicznej
check_per  <-  function(w1,w2,w3)
{
  print(perceptron(w1,w2,w3,0,0))
  print(perceptron(w1,w2,w3,0,1))
  print(perceptron(w1,w2,w3,1,0))
  print(perceptron(w1,w2,w3,1,1))
  
  print ("NOT:")
  print(perceptron(w1,w2,w3,1,0))
  print(perceptron(w1,w2,w3,0,0))
}

# AND
# Tu podstawiam rozne wagi i sprawdzam co wychodzi dla oczekiwanej operacji
check_per(2, 2, -2)

# Perceptron dla AND
and <- function(x1,x2)
{
  return (perceptron(2,2,-2, x1, x2))
}

# Test
and(0,0)
and(0,1)
and(1,0)
and(1,1)


# OR
# Tu podstawiam rozne wagi i sprawdzam co wychodzi dla oczekiwanej operacji
check_per(1, 1, 0)

# Perceptron dla OR
or <- function(x1,x2)
{
  return (perceptron(1,1,0, x1, x2))
}
# Test
or(0,0)
or(0,1)
or(1,0)
or(1,1)

# NOT
check_per(-1, 0, 1)
not <- function(x1)
{
  return (perceptron(-1,0,1, x1, 0))
}
#TEST
not(1)
not(0)

#NAND
nand <- function(x1,x2)
{
  not(and(x1,x2))
}
nand(0,0)
nand(0,1)
nand(1,0)
nand(1,1)

#XOR

# Siec na perceptronach obliczajaca XOR 
xor <- function(x1,x2)
{
  #A XOR B = (A NAND (A NAND B)) NAND (B NAND (A NAND B))
  y = nand(nand(x1, nand(x1,x2)) , nand(x2, nand(x1,x2)))
  print("XOR:")
  t = sprintf("xor: %2.2f xor %2.2f -> %2.2f", x1, x2, y)
  print(t)
}
#TEST
xor(0,0)
xor(0,1)
xor(1,0)
xor(1,1)

