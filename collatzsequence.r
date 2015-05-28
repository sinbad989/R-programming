#The simplest impossible mathematical problem

#Collatz sequence:
collatz = function(x){
  if (x %% 2 == 0){
    return (x/2)
  }
  else{
    return (3*x + 1)
  }
}

num = 0
x = readline()
u = as.integer(x)
while (num != 1){
  num = collatz(u)
  u = num
  print(u)
}


