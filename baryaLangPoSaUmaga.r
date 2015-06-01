# Filename: baryalngposaumaga.r
# Date    : 5/28/2015

# This program simulates the number of 
# coins a jeepney can have as it travels from Ramos to Talamban

# Initial Variables
type_pass = c('rich','middle','poor')
denomination = c(1000,500,200,100,50,20,10,5,1)
distance = c(5,10,15,20,25,30,35,40)


# Random generation of the passenger and their corresponding money
for (i in 1:100){
  class_pass = c(class_pass,sample(type_pass,1))
  money_limit = ifelse(class_pass[i]=='rich',1000,ifelse(class_pass[i]=='poor',100,500))
  money_pass = c(money_pass,sample(money_limit,1))
}

data = data.frame(cbind(class_pass,money_pass))

# This function converts the passenger money into its respective denomination
rem = c()
for (i in 1:100){
  money = money_pass[i]
  for (x in denomination){
    rem = c(rem,money%/% x)
    money = money-(money%/%x)*x
  } 
}





#TO BE CONTINUED



# higher probability for 5,1, and 10 
# on the Bill side the 20 has a higher prob. than 100 or 200
# the number of passenger on a single stop also varies
# the probability of decreases as the number of passenger on a single stop increases
# take note also on the limit of the jeepney: around 20 
# for (x in distance){
#   cat('travelling ...',x,'Kilometers\n')
#   pass = sample(type_pass,1,replace=TRUE)
#   cat(pass,'ride the jeepney\n')
# }


