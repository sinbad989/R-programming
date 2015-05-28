# Committee.r, combinatorial computation example: 
# 3 committees, of sizes 3,4, and 5, are chosen from 
# 20 people; what is the probability that persons A 
# and B are chosen for the same committee?

# number the committee members from 1 to 20, with A and B being 1 and 2

sim <-function(nreps){
  commdata <- list()   #will store all our info about the 3 committes
  commdata$countabsamecomm <- 0
  for (rep in 1:nreps){
    commdata$whosleft <-1:20   #who's left to choose from
    commdata$numabchosen <- 0 #number among A, B chosen so far
    
    #Choose Committee 1
    commdata <- choosecomm(commdata,5)
    #If A or B already chose, no need to look at the other comms
    if (commdata$numabchosen > 0) next
    
    #Choose Committee 2
    commdata<-choosecomm(commdata,4)
    if (commdata$numabchosen > 0) next
    # choose committee 3
    commdata <- choosecomm(commdata,3)
  }
  print(commdata$countabsamecomm/nreps)
}

choosecomm <- function(comdat,comsize){
  #choose committee
  committee <- sample(comdat$whosleft,comsize)
  # count how many of A and B were chosen
  comdat$numabchosen <- length(intersect(1:2,committee))
  if (comdat$numabchosen == 2)
    comdat$countabsamecomm<-comdat$countabsamecomm + 1
  
  #delete chosen committee from the set of people we not have to choose from
  comdat$whosleft<-setdiff(comdat$whosleft,committee)
  return(comdat)  #R (usually) has no side effects
  

}