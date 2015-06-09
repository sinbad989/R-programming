# Author     : Anthony Val Camposano 
# Book source: Understandable Statistics, Brase/Brase

#CHAPTER 3: Averages and Variation



#=====================================
# Mean can be affected by exceptinal values, trimming allows us to 
# ++++++++++++++++++++++++-
# Trimmed Mean

trim_mean = function(x,percent){
  sort(x,decreasing = FALSE)
  trimLen=round(length(x)*percent)
  x=x[-(trimLen)]
return (mean(sort(x,decreasing=TRUE)[-trimLen]))
}

