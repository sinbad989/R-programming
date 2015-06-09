n = 150 
p = 2
sigma = 1

# Positive example: variance, centre, number of positive example
meanpos = 3
npos = round(n/2)

# Negative example: variance, centre, number of positive example
meanneg = 0
nneg = n - npos

# Generate the positive and negative examples
xpos = matrix(rnorm(npos*p,mean=meanpos,sd=sigma),npos,p)
xneg = matrix(rnorm(nneg*p,mean=meanneg,sd=sigma),nneg,p)

x = rbind(xpos,xneg)

# Generate the labels
y = matrix(c(rep(1,npos),rep(-1,nneg)))


# Visualize the data
plot(x,col=ifelse(y>0,1,2))
legend("topleft",c('Positive','Negative'),col=seq(2),pch=1,text.col=seq(2))

#Splitting the data into a training(80%) and testing set(20%)
ntrain = round(n*0.8)
tindex = sample(n,ntrain)
xtrain = x[tindex,]
xtest = x[-tindex,]

ytrain = y[tindex]
ytest = y[-tindex]
istrain = rep(0,n)
istrain[tindex] = 1

#Visualize 
plot(x,col=ifelse(y>0,1,2),pch = ifelse(istrain==1,1,2))
legend("topleft",c('Postive Train','Positive Test','Negative Train','Negative Test'),col=c(1,1,2,2),pch=c(1,2,1,2),text.col=c(1,1,2,2))


# Train a SVM

# load the kernlab package
library(kernlab)

#train the SVM
svp <- ksvm(xtrain,ytrain,type="C-svc",kernel='vanilladot',C=100,scaled=c())

# General Summary
svp
# Attributes that you can access
attributes(svp)

# For example, the support vectors
alpha(svp)
alphaindex(svp)
b(svp)

# Use the built-in function to pretty-plot the classifier
plot(svp,data=xtrain)

plotlinearsvm = function(svp,xtrain){
  
}



