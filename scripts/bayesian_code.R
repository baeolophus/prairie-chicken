library(rjags)
library(HDInterval)
###########
#piecewise code by Michael A. Patten
#mpatten@ou.edu, Oklahoma Biological Survey
#adapted by Claire M. Curry
#cmcurry@ou.edu

############
#create fake data
set.seed(1)
N <- 100
x <- seq(0,1,len=N)
m1 <- 1.0 # slope and intercept for each line segment
b1 <- 0.0
m2 <- 1.5
b2 <- -0.2
CP <- 30 # the designated change point [a.k.a. threshold]
y1 <- m1 * x[1:CP] + b1 # 1st segment
y2 <- m2 * x[(CP+1):N] + b2 # 2nd segment
y <- c(y1,y2) + rnorm(N,0,0.05) # add a bit of noise to the lines
plot(x,y) # double-check scatter

#Create dataset in list
d <- list(x = x, 
          y = y, 
          N = N)
############
#Create model from text file.
m <- jags.model("scripts/piecewise_model.txt",
                d, 
                n.chains=3,
                n.adapt=30000)
#update model call (I think required to make the coda.samples below work)
update(m)
#generate sampling
q <- coda.samples(m,      #model
                  c("T"), #the name of variable to be evaluated (T is the threshold/cut point in piecewise_model.txt)
                  500)#00)#number of iterations
summary(q)
hdi(q)
plot(q)
