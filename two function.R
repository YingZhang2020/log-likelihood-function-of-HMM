rm(list=ls())

#1.function of generating bernoulli-HMM sample with length of T
#tfm=gamma
#probability of state is delta

bern.HMM.generate_sample <- function(T,m,delta,gamma,q)
{ 
mvect <- 1:m 
state <- numeric(T) 
state[1] <- sample(mvect, 1, prob=delta) # when mvect=i, delta=Pr(Ct=i), 
for (i in 2:T) state[i] <- sample(mvect ,1,
                                  prob=gamma[state[i-1],]) 
#gamma is the t??p??m of the Bernoulli-HMM                                
x <-  rbinom(T,1,prob=q) #qi=Pr??Xt=1|Ct=i??
return(x) 
}
m <- 3
delta <- c(0.2,0.3,0.5)#sum equal to 1.
gamma <- matrix(c(0.3,0.1,0.6,0.1,0.6,0.3,0.6,0.3,0.1),3,3)
###gamma need to be symmetric or not????
q <- c(0.2,0.4,0.5)
X <- bern.HMM.generate_sample(100,m,delta,gamma,q)


#2.function of caculating log-liklihood of the sample above
bern.HMM.log.likelihood <- function(T,X,delta,gamma,q)
{
alpha <- delta*dbinom(X[1],T,q)
lscale <- log(sum(alpha))
alpha <- alpha/sum(alpha)
for (i in 2:T) alpha <- alpha %*% gamma*dbinom(X[i],T,q)
lscale <- lscale+log(sum(alpha))
alpha <- alpha/sum(alpha)
lscale
}
Y <- bern.HMM.log.likelihood(30,X,delta,gamma,q)
print(Y)
