# Generates data for a marriage market
# We generate N players of each sex and assign them utilities over the opposing 
# side's players. 
# Man i's utility of marrying j is u[ij]= mu[j]+sigma*g[ij], where mu[j] is  
# j's systematic attractiveness and g is i's idiosyncratic preference over j
# Man i's utility of staying single is u[i0] = sigma*(max_{k=1,...,J}g[ik])
# J depends on N because as the market (N) grows we need the share of bachelors 
# to be non-zero (see Menzel's 2015 Econometrica paper)
# Woman j's preference is modeled the same way, but we use delta instead of mu, 
# e instead of g, omega instead of sigma and v instead of u

library(matrixStats)

#N # number of players on each side of the market is assumed predefined
#sigma # assumed predefined
#omega # assumed predefined

J <- round(sqrt(N))

mu <- rnorm(N, mean = 0, sd = 1)
g <- matrix(rnorm(N^2, mean = 0, sd = 1), nrow=N,ncol=N)
u <- t(matrix(replicate(N,mu),nrow=N))+sigma*g # men's utility over women
g_bach <- rowMaxs(matrix(rnorm(N*J, mean = 0, sd = 1), nrow=N,ncol=J), na.rm = TRUE)
u_bach <- matrix(sigma*g_bach, N, 1) # men's utility of staying single
rownames(u) <- paste("Man",1:N)
colnames(u) <- paste("Woman",1:N)
rownames(u_bach) <- paste("Man",1:N)
colnames(u_bach) <- "Single"

delta <- rnorm(N, mean = 0, sd = 1)
e <- matrix(rnorm(N^2, mean = 0, sd = 1), nrow=N,ncol=N)
v <- t(matrix(replicate(N,mu),nrow=N))+omega*e # women's utility over men
e_bach <- rowMaxs(matrix(rnorm(N*J, mean = 0, sd = 1), nrow=N,ncol=J), na.rm = TRUE)
v_bach <- matrix(omega*e_bach, N, 1) # women's utility of staying single
rownames(v) <- paste("Woman",1:N)
colnames(v) <- paste("Man",1:N)
rownames(v_bach) <- paste("Woman",1:N)
colnames(v_bach) <- "Single"