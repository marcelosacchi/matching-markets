## Initialize variables for the women proposing algorithm

# Matrix to keep track of rejections
# Vector to keep track of current proposals in each round
# pseudo U to ignore rejected utilities
rejected <- matrix(as.logical(0*1:N^2), nrow=N,ncol=N) #row is man who rejects
# man(column)
rownames(rejected) <- paste("Man",1:N)
colnames(rejected) <- paste("Woman",1:N)
rejected_past <- rejected
rejected_by <- t(rejected)
rownames(rejected_by) <- paste("Woman",1:N)
colnames(rejected_by) <- paste("Man",1:N)
proposed_by <- rejected
rownames(proposed_by) <- paste("Man",1:N)
colnames(proposed_by) <- paste("Woman",1:N)
proposals <- max.col(cbind(v,v_bach)) # propose to best available option as long
# as better than being single
pseudo_v <- v
rounds <- 1

## First round
# Men take their proposals, if any, and tentatively accept the best one if it's 
# better than staying single
for (n in 1:N){
  proposed_by[n,] <- (proposals==n)
  if (sum(proposed_by[n,])>0) { #if man n has any proposal(s)
    if (max((proposed_by[n,]==TRUE)*u[paste("Man",n),]+(proposed_by[n,]!=TRUE)*(-100))>u_bach[n]) {
      #if best proposal better than staying single, don't reject it (but reject proposals
      # below the best one
      rejected[n,] <- rejected[n,] + ((proposed_by[n,]==TRUE)*u[paste("Man",n),]<max((proposed_by[n,]==TRUE)*u[paste("Man",n),]) & (proposed_by[n,]==TRUE)) # adds new rejections to previous ones
    }
    else rejected[n,] <- rejected[n,] + (n==proposals)}} # if no proposal tops being single, reject all
rejected_by <- t(rejected)
new_rejections = sum(rejected)-sum(rejected_past)
# Other rounds (keep doing it until no new rejections)
max_rounds = 10*N
while (new_rejections > 0 & rounds <= max_rounds) {
  rejected_past <- rejected
  pseudo_v <- v*(1-rejected_by)+(-100)*rejected_by #to force women never to 
  #propose to a man twice
  proposals <- max.col(cbind(pseudo_v,v_bach)) # propose to best available option as long
  # as better than being single
  for (n in 1:N){
    proposed_by[n,] <- (proposals==n)
    if (sum(proposed_by[n,])>0) { #if man n has any proposal(s)
      if (max((proposed_by[n,]==TRUE)*u[paste("Man",n),]+(proposed_by[n,]!=TRUE)*(-100))>u_bach[n]) {
        #if best proposal better than staying single, don't reject it (but reject proposals
        # below the best one
        rejected[n,] <- rejected[n,] + ((proposed_by[n,]==TRUE)*u[paste("Man",n),]<max((proposed_by[n,]==TRUE)*u[paste("Man",n),]) & (proposed_by[n,]==TRUE)) # adds new rejections to previous ones
      }
      else rejected[n,] <- rejected[n,] + (n==proposals)}} # if no proposal tops being single, reject all
  rejected_by <- t(rejected)
  new_rejections <- sum(rejected)-sum(rejected_past)
  rounds <- rounds + 1
}
match <- (proposals) # matching with N+1 is being single
frac_single <- sum(proposals == N+1)/N
v_star_W<-v_bach
for (n in 1:N) {v_star_W[n] <- cbind(v,v_bach)[n,match[n]]}
mean_v_star_W <- mean(v_star_W)
u_star_W<-u_bach
for (n in 1:N) { if (is.na(match(n,match))) {u_star_W[n] <- u_bach[n]} else {u_star_W[n] <- cbind(u,u_bach)[n,match(n,match)]}}
mean_u_star_W <- mean(u_star_W)