rm(list = ls())
data(iris)
irissubdf <- iris[1:100, c(1, 3, 5)]
names(irissubdf) <- c("sepal", "petal", "species")
irissubdf
g1_x = irissubdf[1:50,1]
g1_y = irissubdf[1:50,2]
g2_x = irissubdf[51:100,1]
g2_y = irissubdf[51:100,2]
g_x = c(g1_x, g2_x)
g_y = c(g1_y, g2_y)
N = 50
group = c(rep(-1,N), rep(1,N))

print(g_x)
print(g_y)
print(group)
plot(g_x, g_y, type='n', xlab='X', ylab='Y')
points(g1_x, g1_y, col='red')
points(g2_x, g2_y, col='blue')

θ0 = 0.1 # initial weitht
θ1 = 0.2 # initial weight
θ2 = 0.3 # initial weitht

M = 15            # number of epochs to run
eta = 0.005       # learning rate
th = 0.9          # threshold to stop
verbose = F   # whether detailed weight update info is printed
for (i in 1:M){
  print(paste('Epoch starts: ', i))
  
  ## We reshuffle the order of the datapoint for each epoch.
  index = 1:(2*N)
  index = sample(index)
  
  for (j in index){
    y_j = θ0 + θ1*g_x[j] + θ2*g_y[j]
    if (y_j >= 0){
      pred_j = 1
    }else{
      pred_j = -1}
    
    θ0 = θ0 + eta*(group[j] - pred_j)*1.0
    θ1 = θ1 + eta*(group[j] - pred_j)*g_x[j]
    θ2 = θ2 + eta*(group[j] - pred_j)*g_y[j]
    if (verbose == T){
      print(paste('  -> updating data point ', j, ' : '))
      print(paste('     -> θ0: ' ,θ0))
      print(paste('     -> θ0: ' ,θ1))
      print(paste('     -> θ0: ' ,θ2))
    }
  }  
  y_all = θ0 + θ1*g_x + θ2*g_y
  y_pred = y_all
  y_pred[y_all >= 0] = 1
  y_pred[y_all< 0] = -1
  
  acc = sum(y_pred == group)/length(group)
  print(paste('Epoch ends: ', i, ' WITH accuracy: ', acc))
  if (acc >= th){
    break
  }
}        
y_all = θ0 + θ1*g_x + θ2*g_y
print(y_all)

y_pred = y_all
y_pred[y_all >= 0] = 1
y_pred[y_all< 0] = -1

print(y_pred)

acc = sum(y_pred == group)/length(group)
print(acc)
plot(g_x, g_y, type='n', xlab='X', ylab='Y')
points(g1_x, g1_y, col='red')
points(g2_x, g2_y, col='blue')
abline(a = -1.0*θ0/θ2, b = -1.0*θ1/θ2, col='dark green', lwd=3, lty=2)

