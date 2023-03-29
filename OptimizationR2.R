# Gradient descent
install.packages("numDeriv")
library(numDeriv)

f = function(x)
{
  x[1]^2+x[2]^2
}

x = c(5,2)
grad(f, x)
gradDesc = function(x, f, alp = 0.01, maxIter = 1000, eps = 10^-5)
{
  for (i in 1 : maxIter)
  {
    #State transition
    x = x - alp * grad(f,x)
    
    #Termination criterion
    if ( norm(grad(f,x), type = "2") < eps)
    {
      print(paste('Termination criterion satisfied at ', i, ' iterations.'))
      break
    }
  }
  
  #Output return
  return(x)
}

x = c(10, 10)
resu = gradDesc(x, f, maxIter = 1000)






# Gradient descent
install.packages("numDeriv")
library(numDeriv)

f = function(x)
{
  x[1]^2+x[2]^2
}

x = c(5,2)
grad(f, x)
gradDesc = function(x, f, alp = 0.01, maxIter = 1000, eps = 10^-5)
{
  #Output initialization
  f.hist = rep(NA, maxIter + 1)
  f.hist[1] = f(x)
  x.hist = matrix(NA, maxIter + 1, length(x))
  x.hist[1,] = x
  
  for (i in 1 : maxIter)
  {
    #State transition
    x = x - alp * grad(f,x)
    
    f.hist[i+1] = f(x)
    x.hist[i+1,] = x
    
    #Termination criterion
    if ( norm(grad(f,x), type = "2") < eps)
    {
      print(paste('Termination criterion satisfied at ', i, ' iterations.'))
      break
    }
  }
  
  #Output return
  out = list(x.opt = x, 
             f.opt = f(x), 
             x.hist = x.hist, 
             f.hist = f.hist)
  
  return(out)
}

x = c(10, 10)
resu = gradDesc(x, f, maxIter = 1000)
plot(resu$f.hist, type = "l")


v = c()
v = rep(NA, 10^7)
tic = Sys.time()
for (i in 1 : (10 * 10^6))
{
  v[i] = i
}
toc = Sys.time()
dt = toc - tic

f.plot = function(x1, x2)
{
  x1^2 + x2^2
}

xx1 = xx2 = seq(-10, 10, by = 0.1)
zz = outer(xx1, xx2, f.plot)
contour(xx1, xx2, zz, asp = 1)
lines(resu$x.hist, type = "p", col = "red")
abline(h = 0, v = 0, col = "blue")


