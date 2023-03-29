# Numerical derivative for f : R -> R
f = function(x)
{
  3*x^2
}

df = function(x, f, del = 10^-4)
{
  # Calculates a derivative of f in x.
  return( (f(x+del) - f(x-del)) / (2*del) )
}

df(-2, f)
df(-1, f)
df(0, f)
df(1, f)
df(2, f)

df(c(-2, -1, 0, 1, 2), f)


# Directional derivative
f = function(x1, x2)
{
  x1^2+x2^2
}

xx = yy = seq(-2, 2, by = 0.25)
zz = outer(xx, yy, f)
persp(xx, yy, zz, phi = 30)
contour(xx, yy, zz)

# Numerical gradient of f : R^n -> R
df = function(x, f, t = 10^-4)
{
  grad = c()
  n = length(x)
  for (i in 1 : n)
  {
    ei = rep(0, n)
    ei[i] = 1
    grad[i] = ((f(x+t*ei) - f(x-t*ei)) / (2*t))
  }
  return(grad)
}

f = function(x)
{
  x[1]^2*x[2]^2
}

x = c(2,2)
df(x, f)

install.packages("numDeriv")
library(numDeriv)
grad(f, x)

