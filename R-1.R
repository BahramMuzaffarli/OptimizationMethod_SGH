
f = function(x1, x2)
{
  x1^2 * x2^2
}

xx = yy = seq(-2, 2, by = 0.2)
Z = outer(xx, yy, f)
persp(xx, yy, Z, phi = 30)
contour(xx, yy, Z, nlevels = 50)


xx = seq(0, 1, by = 0.001)
yy = sin(1/xx)
plot(xx, yy, type = "l")
abline(v = 0.4, col = "blue")



