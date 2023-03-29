# Numerical accuracy
x = c()
x[1] = 0
for (i in 2 : 101)
{
  x[i] = x[i-1] + 0.1
  #x[i] = round(x[i],1)
}

options(digits = 22)
x[101]
x[101] == 10.1

options(digits = 18)
options(scipen = 0)

y = seq(0, 10, by = 0.1)
y[101] == 10

M = matrix(c(x,y), 101, 2)

# Taylor series approx of log f-ction
xx = seq(0.1, 4, by = 0.1)
yy = log(xx)

x = 1
hh = seq(-0.9, 3, by = 0.1)
x+hh
TS1 = log(x) + x^-1 * hh
TS2 = log(x) + x^-1 * hh + .5 * -x^-2 * hh^2
TS3 = log(x) + x^-1 * hh + .5 * -x^-2 * hh^2 + 
  1 / factorial(3) * 2 * x^-3 * hh^3
TS4 = TS3 + 1/factorial(4) * -6 * x^-4 * hh^4

y.min = min(c(yy, TS1, TS2, TS3, TS4))
y.max = max(c(yy, TS1, TS2, TS3, TS4))

plot(xx, yy, type = "l",
     xlab = "Grid for x",
     ylab = "Value of the function",
     main = "The logarithmin function",
     ylim = c(y.min, y.max))
grid()

lines(xx, TS1, col = "blue")
lines(xx, TS2, col = "red")
lines(xx, TS3, col = "green")
lines(xx, TS4, col = "pink")

plot(xx, yy, type = "l",
     xlab = "Grid for x",
     ylab = "Value of the function",
     main = "The logarithmin function",
     #ylim = c(y.min, y.max),
     xlim = c(0.1, 2))
grid()

lines(xx, TS1, col = "blue")
lines(xx, TS2, col = "red")
lines(xx, TS3, col = "green")
lines(xx, TS4, col = "pink")

