library(vars)
library(astsa)
x = cbind(cmort, tempr, part)



cmort
plot(cmort)
class(cmort)
plot(tempr)
plot(part)

x <- cbind(cmort, tempr, part)
plot(x)


summary(VAR(x, p=1, type='both'))


VARselect(x, lag.max=10, type="both")

