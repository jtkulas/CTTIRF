
p <-  50     ## p-value (need to figure out scale)
a <-  .9      ## corrected item-total correlation

total <- seq(1:100)

x <- 1/(1+(exp(-a*(total-p))))
plot(total,x, type="l")


p <-  20     ## p-value (need to figure out scale)
a <-  .1      ## corrected item-total correlation

x2 <- 1/(1+(exp(-a*(total-p))))
lines(total,x2, col="red")


p <-  70     ## p-value (need to figure out scale)
a <-  .5      ## corrected item-total correlation


x3 <- 1/(1+(exp(-a*(total-p))))
lines(total,x3, col="green")
