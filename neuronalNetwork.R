library(caret)
library(fma)

data(package="fma")
plot(BoxCox(dowjones,1))
fit  <- avNNet(score ~ log.savings + log.income + log.address +
                 log.employed, data=dowjones, repeats=25, size=3, decay=0.1,
               linout=TRUE)
