library(caret)

customers <- data.frame(
  id=c(10,20,30,40,50),
  gender=c('male','female','female','male','female'),
  mood=c('happy','sad','happy','sad','happy'),
  outcome=c(1,1,0,0,0))


customers

# dummify the data
dummy <- caret::dummyVars(formula = " ~ .", sep = NULL, data = customers)

dummy

transformed <- data.frame(predict(object = dummy, newdata = customers))

transformed






