# library(caret)

data <- data.frame(cbind(1:10, 1:10))

data

query <- data[1, ]

preProcValues <- caret::preProcess(x = data, method = c("center", "scale"))
testTransformed <- predict(object = preProcValues, newdata = query)

testTransformed

apply(data, 2, mean)

apply(data, 2, sd)

(query - apply(data, 2, mean))/apply(data, 2, sd)


preProcValues$mean

preProcValues$std

