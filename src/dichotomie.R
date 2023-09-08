dichotomie <- function(func, a, b, n){
  # func: function associated with the equation to solve
  # a, b: lower and upper bounds of an interval which contains a solution
  # n: number of iterations to perform
  
  A <- a
  B <- b
  if (func(A)*func(B) > 0){
    print(paste("There is not root between a =", A, "and b =", B))
  }
  else{
    for (i in 1:(n+1)){
      if (func(A)*func((A+B)/2) <= 0){
        B <- (A+B)/2
      }
      else{
        A <- (A+B)/2
      }
    }
  }
  
  (A+B)/2
}


# # example 1
# 
# f <- function(x){
#   x^2 - 1
# }
# 
# result <- dichotomie(func = f, a = -2, b = 0, n = 50)
# 
# result
# 
# result <- dichotomie(func = f, a = 0, b = +2, n = 50)
# 
# result
