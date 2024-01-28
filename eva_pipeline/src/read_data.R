# load functions
make_models <- function(variable = ~1){
  # variable:
  
  if (variable == ~1){
    models_object <- c(list("model_00" = list("location.fun" = variable, "scale.fun" = variable, "shape.fun" = variable)))
  }
  else{
    models_object <- c(list("model_01" = list("location.fun" = variable, "scale.fun" = variable, "shape.fun" = variable),
                            "model_02" = list("location.fun" = variable, "scale.fun" = ~1, "shape.fun" = ~1),
                            "model_03" = list("location.fun" = ~1, "scale.fun" = variable, "shape.fun" = ~1),
                            "model_04" = list("location.fun" = ~1, "scale.fun" = ~1, "shape.fun" = variable),
                            "model_05" = list("location.fun" = variable, "scale.fun" = variable, "shape.fun" = ~1),
                            "model_06" = list("location.fun" = variable, "scale.fun" = ~1, "shape.fun" = variable),
                            "model_07" = list("location.fun" = ~1, "scale.fun" = variable, "shape.fun" = variable)))
  }
  
  models_object
  
}


