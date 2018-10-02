#' Create a classification task with interaction term order
#' @author Yong Kai Wong
#' @description Generating pairwise interaction terms of a data and pass in to a task 

# TODO: Optimise or limit m = number of all engineerd features; heureustic: sqrt(m), m^(1/3)
# TODO: Suppress message of remobing X.Intercept
#' @param data A data frame containing the features and target variable(s)
#' @param target Name of the target variable
#' @param order Interaction order. The allowed values are 1, 2, and 3. Default is 2L
#' @import mlr

modifiedMakeClassifTask <- function(data, target, order = 2, remove.constant = FALSE,...){
  
  # Argument check on "order"
  # It is not necessary to check on "data" or "target" as they will 
  # be handled by makeClassifTask function
  if( !order %in% c(1, 2, 3) | !inherits(order, 'integer')){
    stop('order must be 1, 2, or 3.')
  }
  
  # create the classification task
  
  task   <- mlr::makeClassifTask(data = data, target = target, ...)

  if( order %in% c(2, 3)){
    # extract target and descriptive features
    
    y   <- task$env$data[, target]
    
    if( order == 2){
      fml <- formula(paste0(target, "~.*."))
    }else{
      fml <- formula(paste0(target, "~.^3"))
    }
    
    X   <- data.frame(model.matrix(fml, task$env$data))
    
    # Remove constant terms
    if(remove.constant){
      X   <- mlr::removeConstantFeatures(X)
    }

    
    # bind the data
    newdata <- cbind(X,y)
    colnames(newdata)[ncol(newdata)] <- target
    
    # prompt warning message if p > n
    p <- ncol(X)
    if( p > nrow(X)){
      warning('More features than number of observations.')
    }
    
    # reconfigure the task
    task   <- mlr::makeClassifTask(data = newdata, target = target, ...)
  }

  return(task)
  
}