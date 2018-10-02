source('modifiedMakeClassifTask.R')
source('modifiedMakeRegrTask.R')
spGenerateInteraction <- function(task, wrapper, 
                                  data, seed.number = NULL,
                                  total_features, 
                                  max_interaction_threshold_percent,
                                  measure,
                                  ...){
  
  
  set.seed(seed.number)
  
  # Initialise the empty lists and data frame to store
  results_importance  <- list()
  results_summary     <- data.frame()
  all_edges           <- list()
  covariance          <- list()
  est_coeff           <- list()
  
  # Initialise index for summary and result
  k <- 1
  cat('\nGetting main effect features....\n')
  # Run SP-FSR to select specificied number of features
  spsaMod <- spFeatureSelection(  task = task,
                                  wrapper = wrapper,
                                  measure = measure,
                                  num.features.selected = total_features,
                                  ...)
  
  # Store the summary in the data frame
  results_summary[k, 'p']       <- total_features
  results_summary[k, 'mean']    <- spsaMod$best.value
  results_summary[k, 'std']     <- spsaMod$best.std
  results_summary[k, 'runtime'] <- spsaMod$run.time
  
  # Store the importance result in the list
  results_importance[[k]]   <- getImportance(spsaMod)
  all_edges[[k]]            <- NULL
  features.to.keep <- as.character(results_importance[[k]]$features)
  target           <- task$task.desc$target
  
  # Refit with the full dataset and extract IC values
  fittedTask     <- makeClassifTask(data[, c(features.to.keep, target)], 
                                   target = target, id = 'subset')
  fittedMod      <- train(wrapper, fittedTask)
  pred           <- predict(fittedMod, fittedTask)
  fittedMod      <- fittedMod$learner.model
  est_coeff[[k]] <- data.frame(coefficient = fittedMod$coefficients)
  
  results_summary[k, 'AIC']     <- AIC(fittedMod)
  results_summary[k, 'BIC']     <- BIC(fittedMod)
  results_summary[k, 'measure'] <- performance(pred, measure)
  
  # Create a task with pairwise interactions
  cat('\nGetting interaction features....\n')
  sub_task  <- modifiedMakeClassifTask(data = data[, c(features.to.keep, target)], 
                                       target = target, order = 2L)
  
  # Specify the max interaction number allowed
  max_interactions_number  <- total_features*(total_features-1)/2
  max_interactions_number  <- floor(max_interactions_number*max_interaction_threshold_percent)
  max_interactions_number  <- max(max_interactions_number, 1)
  
  # Run SP-FSR for each interaction number
  for(j in 1:max_interactions_number){
    k <- k + 1
    sub_spsaMod <- spFeatureSelection(  task = sub_task,
                                        wrapper = wrapper,
                                        measure = measure,
                                        num.features.selected = j,
                                        features.to.keep = features.to.keep,
                                        ...)
    
    results_summary[k, 'p']       <- results_summary[k-1, 'p'] + 1
    results_summary[k, 'mean']    <- sub_spsaMod$best.value
    results_summary[k, 'std']     <- sub_spsaMod$best.std
    results_summary[k, 'runtime'] <- sub_spsaMod$run.time
    results_importance[[k]]       <- getImportance(sub_spsaMod)
    
    new_features   <- as.character(results_importance[[k]]$features)
    sub_fittedtask <- makeClassifTask(sub_task$env$data[, c(new_features, target)], target = target, id = 'subset')
    sub_fittedMod  <- train(wrapper, sub_fittedtask)
    sub_pred       <- predict(sub_fittedMod, sub_fittedtask)
    
    sub_fittedMod   <- sub_fittedMod$learner.model
    covariance[[k]] <- sub_fittedMod$R
    est_coeff[[k]]  <- data.frame(coefficient = sub_fittedMod$coefficients)
    
    results_summary[k, 'AIC']     <- AIC( sub_fittedMod )
    results_summary[k, 'BIC']     <- BIC( sub_fittedMod )
    results_summary[k, 'measure'] <- performance(sub_pred, measure)

    # Extract the interaction terms
    y <- sapply(new_features, FUN =function(x){strsplit(x, "\\.")})
    
    edges <- data.frame()
    m <- 0
    for(z in 1:length(y)){
      x <- y[[z]]
      if( length(x) == 2){
        m <- m + 1
        edges[m, 'from']   = x[1]
        edges[m, 'to']     = x[2]
        edges[m, 'weight'] = 1
      } 
    }
    all_edges[[k]] <- edges
  }
  
  # Create a list to store all results
  results <- list(summary      = results_summary, 
                  importance   = results_importance,
                  nodes        = data.frame(variable = features.to.keep),
                  edges        = all_edges,
                  covariance   = covariance,
                  est_coeff    = est_coeff)
  
  return(results)
  
}