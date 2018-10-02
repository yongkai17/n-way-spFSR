# Semi-auto feature selection using SP-FSR
rm(list = ls())
library(mlr)
library(spFSR)
library(mlbench)
library(class)

source('modifiedMakeClassifTask.R')

# make a logistic regression learner
wrapper     <- makeLearner('classif.logreg', predict.type = 'prob')
measure     <- mlr::auc

# create a task
data('Sonar')
task     <- makeClassifTask(data = Sonar, target = 'Class')
  

# Auto feature selection of main effects ----
spsaMod <- spFeatureSelection(task = task,
                              wrapper = wrapper,
                              measure = measure,
                              num.features.selected = 0,
                              norm.method = NULL)
# results for main effecys
spsaMod$features
spsaMod$best.value
spsaMod$best.std
spsaMod$run.time
    
getImportance(spsaMod)
features.to.keep <- as.character(getImportance(spsaMod)$features)
target           <- task$task.desc$target
    
fittedTask     <- makeClassifTask(Sonar[, c(features.to.keep, target)], 
                                target = target, id = 'subset')
    
fittedMod      <- train(wrapper, fittedTask)
pred           <- predict(fittedMod, fittedTask)
fittedMod      <- fittedMod$learner.model

fittedMod$coefficients
    

# Select interactions ----
sub_task  <- modifiedMakeClassifTask(data = Sonar[, c(features.to.keep, target)], 
                                     target = target, order = 2L)
    
sub_spsaMod <- spFeatureSelection(  task = sub_task,
                                    wrapper = wrapper,
                                    measure = measure,
                                    num.features.selected = 10,
                                    norm.method = NULL,
                                    features.to.keep = features.to.keep)
    
sub_spsaMod$features
sub_spsaMod$best.value
sub_spsaMod$best.std
sub_spsaMod$run.time
getImportance(sub_spsaMod)
    
new_features   <- as.character(getImportance(sub_spsaMod)$features)
sub_fittedtask <- makeClassifTask(sub_task$env$data[, c(new_features, target)], target = target, id = 'subset')
sub_fittedMod  <- train(wrapper, sub_fittedtask)
sub_pred       <- predict(sub_fittedMod, sub_fittedtask)

sub_fittedMod$learner.model$coefficients

# Compare with main effect models 
    
AIC( sub_fittedMod$learner.model)
BIC( sub_fittedMod$learner.model )

AIC( fittedMod)
BIC( fittedMod )

mlr::performance(sub_pred, mlr::auc)
mlr::performance(pred, mlr::auc)

    
