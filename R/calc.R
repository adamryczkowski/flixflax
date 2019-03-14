
calc_models<-function(model_names, ads, dv_name, iv_names, path_prefix='models/',  adaptive=NA, assume_calculated=FALSE) {
  library(caret)

  do_model_inner<-function(dv_name, model_name, ads, tc) {
    plik_name<-paste0(path_prefix, 'model_', dv_name, '_', model_name, '.rds')
    if(file.exists(plik_name)) {
      if (assume_calculated) {
        if(file.size(plik_name)>0) {
          cat(paste0("Reading in already calculated model ", model_name, "...\n"))
          return(readRDS(plik_name))
        } else {
          msg<-paste0("File ", plik_name, " is still being calculated")
          cat(paste0(msg, "\n"))
          return(msg)
        }
      } else {
        msg<-paste0("Skipping already calculated model ", model_name)
        cat(paste0(msg, "\n"))
        return(msg)
      }
    } else {
      if(assume_calculated) {
        msg<-paste0("Model ", model_name, " is not computed, skipping it.")
        cat(paste0(msg,'\n'))
        return(msg)
      } else {
        cat(paste0("Opening ", plik_name, "...\n"))
        write.table(data.frame(), file=plik_name, col.names=FALSE)
        return(tryCatch(
          {
            if(is.na(adaptive)) {
              msg<-paste0("Trying adaptive train of model ", model_name, "...")
              cat(paste0(msg, "\n"))
              model<-caret::train(dv ~ ., data = ads, method = model_name,
                                  trControl = tc_adaptive, tuneLength=15)
            } else if(adaptive) {
              msg<-paste0("Calculating adaptive train of model ", model_name, "..")
              cat(paste0(msg, "\n"))
              model<-caret::train(dv ~ ., data = ads, method = model_name,
                                  trControl = tc_adaptive, tuneLength=15)
            } else {
              msg<-paste0("Calculating non-adaptive train of model ", model_name, "...")
              cat(paste0(msg, "\n"))
              model<-caret::train(dv ~ ., data = ads, method = model_name, trControl = tc)
            }
            saveRDS(model, plik_name)
            return(msg)
          },
          error=function(e) {
            if(stringr::str_detect(e$message, stringr::fixed('For adaptive resampling, there needs to be more than one tuning parameter'))) {
              msg<-paste0("Adaptive train failed. Calculating non-adaptive train of model ", model_name, "...")
              cat(paste0(msg, "\n"))
              return(tryCatch(
                {
                  model<-caret::train(dv ~ ., data = ads, method = model_name, trControl = tc, tuneLength=15)
                  saveRDS(model, plik_name)
                  return(msg)
                },
                error=function(e) {
                  msg=paste0("Non-adaptive run of model ", model_name, " returned error: ", e$message)
                  cat(paste0(msg, '\n'))
                  saveRDS(msg, plik_name)
                  return(msg)
                }
              ))
            }
            msg=paste0("Adaptive run of model ", model_name, " returned error: ", e$message)
            cat(paste0(msg, '\n'))
            saveRDS(msg, plik_name)
            return(msg)
          }
        ))
      }
    }
  }

  # if(length(keep_nominal)>0) {
  #   groupvar<-ads[[keep_nominal]]
  #   cvIndex<-caret::createMultiFolds(groupvar, k = 10, times=10)
  # } else {
    cvIndex<-caret::createMultiFolds(ads$dv, times=10,  k = 10)
#  }
  selFun<-function(x, metric,  maximize) caret::oneSE(x=x, metric = metric, num=10, maximize = maximize)
  tc_adaptive <- caret::trainControl(index = cvIndex,
                                     method = 'adaptive_cv',
                                     adaptive = list(min = 5, alpha = 0.05,
                                                     method = "gls", complete = TRUE),
                                     search = "random",
                                     selectionFunction = selFun
  )
  if(adaptive) {
    tc <- caret::trainControl(index = cvIndex,
                              method = 'cv',
                              number = 10, repeats = 10)
  } else {
    tc <- caret::trainControl(index = cvIndex,
                              method = 'cv',
                              number = 10)
  }
  model_names<-model_names[sample(seq_along(model_names))]
  models=setNames(purrr::map(model_names, do_model_inner, ads=ads, tc=tc, dv_name=dv_name), model_names)

  list(ads=ads, models=models)
}
