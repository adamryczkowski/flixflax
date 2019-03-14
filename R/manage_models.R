#Functions that manage caret models

get_all_caret_models<-function(type=NA) {
  if(!(is.na(type))){
    source<-caret::getModelInfo()
    types<-lapply(source, function(x) x$type)
    ktore<-which(unlist(lapply(types, function(x) type %in% x)))
    return(names(ktore))
  } else {
    return(names(caret::getModelInfo()))
  }
}

#Returns a dataset of all models with tags
get_models_with_tags<-function(type=NA) {
  #0. Cache all models
  source<-caret::getModelInfo()
  #1. Get all models
  models<-get_all_caret_models(type)
  #2. Extract tags
  tags<-lapply(source[models], function(x) x$tags)
  #3. Get sorted list of unique tags
  unique_tags<-setdiff(sort(unique(unlist(tags))),'')
  #4. Prepare a dataset
  ans<-data.table(models=models)
  for(tag in unique_tags) {
    ans[,(tag):=NA]
  }
  for(i in seq_along(models)) {
    model<-models[[i]]
    ans[i, (unique_tags):=as.list(unique_tags %in% source[[model]]$tags)]
  }
  function(model) {
    unique_tags %in% source[[model]]$tags
  }
  return(ans)
}

get_R_packages<-function(model_names) {
  source<-caret::getModelInfo()[model_names]
  libs<-unique(unlist(lapply(source, function(x) x$library)))
  return(libs)
}

which_packages_are_missing<-function(models_to_test) {
  libs<-get_R_packages(models_to_test)
  db<-setNames(rep(FALSE, length(libs)), libs)
  for(lib in libs) {
    canbe<-if(require(lib, character.only = TRUE)) {
      TRUE
    } else {
      FALSE
    }
    db[[lib]]<-canbe
  }
  return(unique(names(which(!db))))
}

which_models_are_ready_to_be_used<-function(models_to_test) {
  libs<-get_R_packages(models_to_test)
  db<-setNames(rep(FALSE, length(libs)), libs)
  for(lib in libs) {
    canbe<-if(require(lib, character.only = TRUE)) {
      TRUE
    } else {
      FALSE
    }
    db[[lib]]<-canbe
  }
  oklibs<-names(which(db))
  source<-caret::getModelInfo()[models_to_test]

  db<-setNames(rep(FALSE, length(models_to_test)), models_to_test)
  for(i in seq_along(models_to_test)) {
    model<-models_to_test[[i]]
    db[[model]]<-all(source[[model]]$library %in% oklibs)
  }
  return(names(which(db)))
}

what_packages_are_missing<-function(models_to_test) {
  libs<-get_R_packages(models_to_test)
  db<-setNames(rep(FALSE, length(libs)), libs)
  for(lib in libs) {
    canbe<-if(require(lib, character.only = TRUE)) {
      TRUE
    } else {
      FALSE
    }
    db[[lib]]<-canbe
  }
  oklibs<-names(which(db))
  source<-caret::getModelInfo()[models_to_test]

  db<-setNames(rep(FALSE, length(models_to_test)), models_to_test)
  for(i in seq_along(models_to_test)) {
    model<-models_to_test[[i]]
    db[[model]]<-all(source[[model]]$library %in% oklibs)
  }
  return(names(which(db)))
}

# models_to_test<-get_all_caret_models(type='Classification')
# models_to_try<-which_models_are_ready_to_be_used(models_to_test)
# libs<-which_packages_are_missing(models_to_test)
#
# install.packages(libs)
# summary(get_models_with_tags(type='Classification'))
