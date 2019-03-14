make_ads<-function(dt, iv_names, dv_name, keep_nominal=character(0)) {
  rmvar<-caret::nearZeroVar(dt)
  groupvar<-dt$iv56
  nearZeroVars<-colnames(dt)[ seq_along(colnames(dt)) %in% rmvar]
  vars_to_keep<-colnames(dt)[ ! seq_along(colnames(dt)) %in% rmvar]
  ads<-dt[c(vars_to_keep, dv_name)]
  iv_names=intersect(vars_to_keep, iv_names)

  ans<-prepare_variables_hybrid(ads, iv_names, dv_name, keep_nominal=keep_nominal)
  depvar<-ans$dv
  nas<-is.na(depvar)
  # if(length(keep)>0) {
  #   browser()
  #   to_keep<-setdiff(colnames(ans$db), keep)
  #   ans$db<-cbind(ans$db[to_keep], setNames(data.frame(x=dt[keep]), keep))
  # }
  dvlab<-Hmisc::label(depvar)
  depvar<-depvar[!nas]
  data.table::setattr(depvar, 'label', dvlab)
  cbind(data.frame(dv=depvar), ans$db)
}
