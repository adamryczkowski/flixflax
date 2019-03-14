make_ads<-function(dt, iv_names, dv_name, keep_nominal=character(0)) {
  rmvar<-caret::nearZeroVar(dt)
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

prepare_variables_hybrid<-function(dt, iv_names, dv_name, keep_nominal=character(0)) {
  dv<-dt[[dv_name]]
  nas<-is.na(dv)
  mydt<-as.data.table(dt[!nas,iv_names])
  mydt<-danesurowe::copy_dt_attributes(dt, mydt)
  dv<-dv[!nas]
  data.table::setattr(dv, 'label', Hmisc::label(dt[[dv_name]]))

  fobs<-purrr::map_dbl(iv_names, ~danesurowe::GetFOB(mydt[[.]], flag_recalculate_uniques = TRUE))

  ordinals<-iv_names[fobs==2]
  ans<-list()
  if(length(ordinals)>0) {
    browser()
    bins<-regression_recode_ordinal(ordinals, depvar, mydt)
    ans<-c(ans, bins$df)
  }
  nominals<-iv_names[fobs==1]
  if(length(nominals)>0) {
    a<-purrr::flatten(purrr::map(nominals, regression_recode_factor_1, dt=mydt, keep_nominal=keep_nominal))
    if ('V1' %in% names(a)) {
      browser()
    }
    ans<-c(ans,a)
  }
  binaries<-iv_names[fobs==3]
  if(length(binaries)>0) {
    a<-purrr::flatten(purrr::map(binaries, regression_recode_factor_1, dt=mydt, keep_nominal=keep_nominal))
    #    a<-flatten(purrr::map(binaries, regression_recode_binary_1, dt=mydt))
    ans<-c(ans,a)
  }
  numerics<-iv_names[fobs==0]
  if(length(numerics)>0) {
    a<-mydt %>% select_(.dots=numerics) %>% as_tibble() %>% mutate_all(funs(as.numeric))
    #    a<-flatten(purrr::map(numerics, ~scale(mydt[[.]])))
    #    browser()
    tmp_df<-c(ans,a)
    missing_pattern<-mice::md.pattern(a)
    ans2<-as.data.table(tmp_df)

    imputes<-NULL
    if(nrow(missing_pattern)>2) {
      if(nrow(mydt)>40000) {
        m<-missing_pattern[nrow(missing_pattern),1:ncol(missing_pattern)-1]
        vars_miss<-names(m[m>0])
        ms<-list()
        for(colname in vars_miss) {
          var<-ans2[[colname]]
          m<-mean(var, na.rm = TRUE)
          imputes<-c(imputes, list(sumNA=sum(is.na(var))))
          var[is.na(var)]<-m
          ms<-c(ms, list(m))
          ans2[,(colname):=as.numeric(var)]
        }
        imputes<-data.frame(varname=vars_miss, missing_count=unlist(imputes), mean=unlist(ms))
        ans2<-cbind(ans, imputes)
      } else {
        library(mice)
        imp<-mice::mice(ans2)
        ans2<-complete(imp)
        ans2<-danesurowe::copy_dt_attributes(data.frame(ans), data.frame(ans2))
      }
    }
    for ( mycolname in numerics) {
      mylabel<-attr(mydt[[mycolname]], 'label')
      data.table::setattr(ans2[[mycolname]], 'label', mylabel)

    }
  } else{
    ans2<-mydt
  }

  mydt<-as.data.table(ans2)
  return(list(db=mydt, dv=dv))
}

regression_recode_factor_1<-function(varname, dt, keep_nominal=character(0)) {

  # tmp_df<-data.frame(x=factor(c(1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,3,NA,NA,NA,NA),
  #                             levels=c(1,2,3), labels=c('A', 'B', 'C')))
  #as.integer(dt[[varname]])
  tmp_fac<-addNA(as.integer(dt[[varname]]), ifany=TRUE)
  mylabel<-attr(dt[[varname]], 'label')
  if(is.null(mylabel)) {
    mylabel<-varname
#    browser()
  }
  if(length(unique(tmp_fac))>1) {
    tmp_df<-setNames(tibble::tibble(tmp_fac), varname)
    tmp_df<-predict(caret::dummyVars(~ ., tmp_df, fullRank=FALSE, levelsOnly = FALSE, drop2nd=TRUE), tmp_df)
    to_remove<-caret::nearZeroVar(as.data.table(tmp_df), saveMetrics = FALSE)
    if(varname %in% keep_nominal) {
      #    browser()
      for(to_rm in to_remove) {
        tmp_fac[as.integer(tmp_fac)==to_rm]<-NA
      }
      class(tmp_fac)<-class(dt[[varname]])
      tmp_fac<-addNA(tmp_fac, ifany = TRUE)
      if(any(is.na(levels(tmp_fac)))) {
        setattr(tmp_fac, 'levels', c(attr(dt[[varname]], 'levels'), as.character(NA)))
      } else {
        setattr(tmp_fac, 'levels', attr(dt[[varname]], 'levels'))
      }
      data.table::setattr(tmp_fac, 'label', mylabel)
      setNames(data.table(x=tmp_fac), varname)

      #We still need to replace small groups with NA.
    } else {
      to_keep<-setdiff(seq(ncol(tmp_df)), to_remove)
      tmp_df<-data.table::as.data.table(tmp_df[,to_keep, drop=FALSE])
      mylabels<-levels(dt[[varname]])
      for(i in seq_along(colnames(tmp_df))) {
        mycolname<-colnames(tmp_df)[[i]]
        if(stringr::str_detect(mycolname, '\\.NA$')) {
          data.table::setattr(tmp_df[[i]], 'label', paste0(mylabel, " - NA"))
        } else {
          a<-as.integer(stringr::str_match(mycolname, '\\.([0-9]+)$')[,2])
          data.table::setattr(tmp_df[[i]], 'label', paste0(mylabel, " - ", mylabels[a]))
        }
      }
      tmp_df
    }
  } else {
    data.table()
  }
}
