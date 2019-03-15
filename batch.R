library(doMC)
registerDoMC(32)

models_to_try <-
  c("ada", "AdaBag", "AdaBoost.M1", "adaboost", "avNNet", "awnb",
    "awtan", "bag", "bagEarth", "bagEarthGCV", "bagFDA", "bagFDAGCV",
    "bam", "bartMachine", "bayesglm", "binda", "blackboost", "BstLm",
    "bstSm", "bstTree", "C5.0", "C5.0Cost", "C5.0Rules", "C5.0Tree",
    "cforest", "CSimca", "ctree", "ctree2", "deepboost", "dnn", "dwdLinear",
    "dwdPoly", "dwdRadial", "earth", "evtree", "extraTrees", "fda",
    "FH.GBML", "FRBCS.CHI", "FRBCS.W", "gam", "gamboost", "gamLoess",
    "gamSpline", "gaussprLinear", "gaussprPoly", "gaussprRadial",
    "gbm_h2o", "gbm", "gcvEarth", "glm", "glmboost", "glmnet_h2o",
    "glmnet", "glmStepAIC", "hda", "hdda", "J48", "JRip", "kernelpls",
    "kknn", "knn", "lda", "lda2", "Linda", "LMT", "loclda", "LogitBoost",
    "logreg", "lssvmLinear", "lssvmPoly", "lssvmRadial", "lvq", "manb",
    "mda", "Mlda", "mlp", "mlpKerasDecay", "mlpKerasDecayCost", "mlpKerasDropout",
    "mlpKerasDropoutCost", "mlpML", "mlpWeightDecay", "mlpWeightDecayML",
    "monmlp", "msaenet", "multinom", "naive_bayes", "nb", "nbDiscrete",
    "nbSearch", "nnet", "nodeHarvest", "null", "OneR", "ordinalNet",
    "ordinalRF", "ORFlog", "ORFpls", "ORFridge", "ORFsvm", "ownn",
    "pam", "parRF", "PART", "partDSA", "pcaNNet", "pda", "pda2",
    "PenalizedLDA", "plr", "pls", "plsRglm", "polr", "protoclass",
    "qda", "QdaCov", "randomGLM", "ranger", "rbf", "rbfDDA", "Rborist",
    "rda", "regLogistic", "rf", "rFerns", "RFlda", "rfRules", "rmda",
    "rocc", "rotationForest", "rotationForestCp", "rpart", "rpart1SE",
    "rpart2", "rpartCost", "rpartScore", "RRF", "RRFglobal", "rrlda",
    "RSimca", "sda", "sdwd", "simpls", "SLAVE", "slda", "smda", "snn",
    "sparseLDA", "spls", "stepLDA", "stepQDA", "svmBoundrangeString",
    "svmExpoString", "svmLinear", "svmLinear2", "svmLinear3", "svmLinearWeights",
    "svmLinearWeights2", "svmPoly", "svmRadial", "svmRadialCost",
    "svmRadialSigma", "svmRadialWeights", "svmSpectrumString", "tan",
    "tanSearch", "treebag", "vglmAdjCat", "vglmContRatio", "vglmCumulative",
    "widekernelpls", "wsrf", "xgbDART", "xgbLinear", "xgbTree", "xyf"
  )
library(flix)
library(data.table)
library(plyr)
library(dplyr)
library(caret)

load <- function() {
  mydf<-read.csv2('marketing_dataset.csv')
  return(mydf)
}
mydf<-load()
dv_name<-'y'
iv_names<-setdiff(colnames(mydf), 'y')


dt<-make_ads(dt = mydf, iv_names = iv_names, dv_name = dv_name, keep_nominal = FALSE)

#models_to_try <- c("ada")
#models_to_try <- c("ctree")

ans<-calc_models(model_names=models_to_try, cbind(dt, y=mydf[[dv_name]]), dv_name, iv_names, adaptive = TRUE )
