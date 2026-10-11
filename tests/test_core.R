#!/usr/bin/env Rscript
source(file.path("R", "soil_classification.R"))
assert_true <- function(x, msg) if (!isTRUE(x)) stop(msg, call. = FALSE)
assert_equal <- function(a,b,tolerance=1e-10) if (!isTRUE(all.equal(a,b,tolerance=tolerance,check.attributes=FALSE))) stop("Assertion failed", call.=FALSE)
for (f in c(file.path("R","soil_classification.R"),file.path("R","cli.R"),"basic_results.R","results_cg.R","results_rf.R")) parse(file=f)
assert_equal(length(main_feature_combinations()),33L)
train <- data.frame(x=c(0,5,10),y=c(10,20,30)); v <- data.frame(x=2.5,y=15); t <- data.frame(x=7.5,y=25)
s <- scale_split(train,v,t,c("x","y")); assert_equal(s$train$x,c(0,.5,1)); assert_equal(s$validation$x,.25); assert_equal(s$test$y,.75)
assert_equal(macro_recall(c("A","B","B","B"),c("A","A","B","B")),.75)
iqr <- data.frame(Depth_m=1:10,x=c(1,1,1,1,1,2,2,2,2,100),class=c(rep(1,5),rep(2,5)))
assert_true(10L %in% find_iqr_candidates(iqr,"class",c("Depth_m","x")),"IQR screening failed")
bd <- data.frame(x=c(1:5,10:13),y=c(2:6,20:23),class=c(rep(1,5),rep(2,4)))
b <- balance_training_data(bd,"class",c("x","y"),minimum_per_class=4L); assert_equal(as.integer(table(b$class)),c(8L,8L))
if (file.exists("data.RData")) { a <- load_archived_workspace(); assert_equal(length(a$folds_complete),10L); assert_equal(length(a$folds_geology),10L); assert_equal(length(a$validation_folds),10L) }
if (requireNamespace("kknn",quietly=TRUE)) {
 set.seed(123); n<-100L; d<-data.frame(x=c(rnorm(50,-2,.25),rnorm(50,2,.25)),y=c(rnorm(50,-2,.25),rnorm(50,2,.25)),class=c(rep(1,50),rep(2,50)))
 folds<-split(seq_len(n),rep(1:10,each=10)); vf<-c(2:10,1)
 r<-run_configuration(d,folds,vf,"class",c("x","y"),"rectangular",minimum_per_class=8L,max_k=5L,verbose=FALSE)
 assert_equal(nrow(r$folds),10L); assert_true(all(is.finite(r$folds$accuracy)),"Smoke test returned non-finite accuracy")
}
cat("All KNN/DWNN core tests passed.\n")
