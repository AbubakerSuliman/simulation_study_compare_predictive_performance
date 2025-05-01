define_learners <- function(seed_r, tune_folds=3, n_evals=1, mtry_lwr = 3, mtry_upr = 7, NLI = FALSE){
  #browser()
  set.seed(seed_r)
  rsmp_tune_cv = rsmp("holdout")
  trm_tune = trm("evals", n_evals = n_evals)
  
  fallback = lrn("surv.kaplan")
  
  scale_po <- po("scale", param_vals = list(affect_columns = selector_name(c("x1", "x4", "x5", "x6", "x7"))))

  learners_lst = list()
  
  ###################################
  ### A. Baselearners ###############
  ###################################
  
  learners_lst$kaplan = as_learner(scale_po %>>% lrn("surv.kaplan", id = "class_nonpar_kaplan"))
  learners_lst$kaplan$encapsulate("evaluate", fallback)

  learners_lst$cox = as_learner(scale_po %>>% lrn("surv.coxph", id = "class_semipar_coxph"))
  learners_lst$cox$encapsulate("evaluate", fallback)
 
  learners_lst$aft = as_learner(scale_po %>>% lrn("surv.parametric", id = "class_par_aft"))
  learners_lst$aft$encapsulate("evaluate", fallback)
  


  
  learners_lst$penalized = auto_tuner(
    learner = scale_po %>>% lrn("surv.penalized",
                                    id = "class_semipar_penalized",
                                    lambda1 = to_tune(p_int(0, 10)),
                                    lambda2 = to_tune(p_int(0, 10))),
    resampling = rsmp_tune_cv,
    measure = msr("surv.cindex"),
    terminator = trm_tune,
    tuner = tnr("random_search"), 
    store_models = TRUE
  )
  learners_lst$penalized$encapsulate("evaluate", fallback)

  
  
  if(NLI){
    po_mutate1 = po("mutate", id = "m1",
                    mutation = list(x1 = ~(x1^1.25),
                                    x4 = ~(x4^2))
                    )
    scale_po1 = po("scale", id = "s1", param_vals = list(affect_columns = selector_name(c("x1", "x4", "x5", "x6", "x7"))))
    po_mutate2 = po("mutate", id = "m2",
                    mutation = list(x23 = ~(x2 * x3),
                                    x25 = ~(x2 * x5))
                    )
    scale_po_nli = po_mutate1 %>>% scale_po1 %>>% po_mutate2 
    
    learners_lst$aft_nli = as_learner(scale_po_nli %>>% lrn("surv.parametric", id = "class_par_aft_nli"))
    learners_lst$aft_nli$encapsulate("evaluate", fallback)
  
    
    learners_lst$cv_glmnet_nli = auto_tuner(
      learner = scale_po_nli %>>% lrn("surv.cv_glmnet",
                                  id = "class_semipar_cvglmnet_nli",
                                  alpha = to_tune(p_dbl(0, 1))),
      resampling = rsmp_tune_cv,
      measure = msr("surv.cindex"),
      terminator = trm_tune,
      tuner = tnr("random_search"),
      store_models = TRUE
    )

    learners_lst$cv_glmnet_nli$encapsulate("evaluate", fallback)
   
    
    learners_lst$penalized_nli = auto_tuner(
      learner = scale_po_nli %>>% lrn("surv.penalized",
                                  id = "class_semipar_penalized_nli",
                                  lambda1 = to_tune(p_int(0, 10)),
                                  lambda2 = to_tune(p_int(0, 10))),
      resampling = rsmp_tune_cv,
      measure = msr("surv.cindex"),
      terminator = trm_tune,
      tuner = tnr("random_search"), 
      store_models = TRUE
    )
    learners_lst$penalized_nli$encapsulate("evaluate", fallback)

  }
  
  ###################################
  ### B. RandomForestlearners #######
  ###################################
  ###############################
  #Random Survival Forests #####
  ##############################
  
  learners_lst$at_lrn_rfsrc = auto_tuner(
    learner = scale_po %>>% lrn("surv.rfsrc", 
                  id = "ml_ranfor_rfsrc_brier", 
                  splitrule = "bs.gradient",
                  ntree = to_tune(p_int(250, 750)),
                  mtry = to_tune(p_int(2, 7)),
                  nodesize = to_tune(p_int(15, 25))),
    resampling = rsmp_tune_cv,
    measure = msr("surv.cindex"),
    terminator = trm_tune,
    tuner = tnr("random_search"), 
    store_models = TRUE
  )
  
  
  learners_lst$at_lrn_rfsrc$encapsulate("evaluate", fallback)
  
  
  ###################################################
  #Accelerated Oblique Random Survival Forests  #####
  ###################################################
  
  learners_lst$at_lrn_aorsff = auto_tuner(
    learner = scale_po %>>% lrn("surv.aorsf", 
                  id = "ml_ranfor_aorsf_fast", 
                  control_type = "fast", #cph, net
                  split_rule = 'cstat', #'logrank'
                  n_thread = 1,
                  control_net_df_target = 3,
                  n_tree = to_tune(p_int(lower = 250, upper = 750)),
                  leaf_min_obs = to_tune(p_int(lower = 15, upper = 25)),
                  mtry = to_tune(p_int(lower = mtry_lwr, upper = mtry_upr))
    ),
    resampling = rsmp_tune_cv,
    measure = msr("surv.cindex"),
    terminator = trm_tune,
    tuner = tnr("random_search"), 
    store_models = TRUE
  )
  
  learners_lst$at_lrn_aorsff$encapsulate("evaluate", fallback)
  
  learners_lst$at_lrn_aorsfc = auto_tuner(
    learner = scale_po %>>% lrn("surv.aorsf", 
                  id = "ml_ranfor_aorsf_cph", 
                  control_type = "cph", #cph, net
                  split_rule = 'cstat', #'logrank'
                  n_thread = 1,
                  control_net_df_target = 3,
                  n_tree = to_tune(p_int(lower = 250, upper = 750)),
                  leaf_min_obs = to_tune(p_int(lower = 15, upper = 25)),
                  mtry = to_tune(p_int(lower = mtry_lwr, upper = mtry_upr))
    ),
    resampling = rsmp_tune_cv,
    measure = msr("surv.cindex"),
    terminator = trm_tune,
    tuner = tnr("random_search"), 
    store_models = TRUE
  )
  learners_lst$at_lrn_aorsfc$encapsulate("evaluate", fallback)

  
  learners_lst$at_lrn_aorsfn = auto_tuner(
    learner = scale_po %>>% lrn("surv.aorsf",
                  id = "ml_ranfor_aorsf_net",
                  control_type = "net", #cph, net
                  split_rule = 'cstat', #'logrank'
                  n_thread = 1,
                  control_net_df_target = 3,
                  n_tree = to_tune(p_int(lower = 250, upper = 750)),
                  leaf_min_obs = to_tune(p_int(lower = 15, upper = 25)),
                  mtry = to_tune(p_int(lower = mtry_lwr, upper = mtry_upr))
    ),
    resampling = rsmp_tune_cv,
    measure = msr("surv.cindex"),
    terminator = trm_tune,
    tuner = tnr("random_search"), 
    store_models = TRUE
  )
  learners_lst$at_lrn_aorsfn$encapsulate("evaluate", fallback)

}
