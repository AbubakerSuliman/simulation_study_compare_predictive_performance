run_simualation = function(main_dir, sim_id, sim_scenario, n_scenarios=1, n_workers=5, n_evals=1, NLI_r = FALSE){
  
  lst_files = list.files(sprintf("simulation_data/%s", sim_scenario))
  
  tsks_sims = map(lst_files[seq_len(n_scenarios)],~{
    df_tmp = read_csv(sprintf("simulation_data/%s/%s", sim_scenario, .x), show_col_types = FALSE)
    #mean(df_tmp$event)
    tsk_tmp = as_task_surv(df_tmp, time = "time", event = "event", type = "right", id = sprintf("sim%s", str_extract(.x, "\\d+")))
    tsk_tmp$set_col_roles("event", c("target", "stratum"))
  })
  
  learners_lst = define_learners(seed_r=sim_id, n_evals = n_evals, NLI = NLI_r)
  
  
  set.seed(sim_id)
  rsmp_outer_cv = rsmp("cv", folds = 3)
  

  msrs =  list(
               msr("surv.dcalib", id = "D-Calibration"),
               msr("surv.graf", id = "Brier score"),
               msr("surv.cindex", id = "C_Harrell"),
               msr("surv.cindex", weight_meth = "G2", id = "C_Uno"),
               msr("time_train", id = "time_train"),
               msr("time_predict", id = "time_predict"))
  
  
  # set logger to maximum information - save to scratch
  file_path = sprintf("%s/Simulation_Results/%s", main_dir, sim_scenario)
  dir.create(file_path, showWarnings = FALSE, recursive = TRUE)
  options("mlr3.debug" = TRUE)
  logfile = sprintf("%s/log.txt", file_path)
  logger = lgr::get_logger("mlr3")
  logger$set_threshold("debug")
  logger$add_appender(lgr::AppenderJson$new(logfile), name = "json")
  
  
  
  if(n_scenarios<=20){
    #future::plan("multiprocess")
    future::plan("multisession", workers = n_workers)
    message("Number of parallel workers: ", future::nbrOfWorkers())
    
    start <- Sys.time()
    set.seed(sim_id)
    bmr = benchmark(benchmark_grid(tsks_sims, learners_lst, rsmp_outer_cv), store_models = FALSE)
    
    end <- Sys.time()
    difftime(end, start, units="hours")

    Score = bmr$score(msrs)
    Score = Score %>% select(-c(task, learner, resampling, prediction_test))
    saveRDS(Score, sprintf("%s/Score.rds", file_path))
    
    future::plan("sequential")
    # unlink logger
    logger$remove_appender("json")
  } else {
    ####################################################
    ####Exp 1 ##########################################
    ####################################################
    #future::plan("multiprocess")
    future::plan("multisession", workers = n_workers)
    message("Number of parallel workers: ", future::nbrOfWorkers())
    
    start <- Sys.time()
    set.seed(sim_id)
    bmr = benchmark(benchmark_grid(tsks_sims[1:20], learners_lst, rsmp_outer_cv), store_models = FALSE)
    
    end <- Sys.time()
    difftime(end, start, units="hours")
    
    Score = bmr$score(msrs)
    Score = Score %>% select(-c(task, learner, resampling, prediction_test))
    saveRDS(Score, sprintf("%s/Score1.rds", file_path))
    
    future::plan("sequential")
    # unlink logger
    logger$remove_appender("json")
    
    
    ####################################################
    ####Exp 2 #########################################
    ###################################################
    
    #future::plan("multiprocess")
    future::plan("multisession", workers = n_workers)
    message("Number of parallel workers: ", future::nbrOfWorkers())
    
    start <- Sys.time()
    set.seed(sim_id)
    bmr = benchmark(benchmark_grid(tsks_sims[21:n_scenarios], learners_lst, rsmp_outer_cv), store_models = FALSE)
    
    end <- Sys.time()
    difftime(end, start, units="hours")
    
    Score = bmr$score(msrs)
    Score = Score %>% select(-c(task, learner, resampling, prediction_test))
    saveRDS(Score, sprintf("%s/Score2.rds", file_path))
    
    future::plan("sequential")
    # unlink logger
    logger$remove_appender("json")
    
  }
  
}







