library(readr)
library(tidyverse)
library(mlr3pipelines)
# install learners
library(mlr3learners)
library(mlr3proba)
library(mlr3extralearners)
library(paradox) #ParamSet
library(mlr3tuning) #AutoTuner
library(mlr3pipelines)
library(reticulate)
library(survex)
# control Pyhon warnings through reticulate:
warnings <- import("warnings")
# ignore / suppress:
warnings$simplefilter("ignore")

#Load Functions
source("define_learners_v1.R")
source("run_simualation_v1.R")


#PATH
main_dir = getwd()

#Main Variables
# cc <- commandArgs(trailingOnly  = TRUE)
# Sc <- as.character(cc[1])
# ShortRun <- as.character(cc[2])
# ncr <- as.integer(cc[3])
# nwk <- as.integer(cc[4])

Sc <- "PH"
ShortRun <- "1"

if(Sc=="PH"){
  switch (ShortRun,
    #Sim 1
    "1" = run_simualation(main_dir, sim_id = 1, sim_scenario = "PH_50_500_additive", n_scenarios=40, n_workers=40, n_evals=30),
    #Sim 2
    "2" = run_simualation(main_dir, sim_id = 2, sim_scenario = "PH_70_500_additive", n_scenarios=40, n_workers=40, n_evals=30),
    #Sim 3
    "3" = run_simualation(main_dir, sim_id = 3, sim_scenario = "PH_90_500_additive", n_scenarios=40, n_workers=40, n_evals=30),
    
    #Sim 4
    "4" = run_simualation(main_dir, sim_id = 4, sim_scenario = "PH_50_1000_additive", n_scenarios=40, n_workers=40, n_evals=30),

    #Sim 5 #starts at 11.08 6:00 AM
    "5" = run_simualation(main_dir, sim_id = 5, sim_scenario = "PH_70_1000_additive", n_scenarios=40, n_workers=40, n_evals=30),
    
    #Sim 6 #starts at 11.08 5:21 PM & Finishes at 12.08 3:28
    "6" = run_simualation(main_dir, sim_id = 6, sim_scenario = "PH_90_1000_additive", n_scenarios=40, n_workers=40, n_evals=30),
    
    #Sim 7  #starts at 12.08 11:30 AM & Finishes at 14.08 10:34
    "7" = run_simualation(main_dir, sim_id = 7, sim_scenario = "PH_50_5000_additive", n_scenarios=40, n_workers=40, n_evals=30),
    
    #Sim 8 #starts at 14.08 11:42 PM & Finishes at 17.08 07:03
    "8" = run_simualation(main_dir, sim_id = 8, sim_scenario = "PH_70_5000_additive", n_scenarios=40, n_workers=40, n_evals=30),
    
    #Sim 9 #starts at 17.08 07:03 & Finishes at 21.08 04;15
    "9" = run_simualation(main_dir, sim_id = 9, sim_scenario = "PH_90_5000_additive", n_scenarios=40, n_workers=40, n_evals=30)
  )
} else if (Sc=="NLI"){
  switch (ShortRun,
          
    #Sim 10
    "19" = run_simualation(main_dir, sim_id = 19, sim_scenario = "PH_50_500_NLI", n_scenarios=ncr, n_workers=nwk, n_evals=30, NLI=TRUE),
    
    #Sim 11
    "20" = run_simualation(main_dir, sim_id = 20, sim_scenario = "PH_70_500_NLI", n_scenarios=ncr, n_workers=nwk, n_evals=30, NLI=TRUE),
    
    #Sim 12
    "21" = run_simualation(main_dir, sim_id = 21, sim_scenario = "PH_90_500_NLI", n_scenarios=ncr, n_workers=nwk, n_evals=30, NLI=TRUE),
    
    #Sim 13
    "22" = run_simualation(main_dir, sim_id = 22, sim_scenario = "PH_50_1000_NLI", n_scenarios=ncr, n_workers=nwk, n_evals=30, NLI=TRUE),
    
    #Sim 14
    "23" = run_simualation(main_dir, sim_id = 23, sim_scenario = "PH_70_1000_NLI", n_scenarios=ncr, n_workers=nwk, n_evals=30, NLI=TRUE),
    
    #Sim 15 
    "24" = run_simualation(main_dir, sim_id = 24, sim_scenario = "PH_90_1000_NLI", n_scenarios=ncr, n_workers=nwk, n_evals=30, NLI=TRUE),
    
    #Sim 16  
    "25" = run_simualation(main_dir, sim_id = 25, sim_scenario = "PH_50_5000_NLI", n_scenarios=ncr, n_workers=nwk, n_evals=30, NLI=TRUE),
    
    #Sim 17 
    "26" = run_simualation(main_dir, sim_id = 26, sim_scenario = "PH_70_5000_NLI", n_scenarios=ncr, n_workers=nwk, n_evals=30, NLI=TRUE),
    
    #Sim 18
    "27" = run_simualation(main_dir, sim_id = 27, sim_scenario = "PH_90_5000_NLI", n_scenarios=ncr, n_workers=nwk, n_evals=30, NLI=TRUE)
    
  )
}





