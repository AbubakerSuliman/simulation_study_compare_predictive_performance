rm(list = ls())
library(survival)
library(truncnorm)
library(simstudy)
library(survminer)
library(tidyverse)
set.seed(1965)
#Provide synthetic survival data

defData_r = function(sample_sz=500, seed_r=123){
  set.seed(seed_r)
  # Baseline data definitions
  
  # Simulate Age
  def <- defData(varname = "x1", formula = 60, variance = 10, dist = "normal")
  
  # Simulate Gender
  def <- defData(def, varname = "x2", formula = 0.53, dist = "binary")

  df_tmp = genData(sample_sz, def)
  
  # Simulate Smoke
  b=c(`(Intercept)` = 0.65, x1 = -0.05, x2 = 0.48)
  a=model.matrix(~x1 + x2, data = df_tmp)
  bx=a%*%b
  p_x3=exp(bx)/(1+exp(bx))
  df_tmp$x3=rbinom(sample_sz, size=1, p=p_x3)
  
  # Simulate Total Cholesterol
  b=c(`(Intercept)` = 199.8, x1 = 0.02, x2 = -11.43, x3 = -1.26)
  a=model.matrix(~x1 + x2 + x3, data = df_tmp)
  bx=a%*%b
  df_tmp$x4=rtruncnorm(sample_sz, a = 85, b = 408, mean = bx, sd = 35)
  
  # Simulate Systolic Blood Pressure
  b=c(`(Intercept)` = 83.23, x1 = 0.74, x2 = -2.57, x3 = 0.92, x4 = 0.04)
  a=model.matrix(~x1 + x2 + x3 + x4, data = df_tmp)
  bx=a%*%b
  df_tmp$x5=rtruncnorm(sample_sz, a = 67, b = 205, mean = bx, sd = 21)
  
  return(df_tmp)
}

normalize_r = function(x, upper_num=15) {
  x_n = ((x - min(x))/(max(x) - min(x))) * (upper_num-1)
  x_n = x_n + 1
  return(x_n)
}

samples_seeds_vec = numeric()
ph_censrate_vec = numeric()
seed_init = 1
cens_rate = 0.5 #0.5, 0.7, 0.9
sample_sz_r = 500 #500, 1000, 5000
scale_t = 7 #For 50% scale = 7, For 70% scale = 26, For 90% scale = 150
effect_r = c("additive")

sim_data_path = sprintf("%s/simulation_data", getwd())
data_file_ph = sprintf("%s/%s", sim_data_path, sprintf("%s_%d_%d_%s", "PH", cens_rate*100, sample_sz_r, effect_r))
if(file.exists(data_file_ph)) unlink(data_file_ph, recursive = TRUE)
dir.create(data_file_ph)

while (length(samples_seeds_vec)<40) {
  #browser()
  set.seed(seed_init)
  #random_num = runif(1)
  #For 50% scale = 7, shape = 1/2; , scale = 0.06, shape = 1
  #For 70% scale = 26, shape = 1/2; , scale = 0.06, shape = 1
  #For 90% scale = 150, shape = 1/2; , scale = 0.06, shape = 1
  sdef <- defSurv(varname = "survTime", formula = "0.080*x1 - 0.869*x2 + 0.297*x3 + 0.006*x4 + 0.017*x5", scale = scale_t, shape = 1/2)
  sdef <- defSurv(sdef, varname = "censorTime", scale = 0.06, shape = 1)
  
  dtSurv <- genSurv(defData_r(sample_sz = sample_sz_r, seed_r = seed_init), sdef, timeName = "time", censorName = "censorTime",
                    eventName = "event")
  
  #mean(dtSurv$event);summary(dtSurv$time)
  if(between(mean(dtSurv$event==0), cens_rate-0.015, cens_rate+0.015)){
    samples_seeds_vec = append(samples_seeds_vec, seed_init)
    ph_censrate_vec = append(ph_censrate_vec, mean(dtSurv$event==0))

    #Add non-informative variables 
    set.seed(seed_init)
    dtSurv$x6 = rtruncnorm(sample_sz_r, a = 24, b = 120, mean = 49, sd = 14)
    dtSurv$x7 = rtruncnorm(sample_sz_r, a=16, b=54, mean=25, sd=5.6)
    dtSurv$x8 = rbinom(sample_sz_r, 1, 0.05)
    
    dtSurv = dtSurv %>% select(num_range("x", 1:8), time, event)
    
    dtSurv$time = normalize_r(dtSurv$time)

    #Write data to disk
    write.csv(dtSurv, sprintf("%s/sim_seed_%d.csv", data_file_ph, seed_init), row.names = FALSE)

    
  }
  seed_init = seed_init + 1
}

summary(ph_censrate_vec)
