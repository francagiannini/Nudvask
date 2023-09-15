
# packages ----
library(tidyverse)
library(ggpubr)
library(GGally)
library(RColorBrewer)
library(leaps)
library(caret)
library(randomForest)
#library(gbm)
library(parallel)
library(doParallel)
library(rpart)
library(mlbench)

# data ----

df_m_imp <- readRDS("df_m_imp.RDS")
summary(df_m_imp)

df_gen <-
  df_m_imp |>
  dplyr::select(
    #"merge_mon",
    #"ident",
    #"merge_id",
    "harvest_year",
    #"year",
    "month",
    "WC",
    "N_mineral_spring",
    #"N_mineral_autuomn",
    "N_min_year.1",
    "N_min_year.2",
    "N_f_year",
    "N_f_year.1",
    "N_f_year.2",
    "N_org_year",
    "N_org_year.1",
    "N_org_year.2",
    #"N_from_grassing_animals",
    #"N_topsoil",
    #"clay",
    #"Gamma",
    #"jbnr",
    "prev_Main_nles5",
    "prev_Winter_nles5",
    "Main_nles5",
    "Winter_nles5",
    "meancon",
    #"mediancon",
    #"sdcon",
    #"n",
    #"crop_main_name",
    #"crop_winter_name" ,
    "site_eng",
    #"data_use",
    #"source",
    #"p_no",
    #"DMIGRIDNUM",
    #"X_CENTRE",
    #"Y_CENTRE",
    #"X",
    #"Y",
    #"geometry",
    #"Id",
    "afstro_sum_month" ,
    "Precip_sum_month" ,
    "AirTemp_ave_month"  ,
    "Globrad_ave_month" ,
    "tvspp_month",
    "drain_days",
    "Precip_sum60",
    "Precip_sum90",
    "Precip_sum180",
    "Precip_sum365",
    "AirTemp_ave60",
    "AirTemp_ave90",
    "AirTemp_ave180",
    "tvspp_60",
    "tvspp_90",
    "afstro_cumsumhy",
    "Precip_cumsumhy",
    "afstro_sumhy",
    "Precip_sumhy",
    "AirTemp_avehy",
    "Globrad_avehy",
    #"weigth_astro",
    #"season", 
    "clay_cat"
  ) |> 
  #filter(!is.na(crop_main_name))|>
  filter(!meancon == 0) |>
  mutate(season=case_when(month %in% 3:5 ~ 'spring',
                          month %in% 6:8 ~ 'summer',
                          month %in% 9:11 ~ 'autumn',
                          TRUE ~ 'winter')) |> 
  drop_na()


remove(df_m_imp)
# Parallel 

registerDoParallel(makeCluster(detectCores()-4))

# random forest simplifying ----

## full ----
df_gen_full <- df_gen |> 
  select(!c(site_eng, harvest_year)) |>
  drop_na()

df_gen_full <- df_gen_full[complete.cases(df_gen_full),] |>  
  ##sample_frac(0.2) |> 
  filter(!Winter_nles5==7)

# train model
fitControl <- trainControl(
  ## 10-fold CV
  method = "repeatedcv",
  number = 5,
  ## repeated ten times
  repeats = 2)

#create tunegrid
tunegrid <- expand.grid(.mtry = seq(30,39,3)
                        )

#train with different
modellist <- list()

for (ntree in c(1000, 1500, 2000, 2500)) {
rffit_full <- caret::train(
  log(meancon) ~ .,#month+ WC +N_mineral_spring+ N_min_year.1 +N_min_year.2 +N_f_year,
  data = df_gen_full,
  method = 'rf',
  #preProc = c("center", "scale"),
  tuneGrid = tunegrid,
  metric = 'RMSE',
  ntree = ntree,
  trControl = fitControl,
  verbose = FALSE,
  trace = FALSE)
key <- toString(ntree)
modellist[[key]] <- rffit_full
}


#Compare results

sapply(modellist,summary)

rffit_full$results[rownames(rffit_full$bestTune),]$RMSE/mean(log(df_gen_full$meancon))* 100

sqrt(mean((fitt_rf$finalModel$predicted-log(df_gen$meancon))^2))/mean(log(df_gen$meancon))* 100

plot(rffit_full)

saveRDS(modellist,"rffit_full.RDS")

## full independent ----

df_gen_full_ind <- df_gen |> 
  select(!c(site_eng, harvest_year,
            afstro_sum_month,afstro_cumsumhy,afstro_sumhy)) |>
  drop_na()

df_gen_full_ind <- df_gen_full_ind[complete.cases(df_gen_full_ind),] |>  
  ##sample_frac(0.2) |> 
  filter(!Winter_nles5==7)

tunegrid <- expand.grid(.mtry = seq(30,36,3))

modellist_ind <- list()

for (ntree in c(1000, 1500, 2000, 2500)) {
  rffit_full_ind <- caret::train(
    log(meancon) ~ .,#month+ WC +N_mineral_spring+ N_min_year.1 +N_min_year.2 +N_f_year,
    data = df_gen_full_ind,
    method = 'rf',
    #preProc = c("center", "scale"),
    tuneGrid = tunegrid,
    metric = 'RMSE',
    ntree = ntree,
    trControl = fitControl,
    verbose = FALSE,
    trace = FALSE)
  key <- toString(ntree)
  modellist[[key]] <- rffit_full_ind
}


saveRDS(modellist_ind,"rffit_full_ind.RDS")


## simplified ----


## simplified independent ----





df_gen <- cbind(df_gen,
                "rf_pred"=as.data.frame(exp(fitt_rf$finalModel$predicted))[,1]#,
                #as.data.frame(exp(fitt_gbm$finalModel$fit))[,1]
)

