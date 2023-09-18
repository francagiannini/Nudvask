
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
  filter(!Winter_nles5==7) |> 
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

summary(resamples(modellist))

plot(modellist$'2000')

modellist$'2000'$results[rownames(modellist$'2000'$bestTune),]$RMSE/mean(log(df_gen_full$meancon))* 100

sqrt(mean((modellist$'2000'$finalModel$predicted-log(df_gen$meancon))^2))/mean(log(df_gen$meancon))* 100

saveRDS(modellist,"rffit_full.RDS")

modellist <- readRDS("rffit_full.RDS")

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

modellist_ind <- readRDS("rffit_full_ind.RDS")

## simplified ----

df_gen_simp <- df_gen |> 
  select(!c(site_eng, harvest_year,
            #afstro_sum_month,afstro_cumsumhy,afstro_sumhy,
            "N_min_year.1","N_min_year.2","N_f_year.1","N_f_year.2",
            "N_org_year","N_org_year.1","N_org_year.2",
            "prev_Main_nles5","prev_Winter_nles5",
            "tvspp_month","drain_days",
            "Precip_sum60","Precip_sum90","Precip_sum180",
            "AirTemp_ave60","AirTemp_ave90","AirTemp_ave180",
            "tvspp_60","tvspp_90","season","N_f_year" 
            
            )) |>
  drop_na()

df_gen_simp <- df_gen_simp[complete.cases(df_gen_simp),] |>  
  ##sample_frac(0.2) |> 
  filter(!Winter_nles5==7)

tunegrid <- expand.grid(.mtry = seq(6,18,4))

modellist_sim <- list()

for (ntree in c(1500, 2000, 2500, 5000)) {
  rffit_sim <- caret::train(
    log(meancon) ~ .,#month+ WC +N_mineral_spring+ N_min_year.1 +N_min_year.2 +N_f_year,
    data = df_gen_simp,
    method = 'rf',
    #preProc = c("center", "scale"),
    tuneGrid = tunegrid,
    metric = 'RMSE',
    ntree = ntree,
    trControl = fitControl,
    verbose = FALSE,
    trace = FALSE)
  key <- toString(ntree)
  modellist_sim[[key]] <- rffit_sim
}

saveRDS(modellist_sim,"rffit_sim.RDS")

modellist_sim <- readRDS("rffit_sim.RDS")

res <- resamples(modellist_sim)
summary(res)

## simplified independent ----

df_gen_simp_ind <- df_gen |> 
  select(!c(site_eng, harvest_year,
            afstro_sum_month,afstro_cumsumhy,afstro_sumhy,
            "N_min_year.1","N_min_year.2","N_f_year.1","N_f_year.2",
            "N_org_year","N_org_year.1","N_org_year.2",
            "prev_Main_nles5","prev_Winter_nles5",
            "tvspp_month","drain_days",
            "Precip_sum60","Precip_sum90","Precip_sum180",
            "AirTemp_ave60","AirTemp_ave90","AirTemp_ave180",
            "tvspp_60","tvspp_90","season","N_f_year" 
            
  )) |>
  drop_na()

df_gen_simp_ind <- df_gen_simp_ind[complete.cases(df_gen_simp_ind),] |>  
  ##sample_frac(0.2) |> 
  filter(!Winter_nles5==7)

fitControl <- trainControl(
  ## 10-fold CV
  method = "repeatedcv",
  number = 5,
  ## repeated ten times
  repeats = 2)

tunegrid <- expand.grid(.mtry = seq(6,15,3))

modellist_sim_ind <- list()

for (ntree in c(1500, 2000, 2500, 5000)) {
  rffit_sim_ind <- caret::train(
    log(meancon) ~ .,#month+ WC +N_mineral_spring+ N_min_year.1 +N_min_year.2 +N_f_year,
    data = df_gen_simp_ind,
    method = 'rf',
    #preProc = c("center", "scale"),
    tuneGrid = tunegrid,
    metric = 'RMSE',
    ntree = ntree,
    trControl = fitControl,
    verbose = FALSE,
    trace = FALSE)
  key <- toString(ntree)
  modellist_sim_ind[[key]] <- rffit_sim_ind
}

saveRDS(modellist_sim_ind,"rffit_sim_ind.RDS")

modellist_sim_ind <- readRDS("rffit_sim_ind.RDS")

res <- resamples(modellist_sim_ind)
summary(res)


## prediction and communicating results ----

df_gen$month2 <- df_gen$month^2
df_gen$month3 <- df_gen$month^3

df_gen$afstro_cumsumhy2 <- df_gen$afstro_cumsumhy^2
df_gen$afstro_cumsumhy3 <- df_gen$afstro_cumsumhy^3

#df_gen |> ggplot(aes(x=month, y=Precip_sum365))+ geom_point()+geom_smooth()

library(lmerTest)

linear_translate <- lmer(
  log(meancon)~month+month2+month3+
    afstro_cumsumhy+afstro_cumsumhy2+afstro_cumsumhy3+
    AirTemp_avehy+AirTemp_ave_month+
    WC+
    N_mineral_spring+Precip_sum365+
    Winter_nles5*afstro_cumsumhy2+
    Winter_nles5+clay_cat+N_org_year+Globrad_avehy+ Globrad_ave_month+
    month:clay_cat+month2:clay_cat+month3:clay_cat+(1|harvest_year),
  data = df_gen)

summary(linear_translate)

#df_gen$lin_pred <- exp(predict(linear_translate, df_gen))


linear_translate_ind<- lmer(
  log(meancon)~
    month+month2+month3+
    #afstro_cumsumhy+afstro_cumsumhy2+afstro_cumsumhy3+
    AirTemp_avehy+AirTemp_ave_month+
    WC+
    N_mineral_spring+Precip_sum365+
    Winter_nles5*afstro_cumsumhy2+
    Winter_nles5+clay_cat+N_org_year+Globrad_avehy+ Globrad_ave_month+
    month:clay_cat+month2:clay_cat+month3:clay_cat+(1|harvest_year),
  
  data = df_gen)

summary(linear_translate_ind)

#df_gen$lin_pred_simp <- exp(predict(linear_translate_simp, df_gen))

df_gen_pred <- cbind(df_gen,
                "pred_rf_full" = as.data.frame(exp(modellist$'2000'$finalModel$predicted))[,1],
                "pred_rf_full_ind" =as.data.frame(exp(modellist_ind$'2000'$finalModel$predicted))[,1] ,
                "pred_rf_sim" = as.data.frame(exp(modellist_sim$'2500'$finalModel$predicted))[,1],
                "pred_rf_sim_ind" = as.data.frame(exp(modellist_sim_ind$'1500'$finalModel$predicted))[,1],
                "pred_lin"=exp(predict(linear_translate, df_gen)),
                "pred_lin_ind"=exp(predict(linear_translate_ind, df_gen)) 
                ) |> 
  mutate(leach_obs=meancon*afstro_sum_month/100,
         
         leach_rf_full=pred_rf_full*afstro_sum_month/100,
         leach_rf_full_ind=pred_rf_full_ind*afstro_sum_month/100,
         
         leach_rf_sim=pred_rf_sim*afstro_sum_month/100,
         leach_rf_sim_ind=pred_rf_sim_ind*afstro_sum_month/100,
         
         leach_lin=pred_lin*afstro_sum_month/100,
         leach_lin_ind=pred_lin_ind*afstro_sum_month/100)

testRes  <-  df_gen_pred |> select(starts_with("leach_")) |> corrplot::cor.mtest(conf.level = 0.95)

df_gen_pred |> select(starts_with("leach_")) |> cor() |>
  corrplot::corrplot( p.mat = testRes$p, method = 'circle', 
                      type = 'lower', addCoef.col ='White',
           insig='blank',tl.col = 'black',
           order = 'AOE', diag = FALSE)
  
saveRDS(df_gen, "df_gen1809.RDS")

df_gen_pred |> 
  mutate(clay_plot=fct_recode(clay_cat,low="low", 'midle-high'="middle", 'midle-high'="high")) |> 
  filter(Winter_nles5==c("1","3","5","9")) |> 
  #filter(Main_nles5==1 & Winter_nles5 == 4 & harvest_year== c(1998,1999,2008))|> 
  #select(month, rf_pred, meancon,jbnr,harvest_year,clay_cat) |> 
  pivot_longer(cols=c(leach_obs,leach_rf_full, leach_rf_full_ind,leach_lin_ind),#starts_with("leach_"), 
               values_to = "Nleaching", names_to = "Measurement") |> 
  ggplot(aes(x=month,y=Nleaching,fill=Measurement, col=Measurement))+
  geom_boxplot(alpha=.1, aes(group=interaction(Measurement,month), fill=Measurement
  ) )+
 # scale_color_manual(values=c("#999999", "#E69F00", "#9467bd"))+
 # scale_fill_manual(values=c("#999999", "#E69F00", "#9467bd"))+
  geom_smooth(alpha=0.1, aes(linetype=Measurement))+
  geom_point(alpha=0.2, size=0.1)+
  facet_grid(Winter_nles5~clay_plot)+
  scale_x_continuous(breaks = seq(1,12,1))+
  scale_y_continuous(limits = c(0,30))+
  theme(legend.position = "bottom", panel.background = NULL)+
  guides(linetype=FALSE,fill=FALSE, col=guide_legend("Estimation:"))+
  ylab("N leaching (Kg N/ha)")#+
  #theme_bw()



df_gen_pred |> 
  #mutate(clay_plot=fct_recode(clay_cat,low="low", 'midle-high'="middle", 'midle-high'="high")) |> 
  #filter(Winter_nles5==c("1","3","5","9")) |> 
  #filter(Main_nles5==1 & Winter_nles5 == 4 & harvest_year== c(1998,1999,2008))|> 
  #select(month, rf_pred, meancon,jbnr,harvest_year,clay_cat) |> 
  pivot_longer(cols=c(leach_obs,leach_rf_full, leach_rf_full_ind,leach_lin_ind),#starts_with("leach_"), 
               values_to = "Nleaching", names_to = "Measurement") |> 
  ggplot(aes(x=log(Nleaching) , fill=Measurement, col=Measurement))+
  #geom_density(stat="density", position="identity", alpha=0.2)+
  
  geom_step(aes(y=..y..),stat="ecdf")+
  #geom_boxplot(alpha=.1, aes(group=interaction(Measurement,month), fill=Measurement
 # ) )+
  # scale_color_manual(values=c("#999999", "#E69F00", "#9467bd"))+
  # scale_fill_manual(values=c("#999999", "#E69F00", "#9467bd"))+
  #geom_smooth(alpha=0.1, aes(linetype=Measurement))+
  #geom_point(alpha=0.2, size=0.1)+
  #facet_grid(Winter_nles5~clay_plot)+
  scale_x_continuous(breaks = c(-10,5))+
  #scale_y_continuous(limits = c(0,30))+
  theme_bw()+
  theme(legend.position = "bottom", panel.background = NULL)+
  guides(linetype=FALSE,fill=FALSE, col=guide_legend("Estimation:"))+
 ylab("Percentile")
  # ylab("Frequency")
