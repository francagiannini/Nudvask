
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

theme_set(theme_bw())
theme_update(panel.grid = element_blank())

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

df_gen_simp_ind <- droplevels(df_gen_simp_ind[complete.cases(df_gen_simp_ind),] )|>  
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

## simplified independent max reduced----

df_gen_simp_ind_max <- df_gen |> 
  select(!c(site_eng, harvest_year,
            afstro_sum_month,afstro_cumsumhy,afstro_sumhy,
            "N_min_year.1","N_min_year.2","N_f_year.1","N_f_year.2",
            "N_org_year","N_org_year.1","N_org_year.2",
            "prev_Main_nles5","prev_Winter_nles5",
            "tvspp_month","drain_days",
            "Precip_sum60","Precip_sum90","Precip_sum180",
            "AirTemp_ave60","AirTemp_ave90","AirTemp_ave180",
            "tvspp_60","tvspp_90","season","N_f_year","Globrad_ave_month",
            "Precip_sumhy", "Precip_sum365","AirTemp_avehy", "Precip_sum_month",
            "WC","Globrad_avehy"
            
            
  )) |>
  drop_na()

df_gen_simp_ind_max <- droplevels(df_gen_simp_ind_max[complete.cases(df_gen_simp_ind_max),]) |>  
  ##sample_frac(0.2) |> 
  filter(!Winter_nles5==7)

fitControl <- trainControl(
  ## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 2)

tunegrid <- expand.grid(.mtry = seq(1,6,1))

modellist_sim_ind_max <- list()

for (ntree in c(500, 1000, 3000, 6000)) {
  rffit_sim_ind_max <- caret::train(
    log(meancon) ~ .,
    data = df_gen_simp_ind_max,
    method = 'rf',
    #preProc = c("center", "scale"),
    tuneGrid = tunegrid,
    metric = 'RMSE',
    ntree = ntree,
    trControl = fitControl,
    verbose = FALSE,
    trace = FALSE)
  key <- toString(ntree)
  modellist_sim_ind_max[[key]] <- rffit_sim_ind_max
}

saveRDS(modellist_sim_ind_max,"rffit_sim_ind_max.RDS")

modellist_sim_ind_max <- readRDS("rffit_sim_ind_max.RDS")

res <- resamples(modellist_sim_ind_max)
summary(res)

plot(varImp(modellist_sim_ind_max$'1000'))

modellist_sim_ind_max$'1000'$finalModel$coefs

library(pdp)

variables <-list(colnames(df_gen_simp_ind_max))[[1]]
variables <- variables[variables != c("meancon","clay_cat")] 

pdp_func <- function(i) {
  
#m_c_plot <- 
  partial(modellist_sim_ind_max$'1000', 
                       pred.var = c(print(i),"clay_cat"), 
                       train = sample_frac(df_gen,0.1),
                       chull = TRUE)
  
  }

list_rf_sim_ind_max_partial <- lapply(variables, pdp_func)


autoplot(#m_c_plot
         list_rf_sim_ind_max_partial[6][[1]]
         , contour = TRUE) +
  geom_smooth()


# Linear model ---- 


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
    AirTemp_ave_month*clay_cat+
    Winter_nles5+clay_cat+N_org_year+Globrad_avehy+ Globrad_ave_month+
    month:clay_cat+month2:clay_cat+month3:clay_cat+(1|harvest_year),
  data = df_gen)

summary(linear_translate)

#df_gen$lin_pred <- exp(predict(linear_translate, df_gen))


linear_translate_ind<- lme4::lmer(
  log(meancon)~
    month+month2+month3+
    #afstro_cumsumhy+afstro_cumsumhy2+afstro_cumsumhy3+
    AirTemp_avehy+AirTemp_ave_month+
    WC+
    N_mineral_spring+Precip_sum365+
    #Winter_nles5*afstro_cumsumhy2+
    AirTemp_ave_month*clay_cat+
    Winter_nles5+clay_cat+N_org_year+Globrad_avehy+ Globrad_ave_month+
    month:clay_cat+month2:clay_cat+month3:clay_cat+(1|harvest_year),
  
  data = df_gen)

summary(linear_translate_ind)

fixed_effect_coeff <- cbind("coefficient"=fixef(linear_translate_ind),
                            "covariable"=names(fixef(linear_translate_ind))) |> 
  as.data.frame()

write.table(fixed_effect_coeff,"fixed_effect_coeff.txt", sep="\t")

#df_gen$lin_pred_simp <- exp(predict(linear_translate_simp, df_gen))



# GBM opt sim ind -----

param_gbm <-  expand.grid(
  interaction.depth = seq(10,16,3),#c(10,12,14),
  n.trees = seq(110000,220000,32000),
  shrinkage = c(0.1,0.0001),
  n.minobsinnode = seq(5,20,15)
)

#detectCores()
#Timedf = data.frame(time="")
#stime = data.frame(stime)


control <- trainControl(method ="repeatedcv", #"cv",
                        number = 5,
                        repeats = 2,
                        allowParallel = TRUE
                        #savePredictions = "all"
)

#remotes::install_github("gbm-developers/gbm")

#gbm_raw <- gbm(meancon ~ ., data =df_gen_simp_ind, distribution="tdist")


# gbm()

fitt_gbm_non_gamma_bis <- caret::train(
  log(meancon) ~ .,
  data =df_gen_simp_ind,
  method = "gbm",
  trControl = control,
  #verbose = FALSE,
  distribution = "gaussian",
  tuneGrid = param_gbm
  
)

plot(fitt_gbm_non_gamma_bis)

saveRDS(fitt_gbm_non_gamma,"fitt_gbm_none_gamma.RDS")

fitt_gbm_non_gamma <- readRDS("fitt_gbm_none_gamma.RDS")

fitt_gbm_non_gamma$results[rownames(fitt_gbm_non_gamma$bestTune),]$RMSE/mean(log(df_gen$meancon))*100

sqrt(mean((fitt_gbm_non_gamma$finalModel$fit-log(df_gen$meancon))^2))/mean(log(df_gen$meancon))* 100

plot((fitt_gbm_non_gamma$finalModel$shrinkage))

# xgb gradient ----

param_grid_xgb_max <- expand.grid(
  nrounds = c(500,750,1000),#seq(100, 1000, 300),          # Number of boosting rounds
  max_depth = c(5, 10),              # Maximum depth of trees
  eta = c(0.01, 0.1, 0.3),             # Learning rate
  gamma = c(0.1, 0.2, 0.3),                           # Regularization parameter
  colsample_bytree = c(0.5, 0.9), # Fraction of features to use in each tree
  subsample= c(0.5, 0.9),
  min_child_weight=c(5,20)
  )

fitt_xgb_max <- caret::train(
  log(meancon) ~ .,
  data = df_gen_simp_ind_max,
  preProc = c("center", "scale"),
  method = "xgbTree",
  trControl = trainControl(method="cv", number=5, returnResamp = "all"),
  tuneGrid = param_grid_xgb_max,
  verbose = FALSE
)

summary(fitt_xgb_max)

plot(fitt_xgb_max)

plot(varImp(fitt_xgb_max))

library(xgboost)

importance <-
  xgb.importance(model = fitt_xgb_max$finalModel) |> 
  as.data.frame() |> 
  mutate(Predictor=sub("^(Main_nles5).*", "\\1", Feature)) |>
  mutate(Predictor=sub("^(Winter_nles5).*", "\\1", Predictor)) |>
  mutate(Predictor=sub("^(clay_cat).*", "\\1", Predictor)) |> 
  group_by(Predictor) |> 
  summarise(Contribution=sum(Gain)*100) |> 
  arrange(-Contribution)

importance |> ggplot(aes(x=reorder(Predictor, Contribution), y=Contribution))+
  geom_col(width=0.3)+coord_flip()+theme_hc()+
  labs(x='Predictors',
       y="Contribution (%)")

sample_frac(df_gen_simp_ind_max, 0.1) |> 
  ggpairs(colors='clay_cat') +theme_clean()


saveRDS(fitt_xgb_max, "fitt_xgb_max.RDS")

sqrt(mean((predict(fitt_xgb_max,df_gen)-log(df_gen$meancon))^2))/mean(log(df_gen$meancon))* 100


library(pdp)

variables <-c("N_mineral_spring","Main_nles5","Winter_nles5",
              "AirTemp_ave_month","Precip_cumsumhy")

pdp_func <- function(i) {
  
  m_c_plot <- 
  partial(fitt_xgb_max, 
          pred.var = c(print(i),"clay_cat"), 
          train = droplevels(sample_frac(df_gen,0.2)),
          chull = TRUE)
  
}

list_xgb_sim_ind_max_partial <- lapply(variables, pdp_func)


pdp_Precip_cumsumhy<- 
  list_xgb_sim_ind_max_partial[[6]] |> 
  ggplot(aes(x=Precip_cumsumhy, 
             y=exp(yhat),
             col=clay_cat,
             fill=clay_cat))+
  #scale_x_continuous(breaks = seq(1,12,1))+
  #scale_colour_solarized('red')+
  scale_color_stata()+
  scale_fill_stata()+
  #geom_col(position = "dodge")+
  geom_line()+
  geom_smooth(alpha=0.1)+
  theme_hc()
           

ggarrange(pdp_AirTemp_ave_month,
          pdp_Precip_cumsumhy,
          pdp_N_mineral_spring, 
          pdp_month, 
         
          pdp_Main_nles5,
          pdp_Winter_nles5,
          
       ncol=2, nrow=3, #, 
          common.legend = T,  legend="bottom")



#Prediction and communicating results ----

df_gen_pred <- cbind(df_gen,
                     "pred_rf_full" = as.data.frame(exp(modellist$'2000'$finalModel$predicted))[,1],
                     "pred_rf_full_ind" =as.data.frame(exp(modellist_ind$'2000'$finalModel$predicted))[,1] ,
                     "pred_rf_sim" = as.data.frame(exp(modellist_sim$'2500'$finalModel$predicted))[,1],
                     "pred_rf_sim_ind_max" = as.data.frame(exp(modellist_sim_ind_max$'1000'$finalModel$predicted))[,1],
                     "pred_rf_sim_ind" = as.data.frame(exp(modellist_sim_ind$'1500'$finalModel$predicted))[,1],
                     
                     "pred_gbm_sim_ind"= exp(fitt_gbm_non_gamma$finalModel$fit),
                     "pred_xbt_sim_ind" = exp(predict(fitt_xgb,df_gen)),
                     "pred_xbt_sim_ind_max" = exp(predict(fitt_xgb_max,df_gen)),
                     
                     "pred_lin"=exp(predict(linear_translate, df_gen)),
                     "pred_lin_ind"=exp(predict(linear_translate_ind, df_gen)) 
) |> 
  mutate(
    leach_a_obs=meancon*afstro_sum_month/100,
         
    leach_rf_full=pred_rf_full*afstro_sum_month/100,
    leach_rf_full_ind=pred_rf_full_ind*afstro_sum_month/100,
    leach_rf_sim=pred_rf_sim*afstro_sum_month/100,
    leach_rf_sim_ind=pred_rf_sim_ind*afstro_sum_month/100,
    leach_rf_sim_max=pred_rf_sim_ind_max*afstro_sum_month/100,
         
    leach_gbm_sim_ind= pred_gbm_sim_ind*afstro_sum_month/100,
    leach_xbt_sim_ind =pred_xbt_sim_ind*afstro_sum_month/100,
    leach_xbt_sim_ind_max =pred_xbt_sim_ind_max*afstro_sum_month/100,
    
         
    leach_lin=pred_lin*afstro_sum_month/100,
    leach_lin_ind=pred_lin_ind*afstro_sum_month/100)

testRes  <-  df_gen_pred |> select(starts_with("leach_")) |> corrplot::cor.mtest(conf.level = 0.95)

df_gen_pred |> select(starts_with("leach_")) |> cor() |>
  corrplot::corrplot( method = 'circle', 
                      type = 'lower', addCoef.col ='White',
                      #insig='blank', 
                      tl.col = 'black',
                      #col=gray.colors(100),
                      order = 'AOE', diag = FALSE)

saveRDS(df_gen, "df_gen0610.RDS")

df_gen_pred |>
  mutate(clay_plot = fct_recode(
    clay_cat,
    low = "low",
    'midle-high' = "middle",
    'midle-high' = "high"
  )) |>
  filter(Winter_nles5 == c("1", "4")) |>
  mutate(Winter_nles5=dplyr::recode_factor(Winter_nles5,
                `1`="Winter cereal", 
                `4`="Cover crop" ) )|> 
  #filter(Main_nles5==1 & Winter_nles5 == 4 & harvest_year== c(1998,1999,2008))|>
  #select(month, rf_pred, meancon,jbnr,harvest_year,clay_cat) |>
  pivot_longer(
    cols = c(
      leach_a_obs,
      leach_xbt_sim_ind,
      leach_rf_full_ind,
      leach_lin_ind
    ),
    #starts_with("leach_"),
    values_to = "Nleaching",
    names_to = "Measurement"
  ) |>
  ggplot(aes(
    x = month,
    y = Nleaching,
    fill = Measurement,
    col = Measurement
  )) +
   geom_smooth(alpha = 0.4, se=FALSE #aes(linetype = Measurement)
               ) +
  geom_boxplot(alpha = .1, aes(group = interaction(Measurement, month), 
                               fill = Measurement)) +
  geom_point(alpha = 0.2, size = 0.1) +
  facet_grid(Winter_nles5 ~ clay_plot) +
  scale_x_continuous(breaks = seq(1, 12, 1)) +
  scale_y_continuous(limits = c(0, 30)) +
  
  guides(linetype = FALSE,
         fill = FALSE,
         col = guide_legend("Estimation:")) +
  ylab("N leaching (Kg N/ha)")+
  ggthemes::theme_hc()+
  scale_color_gdocs()+
  scale_fill_gdocs()+
  theme(legend.position = "bottom", panel.background = NULL) 
#theme_bw()



log_den<- df_gen_pred |>
  pivot_longer(
    cols = c(
      leach_a_obs,
      leach_xbt_sim_ind,
      leach_rf_full_ind,
      leach_lin_ind
    ),
    values_to = "Nleaching",
    names_to = "Measurement"
  ) |>
  ggplot(aes(
    x = log(Nleaching) ,
    fill = Measurement,
    col = Measurement
  )) +
  geom_density(stat="density", position="identity", alpha=0.1)+
  #geom_step(aes(y = ..y..), stat = "ecdf") +
  #scale_x_continuous(breaks = c(-2.5, 5.5)) +
  #scale_x_continuous(limits = c(0,75))+
  theme_hc() +
  theme(legend.position = "bottom", 
        panel.background = NULL#,
        #panel.border =NULL
        ) +
  guides(linetype = FALSE,
         fill = FALSE,
         col = guide_legend("Estimation:")) +
  #ylab("Percentile")+
  ylab("Frequency")+
  scale_color_gdocs()+
  scale_fill_gdocs()

ggarrange(log_ecdf, ecdf, log_den, den, ncol=2, nrow=2, #, 
          common.legend = T,  legend="bottom")


# Lookup table ----

#https://climateknowledgeportal.worldbank.org/country/denmark/climate-data-historical

cont <- 
  df_gen_simp_ind_max |> 
  select(!c(month, meancon)) |> 
  select_if(is.numeric) |>
  scale(center = FALSE) |> as.data.frame() 

cont |>  
  pivot_longer(
    cols =colnames(cont),
    values_to = "value",
    names_to = "variable"
  ) |>
  ggplot(aes(
    x = value ,
    fill = variable,
    col = variable
  )) +
  geom_step(aes(y = ..y..), stat = "ecdf") +
  #geom_density(stat="density", position="identity", alpha=0.1)+
  theme_hc() +
  theme(legend.position = "bottom", 
        panel.background = NULL#,
        #panel.border =NULL
  ) +
  guides(linetype = FALSE,
         fill = FALSE,
         col = guide_legend("variable")) +
  ylab("Percentile")+
  #ylab("Frequency")+
  scale_color_gdocs()+
  scale_fill_gdocs()
  
df_gen_simp_ind_max |>  
  ggplot(aes(x = N_mineral_spring )) +
  #geom_density(stat="density", position="identity", alpha=0.1)+
  geom_step(aes(y = ..y..), stat = "ecdf")+
  theme_hc()

quantile(df_gen_simp_ind_max$N_mineral_spring, seq(0.1,1,0.1))

quantile(df_gen_simp_ind_max$AirTemp_ave_month, seq(0.1,1,0.1))

quantile(df_gen_simp_ind_max$Precip_cumsumhy, seq(0.1,1,0.1))

fitt_xgb_max <- readRDS("fitt_xgb_max.RDS")

lookup_01_xbt <- expand.grid(month=seq(1,12,1),
                         Main_nles5=levels(df_gen_simp_ind_max$Main_nles5),
                         Winter_nles5=levels(df_gen_simp_ind_max$Winter_nles5),
                         clay_cat=levels(df_gen_simp_ind_max$clay_cat),
                         AirTemp_ave_month=seq(-1,25,1),
                         Precip_cumsumhy=c(seq(10,550,20), seq(600,1400,50)),
                         N_mineral_spring=seq(0,320,20)
                         )

lookup_01_xbt$pred <- predict(fitt_xgb_max,lookup_01_xbt)

lookup_01_xbt$pred <- exp(lookup_01_xbt$pred)

write.table(lookup_01_xbt,"lookup_01_xbt.txt", sep="\t")
             