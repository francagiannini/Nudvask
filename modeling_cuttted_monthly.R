# packages ----
library(tidyverse)
library(ggpubr)
library(GGally)
library(RColorBrewer)
library(leaps)
library(caret)
library(gbm)
library(parallel)
library(doParallel)
library(rpart)
library(mlbench)
library(Boruta)
library(nlme)
library(mgcv)
#install.packages("mice")
library(mice)
library(tmap)

# data ----

df_m <- read.table("data_preproc/db_Nmonthly_cut_3107.txt",
                   sep ="\t", dec=".")|>
  mutate(prev_Main_nles5=as.factor(prev_Main_nles5),
         prev_Winter_nles5=as.factor(prev_Winter_nles5) ,
         Main_nles5=as.factor(Main_nles5) ,
         Winter_nles5=as.factor(Winter_nles5),
         jbnr=as.factor(jbnr),
         Gamma=as.factor(Gamma),
         WC=as.factor(WC)
  )

summary(df_m)
md.pattern(df_m)

imputation_list <- mice(df_m,
                        method = "rf",
                        m=5)  # "pmm" == predictive mean matching (numeric data)


#plot(df_m$Clay)

df_m_imp <- complete(imputation_list) |>
  mutate(clay_cat=cut(clay,c(-Inf,6,10, Inf),
                      labels=c("low","middle","high")))

saveRDS(df_m_imp, "df_m_imp.RDS")

df_m_imp <- readRDS("df_m_imp.RDS")
  
summary(df_m_imp)

#saveRDS(df_m_imp, "df_m_imp.RDS")

# Some explore -----

df_ggpair <- df_m_imp|>  
  select(
    month,
    meancon,
    clay_cat, 
    afstro_sum_month,
    Precip_sum_month,
    AirTemp_ave_month,
    Globrad_ave_month,
    tvspp_month,
    drain_days,
    # Precip_sum60,
    # Precip_sum90,
    # Precip_sum180,
    # Precip_sum365,
    # #AirTemp_ave60,
    # AirTemp_ave90,
    # #AirTemp_ave180,
    # #tvspp_60,
    # tvspp_90,
    #afstro_sumhy,
    #Precip_sumhy,
    #AirTemp_avehy,
    #Globrad_avehy,
    N_mineral_spring,
    #N_mineral_autuomn,
    N_org_year,
    N_topsoil
  ) #|> 
#mutate(Clay=as.factor(Clay))

ggpair_conc <- 
  ggpairs(df_ggpair,
          mapping = ggplot2::aes(colour=as.factor(clay_cat), 
                                 alpha=0.3),# aca podes poner tus clusters 
          upper = list(continuous = wrap("cor", size = 4)),
          lower = list(continuous = wrap(ggally_points, 
                                         alpha = 0.3, 
                                         size=0.1)
          )
  ) +
  ggplot2::theme(panel.background = NULL)
ggpair_conc

table(df_m_imp$site_eng,df_m_imp$harvest_year) |> 
  as.data.frame() |>
  mutate(Freq=ifelse(Freq==0,NA, Freq)) |> 
  # mutate_all(~ na_if( .,0)) |> 
  ggplot( aes(x = Var2, y = Var1, fill = Freq)) +
  geom_tile(color = "gray") +
  scale_fill_gradientn(name = "n",
                       na.value = 'black',
                       colors = brewer.pal(5,"Purples")) +
  geom_text(aes(label = paste(Freq)), color = "black", size = 2) +
  scale_x_discrete(name = "harvest year") +
  scale_y_discrete(name = "Site")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

table(df_m_imp$crop_main_name,df_m_imp$harvest_year) |> 
  as.data.frame() |>
  mutate(Freq=ifelse(Freq==0,NA, Freq)) |> 
  # mutate_all(~ na_if( .,0)) |> 
  ggplot( aes(x = Var2, y = Var1, fill = Freq)) +
  geom_tile(color = "gray") +
  scale_fill_gradientn(name = "n",
                       na.value = 'black',
                       colors = brewer.pal(5,"Greens")) +
  geom_text(aes(label = paste(Freq)), color = "black", size = 2) +
  scale_x_discrete(name = "harvest year") +
  scale_y_discrete(name = "Crop")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))


table(df_m_imp$crop_winter_name,df_m_imp$harvest_year) |> 
  as.data.frame() |>
  mutate(Freq=ifelse(Freq==0,NA, Freq)) |> 
  # mutate_all(~ na_if( .,0)) |> 
  ggplot( aes(x = Var2, y = Var1, fill = Freq)) +
  geom_tile(color = "gray") +
  scale_fill_gradientn(name = "n",
                       na.value = 'black',
                       colors = brewer.pal(5,"Blues")) +
  geom_text(aes(label = paste(Freq)), color = "black", size = 2) +
  scale_x_discrete(name = "harvest year") +
  scale_y_discrete(name = "Crop")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))


# db_f_m <- db_f |> group_by(harvest_year.x, ident, month) |> 
#   mutate(meanconc=mean(meancon))

df_m_imp |> 
  ggplot(aes(y=meancon, x=as.factor(month)#, group=site_eng.x
  )
  )+
  geom_boxplot() +
  facet_wrap(~Main_nles5, nrow = 4, scales = "free_y")+
  #scale_x_continuous(breaks = seq(1,12,1))+
  #geom_smooth()
  theme_bw()

fil_df_m_imp <- df_m_imp |> dplyr::filter(ident==2702)

# monthly approach  ----

# feature selection ----

df_gen <-df_m_imp |>
  select(
    #"merge_mon",
    #"dent",
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
    "N_from_grassing_animals",
    "N_topsoil",
    "clay",
    "Gamma",
    "jbnr",
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
    "X",
    "Y",
    #"geometry",
    #"Id",
    #"afstro_sum_month" ,
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
    #"afstro_sumhy",
    "Precip_sumhy",
    "AirTemp_avehy",
    "Globrad_avehy",  
    "weigth_astro",
    #"season", 
    "clay_cat") |> 
  #filter(!is.na(crop_main_name))|>
  filter(!meancon == 0) |>
  mutate(season=case_when(month %in% 3:5 ~ 'spring',
                          month %in% 6:8 ~ 'summer',
                          month %in% 9:11 ~ 'autumn',
                          TRUE ~ 'winter')) |> 
  drop_na()

crop_names_codes <- readxl::read_excel("data_raw/nles5_crop__code_param.xlsx",
                                       sheet = "nwe_coding_crops") |>   
  select(numeric_codification, crop_name)

# df_gen <- conc_raw |>
#   mutate(
#     'M' =recode(crop_Main,!!!crop_names_codes$crop_name),
#     'V'=recode(crop_Winter,!!!crop_names_codes$crop_name))

# Boruta gen ----

#Takes some time, so be patient

bor_gen <-
  Boruta(
    log(meancon) ~ . - meancon,
    data = df_gen,
    doTrace = 2,
    xlab = "",
    pValue = 0.01
  )

print(bor_gen)
par(mar= c(10, 4, 4, 2) + 0.5)
plot(bor_gen,lwd = 0.1, las = 2, xlab = "")

stats<-attStats(bor_gen)
plot(TentativeRoughFix(bor_gen),lwd = 0.1, las = 2)


# ? 05_06 crop imbalance ? ----

table(df_gen$Winter_nles5,df_gen$Main_nles5) |> 
  as.data.frame() |>
  mutate(Freq=ifelse(Freq==0,NA, Freq)) |> 
  # mutate_all(~ na_if( .,0)) |> 
  ggplot( aes(x = Var2, y = Var1, fill = Freq)) +
  geom_tile(color = "gray") +
  scale_fill_gradientn(name = "n",
                       na.value = 'black',
                       colors = brewer.pal(5,"Oranges")) +
  geom_text(aes(label = paste(Freq)), color = "black", size = 2) +
  scale_x_discrete(name = "Main crop") +
  scale_y_discrete(name = "Winter crop")+theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))


#predictor selection 
# linear ----

df_gen |>
  mutate(log=log(meancon)) |> 
  ggplot(aes(log)) +
  geom_histogram(aes(y = ..density..), fill = "#BF8200", color = "white", bins = 30) +
  #geom_histogram(aes(log))+
  #geom_density(aes(y = rel_freq), color = "red", linetype = "dashed") +
  scale_y_continuous(name = "Relative Frequency / Density") +
  scale_x_continuous(name= "log(Nitrate conccentration (mg/L))")+
  #labs(title = "Histogram with Relative Frequency and Density Curve") +
  theme_minimal()


hist(hist(df_gen$meancon))
hist(log(df_gen$meancon))

lm1 <- lm(formula=log(meancon) ~., df_gen)
summary(lm1)
anova(lm1)
plot(exp(lm1$fitted.values),df_gen$meancon)
plot((lm1$fitted.values),log(df_gen$meancon))

best_sub <-
  regsubsets(log(meancon) ~.,
             data =df_gen,
             really.big=T, #must specify really.big=T
             nbest = 1,      # 1 best model for each number of predictors
             nvmax = NULL,    # NULL for no limit on number of variables
             force.in = NULL, 
             force.out = NULL,
             method = "backward" #"exhaustive"
  )


summary_best_subset <- summary(best_sub)
as.data.frame(summary_best_subset$outmat)

which.max(summary_best_subset$adjr2)
summary_best_subset$which[which.max(summary_best_subset$adjr2),]

# GBM -----

param_gbm <-  expand.grid(
  interaction.depth = 15,#seq(10,20,10),#c(10,12,14),
  n.trees = seq(5000,15000,5000),
  shrinkage = 0.01,#c(0.01,0.001),
  n.minobsinnode = seq(5,50,5)
)

#detectCores()
#Timedf = data.frame(time="")
#stime = data.frame(stime)

Mycluster = makeCluster(detectCores()-2)
registerDoParallel(Mycluster)

control <- trainControl(method = "cv",#"repeatedcv",
                        number = 2,
                        #repeats = 3
                        allowParallel = TRUE
)

remotes::install_github("gbm-developers/gbm")

# gbm()

fitt_gbm <- caret::train(
  log(meancon) ~ .,
  data =df_gen,
  method = "gbm",
  preProc = c("center", "scale"),
  #trControl = #trainControl(method="cv", number=10, allowParallel = T),
  #control,
  verbose = FALSE,
  metric = "RMSE"#,
  #tuneGrid = param_gbm
)

plot(fitt_gbm)

saveRDS("fitt_gbm.RDS")
fit_gbm <- readRDS("fitt_gbm.RDS")

fitt_gbm$results[rownames(fitt_gbm$bestTune),]$RMSE/mean(log(df_gen$meancon))*100

sqrt(mean((fitt_gbm$finalModel$fit-log(df_gen$meancon))^2))/mean(log(df_gen$meancon))* 100

plot(varImp(fitt_gbm))

# cart ----
y <- log(
  df_gen[complete.cases(df_gen),"meancon"]
  ) 
x <- df_gen[complete.cases(df_gen),] |> select(!meancon)

# df_gen_cart = as.matrix(df_gen_cart)
# mode(df_gen_cart) = "numeric"


fitt_cart <- caret::train(
  x,y,
  #data =df_gen,
  #preProc = c("center", "scale"),
  method = "rpart2",
  tuneLength = 10,
  minbucket =20,
  trControl = trainControl(method = "cv")
)

plot(varImp(fitt_cart))

plot(fitt_cart)
summary(fitt_cart$finalModel)

library(rpart)
library(partykit)

rpartTree <- rpart(y ~ ., data = x, maxdepth = 8)

rpartTree2 <- as.party(rpartTree)

plot(rpartTree2)

sqrt(mean((predict(fitt_cart,df_gen)-log(df_gen$meancon))^2))/mean(log(df_gen$meancon))* 100

plot((predict(fitt_cart,df_gen)),log(df_gen$meancon))

saveRDS("fitt_cart.RDS")

# xgb gradient ----

fitt_xgb <- caret::train(
  log(meancon) ~ .,
  data =df_gen,
  preProc = c("center", "scale"),
  method = "xgbTree",
  trControl = trainControl(method="cv", number=5, returnResamp = "all"),
  verbose = FALSE
)

fitt_xgb$finalModel
plot(fitt_xgb)
varImp(fitt_xgb)

plot(varImp(fitt_xgb))


saveRDS(fitt_xgb, "fitt_xgb.RDS")

sqrt(mean((predict(fitt_xgb,df_gen)-log(df_gen$meancon))^2))/mean(log(df_gen$meancon))* 100


# random forest ----

fitt_rf <- caret::train(
  log(meancon) ~ .,
  data = df_gen,
  preProc = c("center", "scale"),
  method = "rf",
  trControl = trainControl(method = "cv",number = 5 
                           #, allowParallel = T
                           ),
  verbose = FALSE)

saveRDS("fitt_rf.RDS")

rf_pred <- cbind(df_gen,
                 "rf_pred"=as.data.frame(exp(fitt_rf$finalModel$predicted))[,1]#,
                 #as.data.frame(exp(fitt_gbm$finalModel$fit))[,1]
                 )

rf_pred |> 
  filter(Main_nles5==1 & Winter_nles5 == 4 & harvest_year== c(1998,1999,2008))|> 
  select(month, rf_pred, meancon,jbnr,harvest_year,clay_cat) |> 
  pivot_longer(cols=c(rf_pred,meancon), 
               values_to = "Concentration", names_to = "Measurement") |> 
  ggplot(aes(month,Concentration,col=Measurement))+
  scale_color_manual(values=c("#999999", "#E69F00"))+
  geom_smooth(alpha=0.2, aes(linetype=Measurement))+
  geom_point()+
  facet_grid(clay_cat~harvest_year)+
  theme_bw()+
  scale_y_continuous(limits = c(-5,30), breaks = seq(0,30,10))+
  
  scale_x_continuous(breaks = seq(1,12,1))+
  theme(legend.position = "bottom")

rf_pred |> 
  filter(!Winter_nles5==7) |> 
  #filter(Main_nles5==1 & Winter_nles5 == 4 & harvest_year== c(1998,1999,2008))|> 
  #select(month, rf_pred, meancon,jbnr,harvest_year,clay_cat) |> 
  pivot_longer(cols=c(rf_pred,meancon), 
               values_to = "Concentration", names_to = "Measurement") |> 
  ggplot(aes(x=month,y=Concentration,fill=Measurement, col=Measurement))+
  geom_boxplot(alpha=.1, aes(group=interaction(Measurement,month), fill=Measurement
                             ) )+
  scale_color_manual(values=c("#999999", "#E69F00"))+
  scale_fill_manual(values=c("#999999", "#E69F00"))+
  geom_smooth(alpha=0.3, aes(linetype=Measurement))+
  geom_point(alpha=0.2, size=0.1)+
  facet_grid(clay_cat~Winter_nles5)+
  theme_bw()+
  scale_y_continuous(limits = c(-5,30), breaks = seq(0,30,10))+
  
  scale_x_continuous(breaks = seq(1,12,1))+
  theme(legend.position = "bottom")



rf_pred |> group_by(season) |> 
  summarize(RMSE=sqrt(mean((rf_pred-meancon)^2))/mean(meancon))


plot(varImp(fitt_rf), cuts=50)

fitt_rf$results[rownames(fitt_rf$bestTune),]$RMSE/mean(log(df_gen$meancon))* 100

sqrt(mean((fitt_rf$finalModel$predicted-log(df_gen$meancon))^2))/mean(log(df_gen$meancon))* 100

plot(fitt_rf$finalModel)

as.data.frame(fitt_rf$finalModel$importance) |> 
  mutate("variable"=row.names(fitt_rf$finalModel$importance)) |> 
  arrange(desc(IncNodePurity))

rf_mod <- randomForest::randomForest(fitt_rf$finalModel$param, data=df_gen)

library(randomForest)

partialPlot(rf_mod, pred.data =df_gen, 
        pred.var = c("N_mineral_spring"#, 
                     #"Precip_sum180"
                     #,"tvspp_60"
        ), 
        plot = TRUE)



# selected ----

df_gen <- df_gen |> mutate(day_harv2=day_harv*day_harv)


meancon_REML<-lmer(log(meancon)~1+ Clay + crop_main_name+ tvspp_60+
                      crop_winter_name+
                      day_harv+day_harv2+
                      Precip_sum365+(1|harvest_year)+
                      crop_main_name:day_harv + crop_winter_name:day_harv+
                      crop_main_name:day_harv2 + crop_winter_name:day_harv2
                    #,random=list(harvest_year=pdIdent(~1))
                    #,method="REML"
                    #,control=lmeControl(niterEM=150
                    #,msMaxIter=200)
                    ,na.action=na.omit
                    ,data=df_gen)

summary(meancon_REML)

anova(meancon_REML) 

#pred_meancon_REML <- predict(meancon_REML, df_gen)

sqrt(mean((predict(meancon_REML, df_gen)-log(df_gen$meancon))^2))/mean(log(df_gen$meancon))* 100


# gams ----
gam_sel <-gam(log(meancon) ~ ., data = df_gen)



gamm <- mgcv::gam(log(meancon) ~ 1 + clay_cat + Main_nles5+
                    Winter_nles5+
                    s(month, bs='ps')+
                    s(Precip_sum365, bs='ps')+
                    s(tvspp_60, bs='ps')+#harvest_year
                  s(harvest_year, bs = 're')+
                  s(N_mineral_spring, bs='ps')+
                  #day_harv2+
                  #Precip_sum365+(1|harvest_year)+
                  Winter_nles5:clay_cat + #crop_winter_name:day_harv#+
                  #crop_main_name:day_harv2 + crop_winter_name:day_harv2
                  s(month, by = Winter_nles5)
                  #s(month, bs='ps', sp=0.6) + s(x2, bs='ps', sp=0.6) + x3, 
                  ,data = df_gen
                  #,method="REML", 
                  #select=TRUE
)

gamm2 <-mgcv::gam(log(meancon)~ 1+
                    s(Clay) + 
                    s(day_harv)+ 
                    s(Precip_sum365)+
                    s(tvspp_60)+
                    crop_main_name + 
                    crop_winter_name +
                    s(day_harv, by = crop_winter_name) + 
                    s(day_harv, by = crop_main_name)#+
                  #(1 | harvest_year)
                  ,data = df_gen$c
                  , method="REML")


summary(gamm)
plot(gamm)

sqrt(mean((gamm$fitted.values-log(df_gen$meancon))^2))/mean(log(df_gen$meancon))* 100

