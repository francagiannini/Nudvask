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

# data ----

df <- read.table("data_preproc/db_Ndaily_cut_0523.txt", sep ="\t", dec=".")

colnames(df) <- gsub(".x","",colnames(df)) 
summary(df) 

# Some explore -----

df_ggpair <- df|>  
  select(meankonc,AirTemp,Precip,#day_harv,#day_leach, 
         afstro_sum,
         Precip_sumhy, 
         #Detailed_data_coubling_jb#,
         Clay, 
         #JB,
         #site_eng.x
  ) #|> 
#mutate(Clay=as.factor(Clay))

ggpair_conc <- 
  ggpairs(df_ggpair,
          #mapping = ggplot2::aes(colour=as.factor(Detailed_data_coubling_jb), 
          #                       alpha=0.3),# aca podes poner tus clusters 
          upper = list(continuous = wrap("cor", size = 4)),
          lower = list(continuous = wrap(ggally_points, 
                                         alpha = 0.3, 
                                         size=0.1)
          )
  ) +
  ggplot2::theme(panel.background = NULL)


table(df$site_eng,df$harvest_year) |> 
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
  scale_y_discrete(name = "Crop")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

table(df$crop_main_name,df$harvest_year) |> 
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


table(df$crop_winter_name,df$harvest_year) |> 
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
#   mutate(meanconc=mean(meankonc))

df |> 
  ggplot(aes(y=meankonc, x=as.factor(month)#, group=site_eng.x
  )
  )+
  geom_boxplot() +
  facet_wrap(~crop_main_name, nrow = 4, scales = "free_y")+
  #scale_x_continuous(breaks = seq(1,12,1))+
  #geom_smooth()
  theme_bw()

# daily approach  ----


# feature selection ----

df_gen <-   df |>  
  select(meankonc, GlobRad,AirTemp,Precip,tvspp, 
         harvest_year,Detailed_data_coubling_jb,
         Gamma, Mfu,Vfu,Main_crop_nles5,Vinter_crop_nles5,
         N_mineral_autuomn,N_mineral_spring,N_f_year,N_min_year.1,N_f_year.2,
         N_topsoil,
         day_harv,#day_leach,
         Precip_lag1,Precip_lag14,Precip_lag30,Precip_lag60,
         Precip_lag90,Precip_sum14,Precip_sum28,Precip_sum90,
         Precip_sum180,Precip_sum365,Precip_ave3,Precip_ave14,Precip_ave28,
         GlobRad_lag7,GlobRad_lag14,GlobRad_lag28,GlobRad_sum7,GlobRad_sum14,
         GlobRad_sum28,GlobRad_ave7,GlobRad_ave14,GlobRad_ave28,
         AirTemp_lag14,AirTemp_lag30,AirTemp_lag7,AirTemp_ave3,AirTemp_sum3,
         AirTemp_ave7,AirTemp_sum7,AirTemp_ave14,AirTemp_sum14,AirTemp_ave28,
         AirTemp_sum28,tvspp_14d,tvspp_28d,tvspp_60, Clay, 
         crop_main_name,crop_winter_name) |> 
  filter(!is.na(Precip_ave3))|>
  filter(!meankonc == 0) |> 
  drop_na()

# Boruta gen ----

#Takes some time, so be patient

bor_gen <- Boruta(meankonc~.,data=df_gen,doTrace=2,xlab = "")
print(bor_gen)
plot(bor_gen,lwd = 0.1, las = 3, xlab = "")


stats<-attStats(bor_gen)
#Boruta 3
print(stats)

plot(normHits~meanImp,col=stats$decision,data=stats)


# ? 05_06 crop imbalance ? ----

table(df_daily$crop_winter_name,df_daily$crop_main_name) |> 
  as.data.frame() |>
  mutate(Freq=ifelse(Freq==0,NA, Freq)) |> 
  # mutate_all(~ na_if( .,0)) |> 
  ggplot( aes(x = Var2, y = Var1, fill = Freq)) +
  geom_tile(color = "gray") +
  scale_fill_gradientn(name = "n",
                       na.value = 'black',
                       colors = brewer.pal(5,"Blues")) +
  geom_text(aes(label = paste(Freq)), color = "black", size = 2) +
  scale_x_discrete(name = "Main crop") +
  scale_y_discrete(name = "Winter crop")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

bor_gen$finalDecision

# some data missing  becausse wea is missing 

df_daily <- df |>  
select(meankonc, #GlobRad,AirTemp,Precip,tvspp, 
day_harv,#day_leach,
harvest_year,
Precip_lag1,Precip_lag14,Precip_lag30,Precip_lag60,
Precip_lag90,Precip_sum14,Precip_sum28,Precip_sum90,
Precip_sum180,Precip_sum365,Precip_ave3,Precip_ave14,Precip_ave28,
GlobRad_lag7,GlobRad_lag14,GlobRad_lag28,GlobRad_sum7,GlobRad_sum14,
GlobRad_sum28,GlobRad_ave7,GlobRad_ave14,GlobRad_ave28,
AirTemp_lag14,AirTemp_lag30,AirTemp_lag7,AirTemp_ave3,AirTemp_sum3,
AirTemp_ave7,AirTemp_sum7,AirTemp_ave14,AirTemp_sum14,AirTemp_ave28,
AirTemp_sum28,tvspp_14d,tvspp_28d,tvspp_60, Clay, 
crop_main_name,crop_winter_name) |> 
filter(!is.na(Precip_ave3))|>
filter(!meankonc == 0)
#mutate(meankonc = ifelse(meankonc == 0, 0.01, meankonc))

#predictor selection 
# linear ----

hist(log(df_daily$meankonc))

lm1 <- lm(formula=log(meankonc) ~., df_daily)
summary(lm1)
anova(lm1)
plot(lm1$fitted.values,log(df_daily$meankonc))

plot

best_sub <-
  regsubsets(log(meankonc) ~.,
             data =df_daily,
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
  interaction.depth = seq(2,12,2),#c(10,12,14),
  n.trees = seq(20000,100000,10000),
  shrinkage = c(0.01,0.001),
  n.minobsinnode = c(10,15,30)
)

#detectCores()
#Timedf = data.frame(time="")
#stime = data.frame(stime)

Mycluster = makeCluster(detectCores()-2)
registerDoParallel(Mycluster)

control <- trainControl(method = "cv",#"repeatedcv",
                        number = 5,
                        #repeats = 3
                        allowParallel = TRUE
                        )

#remotes::install_github("gbm-developers/gbm")

#gbm()

fitt_gbm <- caret::train(
  log(meankonc) ~.,
  data =df_daily,
  method = "gbm",
  trControl = trainControl(method="cv", number=5, returnResamp = "all"),
    #control,
  verbose = FALSE#,
  #metric = "RMSE",
  #tuneGrid = param_gbm
)

plot(fitt_gbm)

saveRDS("fitt_gbm.RDS")

fitt_gbm$results[rownames(fitt_gbm$bestTune),]$RMSE/mean(log(df_daily$meankonc))*100

sqrt(mean((fitt_gbm$finalModel$fit-log(df_daily$meankonc))^2))/mean(log(df_daily$meankonc))* 100

varImp(fitt_gbm)

# cart ----
y <- log(df_daily[complete.cases(df_daily),"meankonc"]) 
x <- df_daily[complete.cases(df_daily),-1]

# df_daily_cart = as.matrix(df_daily_cart)
# mode(df_daily_cart) = "numeric"


fitt_cart <- caret::train(
  x,y,
  #data =df_daily,
  #preProc = c("center", "scale"),
  method = "rpart2",
  tuneLength = 20, 
  trControl = trainControl(method = "cv")
)

plot(fitt_cart)
summary(fitt_cart$finalModel)

rpartTree <- rpart(y ~ ., data = x, maxdepth = 8)

rpartTree2 <- as.party(rpartTree)
plot(rpartTree2)


sqrt(mean((predict(fitt_cart,df_daily)-log(df_daily$meankonc))^2))/mean(log(df_daily$meankonc))* 100


saveRDS("fitt_cart.RDS")

# xgb gradient ----

fitt_xgb <- caret::train(
  log(meankonc) ~ .,
  data =df_daily,
  preProc = c("center", "scale"),
  method = "xgbTree",
  trControl = trainControl(method="cv", number=5, returnResamp = "all"),
  verbose = FALSE
)
fitt_xgb$finalModel
plot(fitt_xgb)
varImp(fitt_xgb)

saveRDS("fitt_cart.RDS")

sqrt(mean((predict(fitt_xgb,df_daily)-log(df_daily$meankonc))^2))/mean(log(df_daily$meankonc))* 100


# random forest ----

fitt_rf <- caret::train(
  log(meankonc) ~.,
  data =df_daily,
  #preProc = c("center", "scale"),
  method = "rf",
  trControl = trainControl(method = "cv",number = 10,allowParallel = T),
  verbose = FALSE
)

plot(varImp(fitt_rf), cuts=50)

fitt_rf$results[rownames(fitt_rf$bestTune),]$RMSE/mean(log(df_daily$meankonc))* 100

sqrt(mean((fitt_rf$finalModel$predicted-log(df_daily$meankonc))^2))/mean(log(df_daily$meankonc))* 100

plot(fitt_rf$finalModel)

as.data.frame(fitt_rf$finalModel$importance) |> 
  mutate("variable"=row.names(fitt_rf$finalModel$importance)) |> 
  arrange(desc(IncNodePurity))

partial(fitt_rf, pred.data =df_daily, 
        pred.var = c("Clay", 
                     #"Precip_sum180"
                     #,"tvspp_60"
                     ), 
        plot = TRUE)

saveRDS("fitt_rf.RDS")

# selected ----

df_daily <- df_daily |> mutate(day_harv2=day_harv*day_harv)


meankonc_REML<-lmer(log(meankonc)~1+ Clay + crop_main_name+ tvspp_60+
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
                   ,data=df_daily)

summary(meankonc_REML)

anova(meankonc_REML) 

#pred_meankonc_REML <- predict(meankonc_REML, df_daily)
  
sqrt(mean((predict(meankonc_REML, df_daily)-log(df_daily$meankonc))^2))/mean(log(df_daily$meankonc))* 100


# gams ----



gamm <- mgcv::gam(log(meankonc) ~ 1 + s(Clay,bs='ps') + crop_main_name+
                    crop_winter_name+
                    s(day_harv, bs='ps')+
                    s(Precip_sum365, bs='ps')+
                    s(tvspp_60, bs='ps')#+harvest_year
                    #s(harvest_year, bs = 're')
                    #day_harv2+
                    #Precip_sum365+(1|harvest_year)+
                    #crop_main_name:day_harv + crop_winter_name:day_harv#+
                    #crop_main_name:day_harv2 + crop_winter_name:day_harv2
                    #s(x1, bs='ps', sp=0.6) + s(x2, bs='ps', sp=0.6) + x3, 
                  ,data = df_daily
                  ,method="REML"#, 
                  #select=TRUE
                  )

gamm2 <-mgcv::gam(log(meankonc)~ 1+
                     s(Clay) + 
                     s(day_harv)+ 
                     s(Precip_sum365)+
                     s(tvspp_60)+
                     crop_main_name + 
                     crop_winter_name +
                     s(day_harv, by = crop_winter_name) + 
                     s(day_harv, by = crop_main_name)#+
                     #(1 | harvest_year)
                   ,data = df_daily$c
                   , method="REML")


summary(gamm)
plot(gamm)

sqrt(mean((gamm$fitted.values-log(df_daily$meankonc))^2))/mean(log(df_daily$meankonc))* 100

