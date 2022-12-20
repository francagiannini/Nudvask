## packages ----

library(tidyverse)
library(lubridate)
library(readxl)
library(utils)
library(sf)
library(tmap)
library(tidyverse)
getwd()

c_mess_master <- read.table("c_mess_master.txt", sep="\t",header = TRUE)

# c_mess_master <- c_mess_master |>
#   mutate(leach_year = ifelse(month<8,
#                              paste(year-1),
#                              paste(year)
#   )) |>
#   group_by(sted,leach_year) |>
#   arrange(date) |>
#   mutate(afstro_sum=cumsum(afstroemning))


baba <- c_mess_master |> filter(sted==1006) |> 
  select(sumdrain,afstro_sum,date,afstroemning,day_leach)

c_mess_master |> sample_frac(0.2) |> 
  ggplot(aes(y=afstro_sum,x=month))+
  geom_point()+
  theme_bw()


#### first explore -----
c_mess_measure <- c_mess_master |> 
  filter(measure_grp==TRUE) 

#c_mess_site |> 
c_mess_measure|> 
  #dplyr::filter(site_eng =="\"Flakkebjerg\"") |> 
  ggplot(aes(x = month, y = newconc)) +
  geom_point() +
  geom_smooth(method = "loess", se = F) +
  #coord_cartesian(ylim = c(0,0.9)) +
  theme(panel.grid = element_blank()) +
  facet_wrap(~site_eng, nrow=4,scales = "free_y")+
  #scale_x_continuous(breaks = seq(1,12,1))+
  theme_bw()

jul_day_plot <- c_mess_measure |> 
  #dplyr::filter(site_eng =="\"Flakkebjerg\"") |> 
  ggplot(aes(x = lubridate::yday(date), y = newconc)) +
  geom_point() +
  geom_smooth(method = "loess", se = F) +
  theme(panel.grid = element_blank()) +
  #facet_wrap(~site_eng, nrow=4,scales = "free_y")+
  scale_x_continuous(breaks = seq(1,366,11))+
  labs(y="Concenration", x="Julian day")+
  theme_bw()

harv_day_plot <- c_mess_measure |> 
  #dplyr::filter(site_eng =="\"Flakkebjerg\"") |> 
  ggplot(aes(x = day_harv, y = newconc)) +
  geom_point() +
  geom_smooth(method = "loess", se = F) +
  #coord_cartesian(ylim = c(0,0.9)) +
  theme(panel.grid = element_blank()) +
  #facet_wrap(~site_eng, nrow=4,scales = "free_y")+
  scale_x_continuous(breaks = seq(1,366,11))+
  labs(y="Concenration", x="Leaching day from april")+
  theme_bw()

leach_day_plot <- c_mess_measure |> 
  #dplyr::filter(site_eng =="\"Foulum\"") |> 
  ggplot(aes(x = day_leach, y = newconc)) +
  geom_point() +
  geom_smooth(method = "loess", se = F) +
  #coord_cartesian(ylim = c(0,0.9)) +
  theme(panel.grid = element_blank()) +
  facet_wrap(~year, nrow=4,scales = "free_y")+
  scale_x_continuous(breaks = seq(1,366,11))+
  labs(y="Concenration", x="Leaching day from aug")+
  theme_bw()


c_mess_measure |> ggplot()+
  geom_histogram(aes(x=newconc#log(newconc)
                     ,position="identity"), 
                 colour = "black"#,bins=20
  )+
  # geom_histogram(aes(x=newconc,position="identity"), 
  #                colour = "white"#,bins=20
  # )+
  #stat_density(geom = "line", aes(colour = "bla"))+
  ylab("Count")+ xlab("daily concentration" )+
  geom_rug() +
  theme_bw()


c_mess_measure |> ggplot(aes(x = year, y = sted))+
  geom_count(col="tomato3", show.legend=F)+
  scale_x_continuous(breaks=seq(1989,2018,1))+
  #scale_y_continuous(breaks=unique(c_mess_measure$sted))+
  theme_bw()

prob_sted <- c_mess_measure |> filter(sted>2000 & year<1995)

c_mess_measure |> 
  #dplyr::filter(sted =="1092") |> 
  ggplot(aes(x = afstro_sum, y =newconc )) +#afstro_sum
  geom_point() +
  geom_smooth(method = "loess", se = F) +
  #coord_cartesian(ylim = c(0,0.9)) +
  theme(panel.grid = element_blank()) +
  #geom_vline(aes(xintercept = 110, 
  #               size = 2, colour = "red", alpha=0.6))+
  facet_wrap(~site_eng, nrow=4,scales = "free_x")+
  labs(y="Concenration", x="Cummulitive drain")+
  theme_bw()


saveRDS(c_mess_measure,"c_mess_measure.RDS")


train_dat <- c_mess_measure |>
  select(newconc,
         day_leach,
         M,
         site_eng,
         W,
         sted,
         WC,
         WP,
         clay,
         JB,
         N_topsoil,
         afstro_sum,
         afstroemning,
         harvest_year.x,
         year) |>
  drop_na() |>
  mutate(newconc = ifelse(newconc == 0, 0.0001, newconc))


library(lme4)

VCA<-lme4::lmer(
  log(newconc)~1+(1|harvest_year.x)+(1|sted)+(1|afstro_sum)+(1|day_leach)
  ,na.action=na.omit
  ,REML=T
  #,control=lmerControl(optimizer="bobyqa")
  #,data=c_mess_measure)
  ,data=train_dat)

#summary(VCA_gral)

library(nlme)

vca <- as.data.frame(VarCorr(VCA))

vca |> group_by(grp) |> summarise(
  varprop = vcov / sum(vca$vcov) * 100) |> arrange(
    varprop, grp) |> ggplot(aes(x=grp,y=varprop, fill=varprop))+
  geom_col()+
  geom_text(aes(label=round(varprop,digits = 2)), 
            vjust=1.6, color="white", size=3.5)+
  theme_bw()

library(caret)
library(gbm)


param_gbm <-  expand.grid(
  interaction.depth = seq(4,12,4),#c(10,12,14),
  n.trees = seq(20000,100000,10000),
  shrinkage = c(0.01,0.001),
  n.minobsinnode = c(10,5,3)
)

control <- trainControl(method = "repeatedcv",
                        number = 2,
                        repeats = 3)

#remotes::install_github("gbm-developers/gbm")

#gbm()

fitt_gbm <- train(
  log(newconc)~1+day_leach+sted+afstro_sum+afstroemning+harvest_year.x,#+as.factor(M)+
    #site_eng+as.factor(W)+
    #as.factor(WC)+
    #as.factor(WP)+
    #clay+
    #as.factor(JB)+
    #N_topsoil,
  #distribution = "gamma",
  data = train_dat,
  method = "gbm",
  trControl = control,
  verbose = FALSE,
  #metric = "RMSE",
  tuneGrid = param_gbm
)


fitt_gbm$results[rownames(fitt_gbm$bestTune),]$RMSE/mean(log(train_dat$newconc))* 100

sqrt(mean((fitt_gbm$finalModel$fit-log(train_dat$newconc))^2))/mean(log(train_dat$newconc))* 100


fit_gbm_afstro <- train(
  log(newconc)~1+day_leach+
    afstroemning+
    as.factor(M)+
    site_eng+as.factor(W)+
    as.factor(WC)+
    as.factor(WP)+
    clay+
    as.factor(JB)+
    N_topsoil+
    afstro_sum,
  #distribution = "gamma",
  data = train_dat,
  method = "gbm",
  trControl = control,
  verbose = FALSE,
  #metric = "RMSE",
  tuneGrid = param_gbm
)

saveRDS("fitt_gbm.RDS")

plot(fitt_gbm)

summary(fit_gbm_afstro)

fit_gbm_afstro$bestTune

plot(train_dat$newconc,exp(fit_gbm_afstro$finalModel$fit))
abline(0,1)

fitt_gbm$results[rownames(fit_gbm_afstro$bestTune),]

fit_gbm_afstro$results[rownames(fit_gbm_afstro$bestTune),]$RMSE/mean(log(train_dat$newconc))* 100

sqrt(mean((fit_gbm_afstro$finalModel$fit-log(train_dat$newconc))^2))/mean(log(train_dat$newconc))* 100

# lin_rf<- train(
#   newconc~1+day_leach+as.factor(M)+
#     site_eng+as.factor(W)+
#     as.factor(WC)+
#     as.factor(WP)+
#     clay+
#     as.factor(JB)+
#     N_topsoil,
#   distribution = "gamma",
#   data = c_mess_master,
#   method = "gbm",
#   trControl = control,
#   verbose = FALSE,
#   #metric = "RMSE",
#   tuneGrid = param_gbm
# )