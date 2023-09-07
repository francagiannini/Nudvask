library(tidyverse)
library(mgcv)
library(MASS)
library(stringr)
library(gamm4)
library(ggthemes)
library(viridis)
library(cowplot)
library(kableExtra)
library(docxtools)
library(knitr)
library(gratia)
library(latex2exp)
#install.packages("gratia")
library(gratia)

theme_set(theme_bw())
theme_update(panel.grid = element_blank())

df_m_imp <- readRDS(
  "C:/Users/au710823/OneDrive - Aarhus universitet/NLESS2022Fran/NLESSdata/Nudvask/Nudvask/df_m_imp.RDS")

df_gen <-
  df_m_imp |>
  dplyr::select(
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
    #"N_from_grassing_animals",
    #"N_topsoil",
    #"clay",
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
    #"X",
    #"Y",
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
    "afstro_cumsumhy",
    "Precip_cumsumhy",
    "afstro_sumhy",
    "Precip_sumhy",
    "AirTemp_avehy",
    "Globrad_avehy",
    "weigth_astro",
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


df_gen |> #filter(!Winter_nles5==7) |> 
  ggplot(aes(y=meancon, x=month)) + 
  geom_boxplot(alpha=.3, aes(group=as.factor(month), 
                             col = season)) + 
  geom_smooth(alpha=0.3, col="gray40")+
  #ggtitle("TITLE") + 
  facet_grid(clay_cat ~WC#.#Main_nles5
             , scale="free") +
  scale_x_discrete(name = NULL, breaks = NULL) +
  
  theme_bw()# these lines are optional
  #theme(legend.position = "bottom")

df_gen |> filter(!Winter_nles5==7) |> 
  ggplot(aes(y=meancon, x=Precip_cumsumhy)) + 
    geom_point(alpha=.2, aes(col = season))+
  # geom_boxplot(alpha=.3, aes(group=as.factor(month), 
  #                            col = season)) + 
  geom_smooth(alpha=0.3, col="black")+

  #ggtitle("TITLE") + 
  facet_grid(clay_cat ~ Winter_nles5 
             , scale="free") +
  scale_x_discrete(name = NULL, breaks = NULL) +
  
  theme_bw()# these lines are optional
#theme(legend.position = "bottom")


# Just temporal variability 
no_modGS_temp <- gam(meancon ~ 
                          s(month,clay_cat, k=5, bs="fs")+
                          s(month,WC , k=5, bs="fs"),
                        data=df_gen, 
                        method="REML", 
                        family = Gamma(link = "log"))

summary(no_modGS_temp)
draw(no_modGS_temp, residuals = TRUE)
appraise(no_modGS_temp)

sm <- 
  smooth_estimates(no_modGS_temp, 
                   smooth = "s(month,clay_cat)", 
                   data = df_gen) |>
  add_confint()

sm |>
  ggplot(aes(
    x = month,
    y = est,
    colour = clay_cat,
    group = clay_cat
  )) +
  geom_ribbon(aes(
    ymin = lower_ci,
    ymax = upper_ci,
    fill = clay_cat,
    colour = NULL
  ),
  alpha = 0.2) +
  geom_line() +
  labs(
    title = "Evaluation of smooth s(month,clay_cat)",
    colour = "clay_cat",
    fill = "clay_cat"
  )

sm2 <- 
  smooth_estimates(no_modGS_temp, 
                   smooth = "s(month,WC)", 
                   data = df_gen) |>
  add_confint()

fitted_values(no_modGS_temp, data = df_gen) |>
  ggplot(aes(
    x = month,
    y = fitted,
    colour = clay_cat,
    group = clay_cat
  )) +
  geom_ribbon(aes(
    ymin = lower,
    ymax = upper,
    fill = clay_cat,
    colour = NULL
  ),
  alpha = 0.2) +
  geom_line() +
  labs(
    title = "Fitted values from model",
    y = expression(hat(y)),
    colour = "clay_cat",
    fill = "clay_cat"
  )

sm2 |>
  ggplot(aes(
    x = month,
    y = est,
    colour = WC,
    group = WC
  )) +
  geom_ribbon(aes(
    ymin = lower_ci,
    ymax = upper_ci,
    fill = WC,
    colour = NULL
  ),
  alpha = 0.2) +
  geom_line() +
  labs(
    title = "Evaluation of smooth s(month,WC)",
    colour = "WC",
    fill = "WC"
  )

# Just temporal variability plus N level
no_modGS_timeN <- gam(meancon ~ 
                       s(N_mineral_spring, k=3, bs='tp')+
                       s(month,clay_cat, k=5, bs="fs")+
                       s(Main_nles5, WC , k=5, bs="re"),
                     data=df_gen, 
                     method="REML", 
                     family = Gamma(link = "log"))

summary(no_modGS_timeN)
appraise(no_modGS_timeN)
draw(no_modGS_timeN, residuals = TRUE)


# Just temporal variability plus N level


no_modGS_fullI <- gam(meancon~ 
                  s(N_mineral_spring, k=3, bs='tp')+
                  s(month,clay_cat, k=5, bs="fs")+
                  s(Main_nles5, WC , k=5, bs="re")+
                  s(clay_cat, k=3, bs="re")+
                  s(Main_nles5, k=12, bs="re")+
                  s(Winter_nles5, k=12, bs="re")+  
                  s(Precip_cumsumhy,clay_cat, k=5, bs="fs")+
                  s(Precip_sum365, k=3, bs='tp'),
                data=df_gen, 
                method="REML", 
                family="Gamma"(link="inverse"))

summary(no_modGS_fullI)
appraise(no_modGS_fullI)

draw(no_modGS_fullI, residuals = TRUE, scales = "fixed")
coef(no_modGS_fullI)
#plot(no_modGS_example, shade = T)

no_modGS <- gam(meancon ~ #s(month, k=3, bs="tp") +
               #s(clay_cat, k=3, bs="re")+
               s(month,clay_cat, k=5, bs="fs")+
               s(Main_nles5, k=12, bs="re")+
               s(month,Winter_nles5, k=5, bs="fs")+
               s(N_mineral_spring, k=3, bs='tp')+
               s(Precip_sum365, k=3, bs='tp')#+
              
               #s(N_mineral_spring, Winter_nles5, k=9, bs='fs')
                 ,
                data=df_gen, 
               method="REML", 
               family="gaussian"(link="log"))

summary(no_modGS)
draw(no_modGS)
coef(no_modGS)

no_modGS

no_modGS2 <- gam(#log()
  meancon ~ #s(month, k=3, bs="tp") +
                  #s(clay_cat, k=3, bs="re")+
                  s(month,clay_cat, k=5, bs="fs")+
                  s(Main_nles5, k=12, bs="re")+
                  s(prev_Winter_nles5, k=12, bs="re")+
                  s(month,Winter_nles5, k=5, bs="fs")+
                  s(N_mineral_spring, k=3, bs='tp')+
                  s(Precip_sum365, k=5, bs='tp')+
                  s(Globrad_avehy, k=5, bs='tp')#+
                  #s(tvspp_90, k=5, bs='tp')#+
                
                #s(N_mineral_spring, Winter_nles5, k=9, bs='fs')
                ,
                data=df_gen, 
                method="REML", 
                family=Gamma#"gaussian"
                  )


summary(no_modGS2)

draw(no_modGS2)
coef(no_modGS2)

no_modGS

no_modte <- gam(log(meancon) ~ 
                  t2(month, clay_cat, Winter_nles5, bs=c("tp", "re", "re"),
                     k=c(5, 10, 6), m=2, full=TRUE) +
                  te(Main_nles5, k=12, bs="re")+
                  te(N_mineral_spring, k=5, bs='tp'),
               data=df_gen, 
               method="REML", 
               family="gaussian")
summary(no_modte)
draw(no_modte)
k.check(no_modte)

df_gen$month2 <- df_gen$month^2

df_gen$month3 <- df_gen$month^3

df_gen |> ggplot(aes(x=month, y=Precip_sum365))+ geom_point()+geom_smooth()

library(lmerTest)

linear_translate <- lmer(
  log(meancon)~month+month2+month3+N_mineral_spring+Precip_sum365+
    Winter_nles5+clay_cat+
    month:clay_cat+month2:clay_cat+month3:clay_cat+(1 | harvest_year),
  data = df_gen)


summary(linear_translate)

anova(linear_translate, type="F")

# no_modG_pred <- cbind(CO2_modG_pred,
#                     predict(CO2_modG, 
#                             CO2_modG_pred, 
#                             se.fit=TRUE, 
#                             type="response"))
