library(tidyverse)

fou_hy <- c_mess_master |> filter(site_eng == "\"Foulum\"" & 
                                 !is.na(Id) & 
                                 sted == 1033 &
                                 harvest_year.x==2001)

#table(fou$sted, fou$harvest_year.x)

fou_hy |> ggplot(aes(x=afstro_sum, y=meankonc))+
  geom_point()+
  geom_smooth()+
  theme_bw()

fou <- c_mess_master |> filter(site_eng == "\"Foulum\"" & 
                                    !is.na(Id) & 
                                    sted == 1033 &
                                    leach_year==2001
                                  )


fou |> ggplot(aes(x=afstro_sum, y=meankonc))+
  geom_point()+
  geom_smooth()+
  theme_bw()

fou |> ggplot(aes(x=afstro_sum, y=meankonc))+
  geom_point()+
  geom_smooth()+
  theme_bw()


fou_ave <- fou |> group_by(date,afstro_sum,afstroemning) |> 
  summarise(mean_conc=mean(meankonc)) |> 
  ungroup() |> mutate(id_val = row_number())

fou_ave |> ggplot(aes(x=afstro_sum, y=mean_conc, col=date))+
  geom_point()+
  geom_smooth()+
  theme_bw()


fou_wea <- wea |> filter(#site_eng == "\"Foulum\"" & 
                #!is.na(Id) & 
                sted == 1033 &
                leach_year==2001
)



#Create training set
train <- fou_ave |>  sample_frac(.7) 

train_gamm <- train|> 
  mutate(drain_sum=afstro_sum,
         #drain_sum2=
         conc=mean_conc)
#Create test set
test  <- anti_join(fou_ave, train, by = 'id_val')|> 
  mutate(drain_sum=afstro_sum,
         conc=mean_conc)


train <- train |> 
  mutate(Date=as.Date(date,"%y/%m/%d")) |> 
#  sample_frac(0.7) |> 
  complete(Date = seq.Date(#min(Date),#
                           from=as.Date("2001-08-01"), 
                           to=
                             #max(Date),#
                             as.Date("2002-07-31"), 
                           by="day")) |> 
  select(!date) |> 
  mutate(idx=seq(1,n(),1),
         drain=fou_wea$afstroemning,#if_else(afstroemning<0,0,afstroemning),
         drain_sum=fou_wea$afstro_sum,
         conc=ifelse(Date %in% c(min(Date), max(Date)),0,mean_conc))

dfN_spl <- train |> 
  mutate(
    period=
      cut(as.numeric(idx), train$idx[!is.na(train$conc)],
          labels=FALSE,
          include.lowest = TRUE,
          right=FALSE)
  ) |>
  group_split(period) 

period_list <- list()


for(i in 1:length(dfN_spl)){
  
  p <-as.data.frame(dfN_spl[[i]])
  
  pfin<- as.data.frame(
    dfN_spl[[ifelse(unique(p$period)==length(dfN_spl),i,i+1)]][ifelse(unique(p$period)==length(dfN_spl),nrow(p),1),])
  
  period_list[[i]] <- rbind(p, pfin) |>
    mutate(index = row_number()) |> unique()
}

# The interpolation function it runs for each period by row 
# calculating a nwe concentration for each row 

ci_func <- function(i, p = period_list, id) {
  #browser()
  
  cat("The ID is", id, "\n")
  cat("The period is", unique(p$period), "\n")
  
  C_a <- p[1, "conc"]
  C_b <- p[nrow(p), "conc"]
  
  D_a <- p[1, "drain"]
  D_b <- p[nrow(p), "drain"]
  
  B <- nrow(p)
  
  D_i <- p[id, "drain"]
  
  f_Di <- ((D_a / 2) + 
             ifelse(id>2,sum(p[2:(id - 1), "drain"]),0)+ #p[1,"drain"]) +
             (D_i / 2)) /
    ((D_a / 2) + 
       sum(p[2:(B - 1), "drain"]) + 
       (D_b / 2))
  
  f_Di <- ifelse(is.na(f_Di),0,f_Di)
  
  C_i = C_a + (C_b - C_a) * f_Di
  
  res = tibble('beh'=p[id,'beh'],
               'idx'=p[id,'idx'],
               'C_i'=ifelse(is.na(p[id,'conc']), C_i,p[id,'conc'])
  )
  
  return(res)
  
}

# Function to aply the period function to the list of periods

ci_func_df <- function(period_list_df) {
  
  lapply(1:(nrow(period_list_df)), function(x, p) {
    
    ci_func(p[x], p, id = x)
    
  }, p =  as.data.frame(period_list_df)
  )
  
}

### IV ----

# Running the interpolation 

interpol <- do.call(bind_rows, lapply(period_list, ci_func_df)) |> 
  distinct(idx,.keep_all = TRUE)

# Writing the nwe concentration
train$nweconc_inter <- interpol$C_i

library(mgcv)

#mod_lm = gam(conc ~ s(drain_sum, bs = "cr") , data = train_gamm)

mod_lm=lm(conc ~ drain_sum + I(drain_sum^2), data = train_gamm)

test$nweconc_smooth <- predict(mod_lm, test)#predict.gam(mod_lm, test)

train$nweconc_smooth_tr <-  predict(mod_lm, train)#predict.gam(mod_lm, train)

train |> 
  filter(Date %in% test$date) |> 
  mutate(resid_inter=test$mean_conc-nweconc_inter,
         resid_smooth=test$mean_conc-test$nweconc_smooth) |> 
  summarise(RMSPE_inter=sqrt(mean(resid_inter^2)),
            RMSPE_rel_inter=sqrt(mean(resid_inter^2))/mean(test$mean_conc)*100,
            
            RMSPE_smooth=sqrt(mean(resid_smooth^2)),
            RMSPE_rel_smooth=sqrt(mean(resid_smooth^2))/mean(test$mean_conc)*100)
  
plot_baba <- test |> mutate(conc_obs_test=mean_conc) |> bind_rows(train)

plot_baba|> 
  ggplot(aes(x=drain_sum, y=nweconc_inter#, 
             #col=as.factor(ifelse(is.na(id_val),0,1))
             ))+
  geom_point()+
  geom_point(aes(y=nweconc_smooth_tr, col="smooth"))+
  geom_point(aes(y=conc_obs_test, col="test", size=2))+
  #scale_y_continuous(
  # sec.axis = sec_axis(~ ."nweconc_smooth")
  # )
  #geom_smooth()+
  theme_bw()






# dfN <- dfN |> 
#   group_by(beh) |>
#   mutate(
#     cumdrain=cumsum(drain),
#     leach = drain*nweconc,
#     int_outflux = cumsum(drain*nweconc)
#   )
