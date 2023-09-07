# packages ----
library(tidyverse)
library(zoo)
library(ggbreak)
library(readxl)
library(utils)
library(sf)
library(tmap)
library(RColorBrewer)
library(ggpubr)
library(GGally)
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
library(tmap)

# Concentration observations ----
# filtered and checked by CDB May 2023

crop_names_codes <-
  readxl::read_excel("data_raw/nles5_crop__code_param.xlsx",
                     sheet = "nwe_coding_crops") |>
  select(numeric_codification, crop_name)

conc_raw <-
  readxl::read_excel("data_raw/conc_raw_combined_with soildata_120523_7.xlsx",
                     sheet = "conc_raw") |>
  #filter(!is.na(interaction(crop_Main, Clay))) |>
  mutate(merge_id = fct_cross(as.character(ident),
                              as.character(harvest_year),
                              sep = "_")) |> select(
                                ident,
                                harvest_year,
                                year,meankonc,
                                month,
                                day,
                                Clay,
                                Or_n_tonN_ha,
                                crop_Main,
                                crop_Winter,
                                Detailed_data_coubling_jb,
                                Gamma,
                                Mfu,
                                Vfu,
                                Main_crop_nles5,
                                Vinter_crop_nles5,
                                merge_id
                              ) |> 
  rename(Main_nles5=Main_crop_nles5,
         Winter_nles5=Vinter_crop_nles5,
         jbnr=Detailed_data_coubling_jb)

summary(conc_raw)

missing_conc_raw <- 
  conc_raw[which(complete.cases(conc_raw)==FALSE),]

#write.table(conc_raw_miss_artisanal, "data_raw//missing_conc_raw.txt")

missing_conc_raw_id_year <- 
  missing_conc_raw |> group_by(merge_id) |> summarise(n=n())

conc_raw_miss_artisanal <-
 read.table("data_preproc/missing_conc_raw_artesanal.txt",
                     header=T, dec=".", sep = "\t", na.strings = ".")

conc_raw <- bind_rows(conc_raw[which(complete.cases(conc_raw)==TRUE),], 
          conc_raw_miss_artisanal)

# Managment master with  data cuted ----

master <- readxl::read_excel(
  "data_preproc/master_merge_3107.xlsx")

missing <- conc_raw[which(!conc_raw$merge_id %in% master$merge_id),]

df_monthly <- conc_raw |>
  #select(harvest_year, ident, month, meankonc) |>
  group_by(
    ident,
    harvest_year,
    year,
    month,
    Clay,
    Or_n_tonN_ha,
    crop_Main,
    crop_Winter,
    jbnr,
    Gamma,
    Mfu,
    Vfu,
    Main_nles5,
    Winter_nles5,
    # afstroem_mm_1,
    # afstroem_mm2,
    # afstroem_mm3,
    # afstroem_mm4,
    # afstroem_mm5,
    # afstroem_mm6,
    # afstroem_mm7,
    # afstroem_mm8,
    # afstroem_mm_9,
    # afstroem_mm10,
    # afstroem_mm11,
    # afstroem_mm12,
    merge_id#,
    #crop_main_name,
    #crop_winter_name
  ) |>
  summarise(
    meancon = mean(meankonc, na.rm = TRUE),
    mediancon = median(meankonc, na.rm = TRUE),
    sdcon = sd(meankonc, na.rm = TRUE),
    n = n()
  ) |> unique()

df_monthly <- merge(df_monthly,master, by='merge_id', all.x = TRUE) |> 
  select(!contains(".y"))
colnames(df_monthly)<-gsub(".x","",colnames(df_monthly)) 

df_monthly <- df_monthly |>   
   mutate(
     clay=ifelse(is.na(Clay), clay, Clay),
     #N_org_year=ifelse(is.na(N_org_year),N_Org,N_org_year),
     #jbnr=ifelse(is.na(Detailed_data_coubling_jb),jbnr,Detailed_data_coubling_jb),
     Gamma=ifelse(is.na(Gamma),ggamma,Gamma),
     WC=ifelse(is.na(WC),ifelse("Vinter_crop_nles5"==6,1,WC),WC),
     prev_Main_nles5=ifelse(is.na(Mfu), MP, Mfu),
     prev_Winter_nles5=ifelse(is.na(Vfu), WP, Vfu),
     Main_nles5=ifelse(is.na(Main_nles5),M,Main_nles5),
     Winter_nles5=ifelse(is.na(Winter_nles5),W,Winter_nles5)) |> 
  select("merge_id","ident","harvest_year",
        "year","month","WC","N_mineral_spring","N_mineral_autuomn",
        "N_min_year-1","N_min_year-2",
        "N_f_year",
        "N_f_year-1","N_f_year-2","N_org_year","N_org_year-1",
        "N_org_year-2","N_from_grassing_animals","N_topsoil",
        "clay", "Gamma","jbnr",
        "prev_Main_nles5","prev_Winter_nles5",
        "Main_nles5","Winter_nles5",
        "meancon","mediancon","sdcon","n") |> 
  mutate(
    'crop_main_name' =recode(Main_nles5,!!!crop_names_codes$crop_name),
    'crop_winter_name'=recode(Winter_nles5,!!!crop_names_codes$crop_name))

summary(df_monthly)

# Sites ----

sites<- read.table("data_preproc/sites.txt", sep = "\t",header = T)

## monthly concentration sites ----

df_monthly <-
  merge(df_monthly,
        sites,
        by.x = 'ident',
        by.y = 'strno',
        sort = FALSE)

# df_monthly_fil <- df_monthly |> 
#   filter(#between(harvest_year,2016,2018) & 
#            site_eng == c("\\Foulum\\", "\\Flakkebjerg\\") & 
#              crop_main_name == "vinterhved" #&
#              #harvest_year== c(2016,2017)
#            )
# 
# df_monthly_fil |> 
#   ggplot(aes(month, meanconc, col=interaction(site_eng)))+
#   geom_point()+
#   geom_smooth()+
#   theme_light()
  

#colnames(df_monthly) <- gsub(".x","",colnames(df_monthly)) 

df_monthly <- df_monthly |> 
  dplyr::filter(!site_eng == "\\Skara\\")|> 
  dplyr::filter(harvest_year>1989)

summary(df_monthly)

table(df_monthly$site_eng,df_monthly$harvest_year) |> 
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
  scale_y_discrete(name = "Site")+theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

## map ----
tmap_mode("view")

df_monthly  |> 
  st_as_sf(coords = c("X", "Y"), crs = 25832) |> 
  group_by(site_eng) |> 
tm_shape()+
#tm_polygons(col='DMIGRIDNUM', alpha = 0.5)+
#tm_shape(sites)+
tm_dots(col='site_eng', size=0.5, palette="Set2") #+
#tm_text("site_eng", 
          #clustering = TRUE,   
          #remove.overlap = TRUE)
  

df_monthly |> 
  ggplot(aes(y=ident,x=year))+
  scale_y_break(c(0, 1000)) +
  scale_y_break(c(1200,2000))+
  geom_point(col="darkgreen")+
  theme_bw()


table(df_monthly$crop_main_name,df_monthly$harvest_year) |> 
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
  scale_y_discrete(name = "Main Crop")+theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))


table(df_monthly$crop_winter_name,df_monthly$harvest_year) |> 
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
  scale_y_discrete(name = "Winter crop")+theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))



# DMI ----
fnames <- as.list(paste(
  "O:\\Tech_AGRO\\Jornaer\\Franca\\N_conc\\10kmgridnew_geusprec22\\", 
  unique(df_monthly$DMIGRIDNUM[which(!is.na(df_monthly$DMIGRIDNUM))]),
  "8022", 
  ".dwf", 
  sep = ""))

names(fnames) <- unique(df_monthly$DMIGRIDNUM[which(!is.na(df_monthly$DMIGRIDNUM))])

dmi_list <- lapply(fnames, read.fwf, 
                   sep ="\t",
                   skip = 36,
                   widths= c(8, 6, 5, 8,8 ,10),
                   col.names= 
                     c("Year","Month","Day","GlobRad", "AirTemp","Precip"))


dmi_table <- do.call(rbind,dmi_list)|> 
  bind_cols("dmi"=row.names(do.call(rbind,dmi_list))) |> 
  separate(dmi,c('DMIGRIDNUM','idx')#, sep="." 
  ) |> 
  filter(Year>1988) |> 
  mutate(date=lubridate::make_date(year=Year, month=Month, day=Day),
         merge_dmi=fct_cross(as.factor(DMIGRIDNUM), as.factor(date), sep = "_")
  ) |> 
  select(date, DMIGRIDNUM, GlobRad, AirTemp,Precip, merge_dmi) |> 
  mutate(
    #nwe features/covariates
    tvspp=AirTemp/ifelse(Precip<=0,0.1,Precip))

# Drain ----
wea <- read.table("data_raw/wea_txt.txt", sep = "\t", header = T) |>
  mutate(date = lubridate::make_date(day = mday, month = month, year = year)) |>
  rename('Id' = 'eksponr') |>
  select(!c(
    saedidentnr,
    drain,
    Intpol_newconc,
    udvaskday,
    sumudvask,
    Maaltkonc#,
    #sumafstroem
  )) |>
  mutate(
    obs_id = paste(Id, date, sep = "_"),
    drain_bi = ifelse(afstroemning > 0.3, 1, 0),
    leach_year = ifelse(month < 8,
                        paste(year - 1),
                        paste(year)),
    harvest_year = ifelse(month < 4,
                          paste(year - 1),
                          paste(year))
  ) |>
  group_by(Id, harvest_year) |>
  arrange(date) |>
  mutate(
    afstro_sum = sum(afstroemning)#,
    # afsto_lag1 = lag(afstroemning, 1),
    # afstro_sum3 = rollapply(
    #   afstroemning,
    #   width = 2,
    #   FUN = sum,
    #   align = "right",
    #   fill = 0
    # ),
    # afstro_sum7 = rollapply(
    #   afstroemning,
    #   width = 6,
    #   FUN = sum,
    #   align = "right",
    #   fill = 0
    # ),
    # afstro_sum15 = rollapply(
    #   afstroemning,
    #   width = 15,
    #   FUN = sum,
    #   align = "right",
    #   fill = 0
    # ),
    # afstro_sum30 = rollapply(
    #   afstroemning,
    #   width = 30,
    #   FUN = sum,
    #   align = "right",
    #   fill = 0
    # )
  ) |>
  ungroup()


## wea sites ----

wea_s <- merge(wea, sites, by.x = 'Id', by.y = 'strno') |>
  mutate(
    merge_dmi = fct_cross(as.factor(DMIGRIDNUM), as.factor(date), sep = "_"),
    merge_id = fct_cross(as.character(Id),
                         as.character(harvest_year),
                         sep = "_")
  ) |> 
  select(!c(data_use,source))

remove(wea)

# Monthly Environmental temporal covariates  ----
# (meteorological plus bio meteorological plus drain)

daily_co <-
  merge(dmi_table, wea_s, by = 'merge_dmi',no.dups=FALSE, all.x=TRUE)

remove(dmi_table)

monthly_covar_primitive <- daily_co |> #top_n(100) |>
  select(!contains(".y")) |>
  rename(date = date.x,
         DMIGRIDNUM = DMIGRIDNUM.x) |>
  unique() |> 
  filter(!is.na(merge_id)) |> 
  # # days
  # mutate(day_harv = as.numeric(as.Date(date) -
  #                                as.Date(paste(
  #                                  harvest_year, "04", "01", sep = "-"
  #                                ))),
  #        day_leach = as.numeric(as.Date(date) -
  #                                 as.Date(ifelse(
  #                                   month < 8,
  #                                   paste(year - 1, "08", "01", sep =
  #                                           "-"),
  #                                   paste(year, "08", "01", sep = "-")
  #                                 ))),
  #        month=month(date),
  #        merge_id = fct_cross(as.character(Id),
  #                             as.character(harvest_year),
  #                             sep = "_")#,
  #        #afstroemning=as.numeric(afstroemning)
  #        ) |> 
  group_by(merge_id,Id,site_eng, harvest_year,DMIGRIDNUM,X,Y,year,month) |>
  #arrange(year, Id, month) |> 
  summarise(
    afstro_sum_month = sum(afstroemning),
    Precip_sum_month =ifelse((sum(Precip))==0,0.01,(sum(Precip))),
    AirTemp_ave_month = mean(AirTemp),
    Globrad_ave_month = mean(GlobRad),
    tvspp_month = mean(AirTemp)/ifelse((sum(Precip))==0,0.01,(sum(Precip))),
    drain_days = sum(drain_bi)
    ) |> 
  ungroup()

monthly_covar <- monthly_covar_primitive |> #top_n(100) |>
  group_by(DMIGRIDNUM, Id) |>
  arrange(Id, year, month) |>
  mutate(
    Precip_sum60 = rollapply(
      Precip_sum_month,
      width = 2,
      FUN = sum,
      align = "right",
      na.rm = TRUE,
      partial = TRUE
    ),
    Precip_sum90 = rollapply(
      Precip_sum_month,
      width = 3,
      FUN = sum,
      align = "right",
      na.rm = TRUE,
      partial = TRUE
    ),
    Precip_sum180 = rollapply(
      Precip_sum_month,
      width = 6,
      FUN = sum,
      align = "right",
      na.rm = TRUE,
      partial = TRUE
    ),
    Precip_sum365 = rollapply(
      Precip_sum_month,
      width = 12,
      FUN = sum,
      align = "right",
      na.rm = TRUE,
      partial = TRUE
    ),
    AirTemp_ave60 = rollapply(
      AirTemp_ave_month,
      width = 2,
      FUN = mean,
      na.rm = TRUE,
      align = "right",
      fill = NA,
      partial = TRUE
    ),
    AirTemp_ave90 =  rollapply(
      AirTemp_ave_month,
      width = 3,
      FUN = mean,
      na.rm = TRUE,
      align = "right",
      fill = NA,
      partial = TRUE
    ),
    AirTemp_ave180 = rollapply(
      AirTemp_ave_month,
      width = 6,
      FUN = mean,
      na.rm = TRUE,
      align = "right",
      fill = NA,
      partial = TRUE
    ),
    
    tvspp_60 =
      rollapply(
        AirTemp_ave_month,
        width = 2,
        FUN = mean,
        na.rm = TRUE,
        align = "right",
        fill = NA,
        partial = TRUE
      ) /
      ifelse(
        rollapply(
          Precip_sum_month,
          width = 2,
          FUN = sum,
          align = "right",
          na.rm = TRUE,
          partial = TRUE
        ) <= 0,
        0.01,
        rollapply(
          Precip_sum_month,
          width = 2,
          FUN = sum,
          align = "right",
          na.rm = TRUE,
          partial = TRUE
        )
      ),
    
    tvspp_90 =
      rollapply(
        AirTemp_ave_month,
        width = 3,
        FUN = mean,
        na.rm = TRUE,
        align = "right",
        fill = 0
      ) /
      ifelse(
        rollapply(
          Precip_sum_month,
          width = 3,
          FUN = sum,
          align = "right",
          na.rm = TRUE,
          partial = TRUE
        ) <= 0,
        0.01,
        rollapply(
          Precip_sum_month,
          width = 3,
          FUN = sum,
          align = "right",
          na.rm = TRUE,
          partial = TRUE
        )
      )
  ) |>
  ungroup() |>
  arrange(year, month) |>
  # cumulative in harvest_year
  group_by(merge_id) |>
  mutate(
    afstro_cumsumhy = cumsum(afstro_sum_month),
    Precip_cumsumhy = cumsum(Precip_sum_month),
    afstro_sumhy = sum(afstro_sum_month),
    Precip_sumhy = sum(Precip_sum_month),
    AirTemp_avehy = mean(AirTemp_ave_month, na.rm = TRUE),
    Globrad_avehy = mean(Globrad_ave_month, na.rm = TRUE)
  ) |>
  ungroup() |>
  mutate(
    merge_mon = fct_cross(
      as.character(Id),
      as.character(harvest_year),
      as.character(month),
      sep = "_"
    ),
    weigth_astro = afstro_sum_month / afstro_sumhy,
    season = case_when(
      month %in% 3:5 ~ 'spring',
      month %in% 6:8 ~ 'summer',
      month %in% 9:11 ~ 'autumn',
      TRUE ~ 'winter'
    )
  )

monthly_covar |> ggplot(aes(y=Precip_cumsumhy, x=month))+
  geom_point()

  
saveRDS(monthly_covar, "monthly_covar.RDS")


#remove(daily_co)

# head(daily_covar)
#   
# daily_covar |>  filter(obs_id=="1029_2009-03-19")
# daily_covar |>  filter(Id=="1029"& year==2009)
# 10483
# daily_covar |>  filter(DMIGRIDNUM=="10483"& year==2009)
#|> 
#     group_by(Id) |> 
#     arrange(day)
#     mutate(Precip_1=lubridate::lag(Precip,1))

df_monthly <- df_monthly|> 
  mutate(
    merge_mon=fct_cross(as.character(ident),
                       as.character(harvest_year),
                       as.character(month),
                        sep = "_"))

# Merge ----
db <- 
  merge(df_monthly, monthly_covar, by='merge_mon', all.x = TRUE) |>  
  select(!contains(".y")) #|> 
  #select(!contains("afstroem_m")) |> unique()

colnames(db)<-gsub(".x","",colnames(db))    

write.table(db, "data_preproc/db_Nmonthly_cut_3107.txt", sep = "\t")

writexl::write_xlsx(db,
                    "data_preproc/db_Nmonthly_cut_3107.xlsx",# sep = "\t",
                    col_names = TRUE,
                    format_headers = TRUE)
