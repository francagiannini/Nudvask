# Tools ----
library(tidyverse)
library(zoo)
library(ggbreak)
library(readxl)
library(utils)
library(sf)
library(tmap)
library(RColorBrewer)

# Concentration observations ----
# filtered and checked by CDB May 2023

crop_names_codes <- readxl::read_excel("data_raw/nles5_crop__code_param.xlsx",
                                       sheet = "nwe_coding_crops") |>   
  select(numeric_codification, crop_name)

conc_raw <- readxl::read_excel("data_raw/conc_raw_combined_with soildata_120523_7.xlsx",
                               sheet = "conc_raw") |> 
  filter(!is.na(interaction(crop_Main, Clay))) |> 
  mutate(merge_id = fct_cross(as.character(ident),
                              as.character(harvest_year),
                              sep = "_"))

conc_raw <- conc_raw |>
  mutate(
    'crop_main_name' =recode(crop_Main,!!!crop_names_codes$crop_name),
    'crop_winter_name'=recode(crop_Winter,!!!crop_names_codes$crop_name))

summary(conc_raw)

conc_raw

conc_raw |> 
  ggplot(aes(y=ident,x=year))+
  scale_y_break(c(0, 1000)) +
  scale_y_break(c(1200,2000))+
  geom_point(col="darkgreen")+
  theme_bw()


table(conc_raw$crop_main_name,conc_raw$harvest_year) |> 
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


table(conc_raw$crop_winter_name,conc_raw$harvest_year) |> 
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



# Managmentmaster with  data cuted ----

master_b <- readxl::read_excel(
  "data_preproc/master_b.xlsx") |> 
  mutate(merge_id = fct_cross(as.character(Id),
                              as.character(harvest_year),
                              sep = "_")) |> 
  select( 
    # identification
    "Id",	"harvest_year",	"merge_id",
    # Site and spatial id 
    "site_eng","DMIGRIDNUM","X_CENTRE","Y_CENTRE","X","Y",
    # crop sequence
    "WC",	"N_fix",	"MP",	"WP",	"M",	"W",	
    #soil
    "clay", "JB",
    #annual leaching
    "N_leaching",
    # Ninouts
    "N_mineral_spring",	"N_mineral_autuomn"	,	
    "N_min_year-1",	"N_min_year-2",	"N_fix_year",	"N_fix_year-1",	"N_fix_year-2",	
    "N_org_year",	"N_org_year-1",	"N_org_year-2",	
    "N_from_grassing_animals",	"N_topsoil")

# DMI ----
fnames <- as.list(paste(
  "O:\\Tech_AGRO\\Jornaer\\Franca\\N_conc\\10kmgridnew_geusprec22\\", 
  unique(master_b$DMIGRIDNUM[which(!is.na(master_b$DMIGRIDNUM))]),
  "8022", 
  ".dwf", 
  sep = ""))

names(fnames) <- unique(master_b$DMIGRIDNUM[which(!is.na(master_b$DMIGRIDNUM))])

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
  select(!c(saedidentnr,
            drain,
            Intpol_newconc,
            udvaskday,
            sumudvask,
            Maaltkonc,
            sumafstroem)) |> 
  mutate(
    obs_id = paste(Id, date, sep = "_"),
    drain_bi = ifelse(afstroemning > 0.3, 1, 0),
    leach_year = ifelse(month<8,
                        paste(year-1),
                        paste(year)),
    harvest_year = ifelse(month<4, 
                          paste(year - 1), 
                          paste(year))) |> 
  group_by(Id, harvest_year) |>
  arrange(date) |>
  mutate(
    afstro_sum = cumsum(afstroemning),
    afsto_lag1 = lag(afstroemning,1),
    afstro_sum3 = rollapply(afstroemning, width = 2, FUN = sum, align = "right",fill = 0),
    afstro_sum7 = rollapply(afstroemning, width = 6, FUN = sum, align = "right",fill = 0),
    afstro_sum15 = rollapply(afstroemning, width = 15, FUN = sum, align = "right",fill = 0),
    afstro_sum30 = rollapply(afstroemning, width = 30, FUN = sum, align = "right",fill = 0)
  ) |>
  ungroup()

# Sites ----

sites<- read.table("data_preproc/sites.txt", sep = "\t",header = T)


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

# Environmental temporal covariates meteorological and bio meteorological and drain ----

daily_co <-
  merge(wea_s, dmi_table, by = 'merge_dmi',no.dups=FALSE)

remove(dmi_table)

daily_covar <- daily_co |> #top_n(10) |> 
  select(!contains(".y")) |> 
  rename(date=date.x,
         DMIGRIDNUM=DMIGRIDNUM.x) |> 
  # days
  mutate(day_harv = as.numeric(as.Date(date) - 
                                 as.Date(paste(harvest_year, "04", "01", sep = "-"))),
         day_leach = as.numeric(as.Date(date) - 
                                  as.Date(ifelse(month < 8,
                                                 paste(year - 1, "08", "01", sep ="-"),
                                                 paste(year, "08", "01", sep = "-")))
         )) |> 
  # features from previous days and lags
  group_by(Id) |>
  arrange(date) |>
  mutate(
    Precip_lag1 = lag(Precip,1),
    #Precip_lag2 = lag(Precip,2),
    #Precip_lag3 = lag(Precip,3),
    #Precip_lag7 = lag(Precip,7),
    Precip_lag14 = lag(Precip,14),
    Precip_lag30 = lag(Precip,30),
    Precip_lag60 = lag(Precip,60),
    Precip_lag90 = lag(Precip,90),
    
    Precip_sum3 = rollapply(Precip, width = 2, FUN = sum, align = "right",fill = 0),
    #Precip_sum7 = rollapply(Precip, width = 6, FUN = sum, align = "right",fill = NA),
    Precip_sum14 = rollapply(Precip, width = 13, FUN = sum, align = "right",fill = 0),
    Precip_sum28 = rollapply(Precip, width = 27, FUN = sum, align = "right",fill = 0),
    Precip_sum90 = rollapply(Precip, width = 89, FUN = sum, align = "right",fill = 0),
    Precip_sum180 = rollapply(Precip, width = 179, FUN = sum, align = "right",fill = 0),
    Precip_sum365 = rollapply(Precip, width = 364, FUN = sum, align = "right",fill = 0),
    
    Precip_ave3 = rollapply(Precip, width = 2, FUN = mean, na.rm = TRUE, align = "right",fill = NA),
    #Precip_ave7 = rollapply(Precip, width = 6, FUN = mean, na.rm = TRUE, align = "right",fill = NA),
    Precip_ave14 = rollapply(Precip, width = 13, FUN = mean, na.rm = TRUE, align = "right",fill = NA),
    Precip_ave28 = rollapply(Precip, width = 27, FUN = mean, na.rm = TRUE, align = "right",fill = NA),
    
    #GlobRad_lag1 = lag(GlobRad,1),
    #GlobRad_lag3 = lag(GlobRad,3),
    GlobRad_lag7 = lag(GlobRad,6),
    GlobRad_lag14 = lag(GlobRad,13),
    GlobRad_lag28 = lag(GlobRad,27),
    
    #GlobRad_sum3 = rollapply(GlobRad, width = 2, FUN = sum, align = "right",fill = 0),
    GlobRad_sum7 = rollapply(GlobRad, width = 6, FUN = sum, align = "right",fill = 0),
    GlobRad_sum14 = rollapply(GlobRad, width = 13, FUN = sum, align = "right",fill = 0),
    GlobRad_sum28 = rollapply(GlobRad, width = 27, FUN = sum, align = "right",fill = 0),
    
    #GlobRad_ave3 = rollapply(GlobRad, width = 2, FUN = mean, na.rm = TRUE, align = "right",fill = NA),
    GlobRad_ave7 = rollapply(GlobRad, width = 6, FUN = mean, na.rm = TRUE, align = "right",fill = NA),
    GlobRad_ave14 = rollapply(GlobRad, width = 13, FUN = mean, na.rm = TRUE, align = "right",fill = NA),
    GlobRad_ave28 = rollapply(GlobRad, width = 27, FUN = mean, na.rm = TRUE, align = "right",fill = NA),
    
    #AirTemp_lag1 = lag(AirTemp,1),
    #AirTemp_lag2 = lag(AirTemp,2),
    #AirTemp_lag3 = lag(AirTemp,3),
    AirTemp_lag14 = lag(AirTemp,14),
    AirTemp_lag30 = lag(AirTemp,30),
    AirTemp_lag7 = lag(AirTemp,60),
    
    AirTemp_ave3 = rollapply(AirTemp, width = 2, FUN = mean, na.rm = TRUE, align = "right", fill = NA),
    AirTemp_sum3 = rollapply(AirTemp, width = 2, FUN = sum, align = "right",fill = 0),
    
    AirTemp_ave7 = rollapply(AirTemp, width = 6, FUN = mean, na.rm = TRUE, align = "right",fill = NA),
    AirTemp_sum7 = rollapply(AirTemp, width = 6, FUN = sum, align = "right",fill = 0),
    
    AirTemp_ave14 = rollapply(AirTemp, width = 13, FUN = mean, na.rm = TRUE, align = "right",fill = NA),
    AirTemp_sum14 = rollapply(AirTemp, width = 13, FUN = sum, align = "right",fill = 0),
    
    AirTemp_ave28 = rollapply(AirTemp, width = 27, FUN = mean, na.rm = TRUE, align = "right",fill = NA),
    AirTemp_sum28 = rollapply(AirTemp, width = 27, FUN = sum, align = "right",fill = 0),
    
    tvspp_14d = 
      rollapply(AirTemp, width = 13, FUN = mean, na.rm = TRUE, align = "right", fill = NA)/
      ifelse(
        rollapply(Precip, width = 13, FUN = sum, align = "right",fill = 0)<=0,
        0.01,
        rollapply(Precip, width = 13, FUN = sum, align = "right",fill = 0)),
    
    tvspp_28d = 
      rollapply(AirTemp, width = 27, FUN = mean, na.rm = TRUE, align = "right", fill = NA)/
      ifelse(
        rollapply(Precip, width = 27, FUN = sum, align = "right",fill = 0)<=0,
        0.01,
        rollapply(Precip, width = 27, FUN = sum, align = "right",fill = 0)),
    
    tvspp_60= 
      rollapply(AirTemp, width = 60, FUN = mean, na.rm = TRUE, align = "right", fill = NA)/
      ifelse(
        rollapply(Precip, width = 60, FUN = sum, align = "right",fill = 0)<=0,
        0.01,
        rollapply(Precip, width = 60, FUN = sum, align = "right",fill = 0))
    
  ) |> 
  ungroup() |> 
  # cumulative in harvest_year 
  group_by(Id, harvest_year) |>
  arrange(date) |>
  mutate(
    afstro_sumhy = cumsum(afstroemning),
    Precip_sumhy = cumsum(Precip),
    AirTemp_avehy = mean(AirTemp, na.rm=TRUE),
    Globrad_avehy = mean(GlobRad, na.rm=TRUE)
  ) |>
  ungroup() |> 
group_by(Id, harvest_year,month) |>
  arrange(date) |>
  mutate(
    afstro_sum_month = cumsum(afstroemning),
    Precip_sum_month = cumsum(Precip),
    AirTemp_ave_month = mean(AirTemp),
    Globrad_ave_month = mean(GlobRad)
  ) |>
  ungroup()

#remove(daily_co)

# head(daily_covar)
#   
# daily_covar |>  filter(DMIGRIDNUM=="10233") |> 
#     group_by(Id) |> 
#     arrange(day)
#     mutate(Precip_1=lubridate::lag(Precip,1))

conc_raw <- conc_raw |> 
  mutate(date=make_date(year=year,month=month,day=day)) |> 
  mutate(obs_id = paste(ident, date, sep = "_"))

# Merge ----
db <- 
  merge(daily_covar,conc_raw, by='obs_id', all.x = TRUE) |>  
  #|> top_n(10) |> 
  select(!contains(".y"))  

colnames(db)<-gsub(".x","",colnames(db))    

db_f <- 
  merge(db,master_b, by='merge_id')|>  #top_n(10) |> 
  select(!contains(".y")) 

remove(db)

colnames(db_f) <- gsub(".x","",colnames(db_f))  

db_f <-db_f |>  filter(Id %in% unique(conc_raw$ident)) |> 
  mutate(measured=ifelse(is.na(meankonc),0,1))

db_f <- db_f |> filter(measured==1)

# last merge ----

daily_covar <- daily_covar |> unique()

db_c <- merge(db_f, daily_covar, by='merge_dmi') |> #top_n(5)
  select(!contains(".y")) |> unique()

colnames(db_c) <- gsub(".x","",colnames(db_c))  


write.table(db_c, "data_preproc/db_Ndaily_cut_0323.txt", sep = "\t")

writexl::write_xlsx(db_c,"data_preproc/db_Ndaily_cut_0323.xlsx",# sep = "\t",
                    col_names = TRUE,
                    format_headers = TRUE)

# Explore ----

table(db_f$measured)  

unique(conc_raw$Id)[!(unique(conc_raw$Id) %in% unique(wea_s$Id))]
#123 me faltan hay satan 

unique(conc_raw$Id)[!(unique(conc_raw$Id) %in% unique(master_b$Id))]
#siiiiiii gooooool vamos todaviaaaaa una que coincida LPMQLRP

# Explore

hist(db_f$meankonc, col="darkgreen")

db_f |> ggplot(aes(x=meankonc))+
  geom_histogram(aes(position="identity"), fill = "darkgreen",bins=80)+
  #stat_density(geom = "line", aes(colour = "bla"))+
  ylab("Count")+ xlab("daily N concentrations (mg/L)" )+
  geom_rug() +
  theme_bw()

head(conc_raw)

db_f_m <-  db_f |> 
  filter(measured==1) |> 
  filter(!site_eng=="\\Skara\\")

table(db_f_m$site_eng,db_f_m$harvest_year) |> 
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
