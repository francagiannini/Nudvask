
library(tidyverse)

# Concentration observations 
cut_CDB_raw <- readxl::read_excel(
  "data_raw/daily_cut_CDB.xls",
  sheet="Cal_data_allobs130323") |> 
  rename(Nconc=`measure nitrate_N conc [mg nitrateN per liter]`,
         month=mm,
         day=dd,
         Id=stnr) |> 
  mutate(harvest_year = ifelse(month < 4, year - 1, year)) |>
  mutate(merge_id = fct_cross(as.character(Id),
                              as.character(harvest_year),
                              sep = "_"),
         date = 
           lubridate::make_date(day = day, 
                                month = month, 
                                year = year)) |> 
  mutate(obs_id = paste(as.factor(Id), date, sep = "_"))

head(cut_CDB_raw)


# master with managment data cuted

master_b <- readxl::read_excel(
  "data_preproc/master_b.xlsx") |> 
  mutate(merge_id = fct_cross(as.character(Id),
                               as.character(harvest_year),
                               sep = "_")) |> 
  select( 
    # identification
    "Id",	"harvest_year",	
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

# DMI 

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
  mutate(date=lubridate::make_date(year=Year, month=Month, day=Day),
         merge_dmi=fct_cross(as.factor(DMIGRIDNUM), as.factor(date), sep = "_")
  ) |> 
  select(date, DMIGRIDNUM, GlobRad, AirTemp,Precip, merge_dmi) |> 
  mutate(
    #nwe features/covariates
    tvspp=AirTemp/ifelse(Precip<=0,0.1,Precip))

# Drain
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
    afstro_sum3 = rollapply(afstroemning, width = 2, FUN = sum, align = "right",fill = NA),
    afstro_sum7 = rollapply(afstroemning, width = 6, FUN = sum, align = "right",fill = NA)
  ) |>
  ungroup()


# Environmental temporal covariates meteorological and bio meteorological and drain 

daily_covar <-
  merge(wea, dmi_table) |>
  # days
  mutate(day_harv = as.numeric(as.Date(date) - 
                                 as.Date(paste(harvest_year, "04", "01", sep = "-"))),
         day_leach = as.numeric(as.Date(date) - 
                                  as.Date(ifelse( month < 8,
                                                  paste(year - 1, "08", "01", sep ="-"),
                                                  paste(year, "08", "01", sep = "-")))
         )) |> 
  # features from previous days and lags
  group_by(Id) |>
  arrange(date) |>
  
  mutate(
  Precip_lag1 = lag(Precip,1),
  Precip_lag2 = lag(Precip,2),
  Precip_lag1 = lag(Precip,3),
  
  Precip_sum3 = rollapply(Precip, width = 2, FUN = sum, align = "right",fill = NA),
  Precip_sum7 = rollapply(Precip, width = 6, FUN = sum, align = "right",fill = NA),
  
  GlobRad_lag1 = lag(GlobRad,1),
  GlobRad_sum3 = rollapply(GlobRad, width = 2, FUN = sum, align = "right",fill = NA),
  GlobRad_sum7 = rollapply(GlobRad, width = 6, FUN = sum, align = "right",fill = NA),
  
  AirTemp_lag1 = lag(AirTemp,1),
  AirTemp_lag2 = lag(AirTemp,2),
  AirTemp_lag2 = lag(AirTemp,3),
  
  AirTemp_ave3 = rollapply(AirTemp, width = 2, FUN = mean, align = "right", fill = NA),
  AirTemp_sum3 = rollapply(AirTemp, width = 2, FUN = sum, align = "right",fill = NA),
  
  AirTemp_ave7 = rollapply(AirTemp, width = 6, FUN = mean, align = "right",fill = NA),
  AirTemp_sum7 = rollapply(AirTemp, width = 6, FUN = sum, align = "right",fill = NA),
  
  tvspp_3d = 
    rollapply(AirTemp, width = 2, FUN = mean, align = "right", fill = NA)/
    ifelse(
      rollapply(Precip, width = 2, FUN = sum, align = "right",fill = NA)<=0,
      0.01,
      rollapply(Precip, width = 2, FUN = sum, align = "right",fill = NA))
  ) |> 
  ungroup() |> 
  # cumulative in harvest_year 
  group_by(Id, harvest_year) |>
  arrange(date) |>
  mutate(
    afstro_sumhy = cumsum(afstroemning),
    Precip_sumhy = cumsum(Precip),
    AirTemp_sumhy = cumsum(AirTemp),
    Globrad_sumhy = sumsum(GlobRad)
  ) |>
  ungroup()

# head(daily_covar)
#   
# daily_covar |>  filter(DMIGRIDNUM=="10233") |> 
#     group_by(Id) |> 
#     arrange(day)
#     mutate(Precip_1=lubridate::lag(Precip,1))


# Merge 


wea |> 
  filter(merge_id, unique(cut_CDB_raw$merge_id)) |>  

