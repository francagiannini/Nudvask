library(tidyverse)
library(lubridate)
library(readxl)
library(utils)
library(sf)
library(tmap)
library(RColorBrewer)

# Measured concentration ----

renset <- read_excel(
  "data_raw/Scenarier20190909B4_231120_sendt_GBM_anonym_sendtdata_med_id_vers_medNkonc Franca_updated_cdb211122.xls",
  sheet = "renset_nconc_cal12") |> 
  select(ident,year,month,day,meankonc)

Nkncval <- read_excel(
  "data_raw/Scenarier20190909B4_231120_sendt_GBM_anonym_sendtdata_med_id_vers_medNkonc Franca_updated_cdb211122.xls",
  sheet = "N koncentration_validering") |> 
  select(ident,year,month,day,meankonc)

valNkonc <- read_excel(
  "data_raw/Scenarier20190909B4_231120_sendt_GBM_anonym_sendtdata_med_id_vers_medNkonc Franca_updated_cdb211122.xls",
  sheet = "val_Nkonc") |> 
  select(ident,year,month,day,meankonc)

conc_suplI <- read_excel(
  "data_raw/Scenarier20190909B4_231120_sendt_GBM_anonym_sendtdata_med_id_vers_medNkonc Franca_updated_cdb211122.xls",
  sheet = "conc_suppl_0001_2926") |> 
  select(ident,year,month,day,meankonc) 

conc_suplII <-
  read_excel(
  "data_raw/Scenarier20190909B4_231120_sendt_GBM_anonym_sendtdata_med_id_vers_medNkonc Franca_updated_cdb211122.xls",
  sheet = "Conc_suplement_2701_2926") |> 
  select(ident,year,month,day,meankonc)

conc_nov <- bind_rows(renset,Nkncval,valNkonc,conc_suplI,conc_suplII) |> 
  unique() |> 
  filter(meankonc<200)

conc_nov |> ggplot(aes(y=ident,x=year))+geom_point()

summary(conc_nov)
plot(conc_nov$meankonc)
abline(100,0, col=2)

hist(conc_nov$meankonc)
(sum(conc_nov$meankonc==0)/nrow(conc_nov))*100

### answer to Christen

# CDB <1003  data obtained before 1990
conc_nov |> dplyr::filter(ident<1003) |> 
  group_by(ident) |> summarise(n=n(), minyear=min(year), maxyear=max(year))

yless1003 <- conc_nov |> dplyr::filter(ident<1003) |>  
  group_by(year,ident) |> summarise(n=n())
# we dismiss from 983 to 1001
conc_nov_dep <- conc_nov |> filter(!between(ident,983,1001))
# The other stays because they are LOOPs and 1002 has acceptable data 

# 2430-2436  Do not exist 
nana2430_2436 <- conc_nov |> dplyr::filter(between(ident,2430,2436)) #|> group_by(ident,year) |> summarise(n=n())
# CDB They do exist they stay they are data from 2010 and 2011

# CDB 2438-2499  Before 1990
nana2438_2499 <- conc_nov |> dplyr::filter(between(ident,2438,2499)) |> 
  group_by(ident,year) |> summarize(n())
#|> group_by(ident) |> summarise(n=n(), minyear=min(year), maxyear=max(year))
# They are not before 1990 we only dismiss ids from 2463to2499 the other stays
conc_nov_dep <- conc_nov_dep |> filter(!between(ident,2463,2499))

# CDB 2666-2683  Before 1990  (1988) 
conc_nov |> dplyr::filter(between(ident,2666,2683)) 
#|> group_by(ident) |> summarise(n=n(), minyear=min(year), maxyear=max(year))
# we do dismiss them 
conc_nov_dep <- conc_nov_dep |> filter(!between(ident,2666,2683))

# 2766-2799  In 1990  - to low number of observations (only four times) 
na2766_2799 <- conc_nov |> dplyr::filter(between(ident,2766,2799))
# we do dismiss them 
conc_nov_dep <- conc_nov_dep |> filter(!between(ident,2766,2799))

# 2849-2900  Data from 1989  and only four observations # dismiss
na2849_2900 <- conc_nov |> dplyr::filter(between(ident,2849,2900))
# we do dismiss them 
conc_nov_dep <- conc_nov_dep |> filter(!between(ident,2849,2900))

# CDB Missing and good data 2 sted 2916 2917
conc_nov |> dplyr::filter(between(ident,2916,2917)) |> 
  group_by(ident) |> 
  summarise(n=n(), minyear=min(year), maxyear=max(year))
# They stay en of the discussion 

# CDB 2932-2939  These data are from Foulum – but not included due to crops not included ïn the calibration of the NLES5
conc_nov |> dplyr::filter(between(ident,2932,2939)) |> 
  group_by(ident) |> 
  summarise(n=n(), minyear=min(year), maxyear=max(year))
# We actually do not have it but anyway is not a good reason to dismiss them 

# Sites -----
# Data set build from information of the NLESS5 ppr, the report an going back to the source

sites <- read.table(
  "data_preproc/sites_navn.txt",
  sep = "\t",
  header = TRUE,
  dec = ".",
  na.strings = ".",
  quote = ""
) |> #mutate(X=as.numeric(as.factor('X')),Y=as.numeric(as.factor('Y'))) |>  
  drop_na(site_eng) |> 
  st_as_sf(coords = c("X", "Y"), crs = 25832
           #st_crs(all_sites_pont)
  ) |> select(!c(StedNavn , sites_dk))


dmi_grid <- st_read(
  "O:\\Tech_Agro-data1\\Geodata\\Denmark_national\\Climate\\DMI_GRID\\DMI_10km.shp")

table(sites$site_eng)

tmap_mode("view")

sites_clust_map <- 
  tm_shape(dmi_grid)+
  tm_polygons(col='DMIGRIDNUM', alpha = 0.5)+
  tm_shape(sites)+
  tm_dots()+
  tm_text("site_eng", 
          clustering = TRUE,   
          remove.overlap = TRUE)

# DMI grid ----

sites <- st_join(sites,dmi_grid) |> 
  bind_cols(st_coordinates(sites))

#tmap_save(sites_clust_map, "data_preproc/sites_clust_map.html")

# DMI data

fnames <- as.list(paste(
  "O:\\Tech_AGRO\\Jornaer\\Franca\\N_conc\\10kmgridnew_geusprec22\\", 
  unique(sites$DMIGRIDNUM[which(!is.na(sites$DMIGRIDNUM))]),
  "8022", 
  ".dwf", 
  sep = ""))

names(fnames) <- unique(sites$DMIGRIDNUM[which(!is.na(sites$DMIGRIDNUM))])

dmi_list <- lapply(fnames, read.fwf, 
                           sep ="\t",
                           skip = 36,
                           widths= c(8, 6, 5, 8,8 ,10),
                           col.names= 
                        c("Year","Month","Day","GlobRad", "AirTemp","Precip"))

dmi_table <- do.call(rbind,dmi_list)

dmi_table <- dmi_table |> 
  bind_cols("dmi"=row.names(dmi_table)) |> 
  separate(dmi,c('DMIGRIDNUM','idx')#, sep="." 
           ) |> 
  mutate(date=make_date(year=Year, month=Month, day=Day),
         merge_dmi=fct_cross(as.factor(DMIGRIDNUM), as.factor(date), sep = "_")
         ) |> 
  select(date, DMIGRIDNUM, GlobRad, AirTemp,Precip, merge_dmi)
  
# dwf_try <- read.fwf(
#       fnames[]
#       ,
#       #row.names = FALSE,
#       #header = FALSE,
#       row.names=
#       sep ="\t",
#       skip = 36,
#       widths=
#         c(8, 6, 5, 8,8 ,10),
#       col.names= c("Year","Month","Day","GlobRad", "AirTemp","Precip")
#       )


conc_nov_site <- merge(conc_nov_dep,
                       sites,
                       by.x = 'ident',
                       by.y = 'strno',
                       all.x = TRUE) |>
  mutate(harvest_year = ifelse(month < 4, year - 1, year)) |>
  mutate(merge_id = fct_cross(as.character(ident),
                              as.character(harvest_year), #as.character(year)
                              sep = "_"),
                              date = 
                                lubridate::make_date(day = day, 
                                                     month = month, 
                                                     year = year)) |> 
  mutate(obs_id = paste(as.factor(ident), date, sep = "_")) |> 
  mutate(merge_dmi=fct_cross(as.factor(DMIGRIDNUM), as.factor(date), sep = "_"))

  tm_shape(st_as_sf(conc_nov_site))+
  tm_dots()
  
  
  conc_nov_site_dmi <- merge(conc_nov_site,
                             dmi_table,
                             by='merge_dmi',
                             all.x = TRUE) |> 
  select(!c(date.y,DMIGRIDNUM.y))
  

write.table(conc_nov_site,"data_preproc//concentrationN_daily_raw_FGK.txt", sep = "\t")

#conc_nov_site_dmi <- read.table("data_preproc//concentrationN_daily_raw.txt", sep = "\t")


  
  # filter without loops and years before 1991----
  
  conc_nov_site_dmi_dep <- conc_nov_site_dmi |> 
  filter(!site_eng == '\\Skara\\') |>
  filter(harvest_year>=1991) |>
  filter(!between(ident,102,608))|>
  filter(!site_eng =='\\Arslev\\') |> 
  filter(!site_eng =='\\Agervig\\') |> 
  filter(!c(site_eng =='\\Abenra\\' & harvest_year < 1993)) |> 
  filter(!c(site_eng =='\\Askov\\' & harvest_year < 1995)) |> 
  filter(!c(site_eng =='\\Silstrup\\' & harvest_year<= 1994)) |> 
  #rename(date=date.x) |> 
  mutate(date=make_date(year=year, month=month, day=day),
         site_eng=as.character(site_eng))


table(conc_nov_site_dmi_dep$site_eng,conc_nov_site_dmi_dep$harvest_year) |> 
  as.data.frame() |>
  mutate_all(~na_if(., 0)) |> 
  ggplot( aes(x = Var2, y = Var1, fill = Freq)) +
  geom_tile(color = "gray") +
  scale_fill_gradientn(name = "n",
                       na.value = 'black',
                       colors = brewer.pal(5,"Purples")) +
  geom_text(aes(label = paste(Freq)), color = "black", size = 2) +
  scale_x_discrete(name = "harvest year") +
  scale_y_discrete(name = "Site")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

explo_conc_plot <- conc_nov_site_dmi_dep |>
  ggplot(aes(x=date,y=meankonc))+
  geom_point()+
  #geom_smooth(se=FALSE)+
  scale_x_date(date_breaks = "1 years", 
               date_labels = "%Y",
               date_minor_breaks = "1 month")+
  facet_grid(site_eng~.,scales = "free_y"#,switch = "y"
             )+
  theme(strip.text.y = element_text(angle=0),
        axis.ticks.x = element_text(angle = 90))+
  theme_bw() 
  
ggsave("explo_conc_plot.jpg", 
       explo_conc_plot, 
       height =100, 
       width = 50,
       units = "cm")

missing_sites <- 
conc_nov_site |> 
  filter(is.na(site_eng)) |> 
  select(ident) |>
  unique() #|>
  #summarize(n=n())

# Drain ----

wea <- read.table("data_raw/wea_txt.txt", sep = "\t", header = T) |>
  mutate(date = lubridate::make_date(day = mday, month = month, year = year)) |>
  rename('sted' = 'eksponr') |>
  select(!c(saedidentnr,
            drain,
            Intpol_newconc,
            udvaskday,
            sumudvask,
            Maaltkonc,
            sumafstroem)) |> 
  mutate(
    obs_id = paste(sted, date, sep = "_"),
  
    drain_day = ifelse(afstroemning > 0.3, 1, 0),
    leach_year = ifelse(month<8,
                               paste(year-1),
                               paste(year)),
    harvest_year = ifelse(month<4, 
                          paste(year - 1), 
                          paste(year))
    ) |>
    group_by(sted,leach_year) |>
    arrange(date) |>
    mutate(afstro_sum=cumsum(afstroemning),
           e_sum=cumsum(ea))

#wea[,c("afstro_sum","e_sum")] |> cor()

#plot(wea[,c("afstro_sum","e_sum")])

#hist(wea$afstroemning)

# wea |> ggplot(aes(x=afstroemning))+
#   geom_histogram(aes(position="identity"), colour = "black",bins=80)+
#   #stat_density(geom = "line", aes(colour = "bla"))+
#   ylab("Count")+ xlab("daily afstroemning (mm)" )+
#   geom_rug() +
#   theme_bw()

# #temporal ggplot
# 
# wea |>
#   #filter(Id<20) |>
#   ggplot(aes(x = month, y = afstroemning)) +
#   geom_point() +
#   geom_smooth(method = "loess", se = F) +
#   #coord_cartesian(ylim = c(0,0.9)) +
#   theme(panel.grid = element_blank()) +
#   facet_wrap(~year, nrow=4)+
#   #scale_x_continuous(breaks = seq(1,12,1))+
#   theme_bw()


# Merge mess ----
c_mess_nov <- merge(
  wea,
  conc_nov_site,
  by = 'obs_id',
  .name_repair = "unique",
  suffixes = c("", ".y"), 
  all.x = TRUE
) |> select(
  "obs_id",
  "sted",
  "year",
  "month",
  "day",
  "meankonc",
  "site_eng",
  "geometry",
  "harvest_year",
  "merge_id",
  "date",
  "ep",
  "ea",
  "afstroemning",
  "drain_day",
  "leach_year",
  "afstro_sum"
) 

c_mess_nov <- c_mess_nov |> 
  mutate(
    day_harv = as.numeric(as.Date(date) - 
               as.Date(paste(harvest_year, "04", "01", sep = "-"))),
    day_leach = as.numeric(as.Date(date) - 
            as.Date(ifelse( month < 8,
  paste(year - 1, "08", "01", sep ="-"),
  paste(year, "08", "01", sep = "-")))
    ),
  day=day(as.Date(date))
  )

c_mess_nov |> write.table("data_preproc/c_mess_nov.txt" ,sep="\t")

# Explore ----
#conc_nov_site |> 

c_mess_nov|> 
  #dplyr::filter(site_eng =="\"Flakkebjerg\"") |> 
  ggplot(aes(x = day_harv, y = meankonc)) +
  geom_point() +
  geom_smooth(method = "loess", se = F) +
  theme(panel.grid = element_blank()) +
  facet_wrap(~harvest_year, nrow=4,scales = "free_y")+
 # scale_x_continuous(breaks = seq(1,12,1))+
  theme_bw()


c_mess_nov|> 
  ggplot(aes(x = afstro_sum, y = meankonc)) +
  geom_point() +
  geom_smooth(method = "loess", se = F) +
  theme(panel.grid = element_blank()) +
  facet_wrap(~site_eng, nrow=4,scales = "free")+
  theme_bw()





# Master data ----

master_b <- read_excel(
    "data_raw/masterNLESS_Franka100822.xls"
    , sheet = "master_engl"
    #,.name_repair = "minimal"
 )|> 
  # select(Id,harvest_year,N_leaching,
  #        WC,N_fix,MP,WP,M,W,N_mineral_spring,N_mineral_autuomn,JB)

master_a <- read_excel(
  "data_raw/Scenarier20190909B4_231120_sendt_GBM_anonym_sendtdata_med_id_vers_medNkonc Franca_updated_cdb211122.xls"
  #"data_raw/masterNLESS_Franka100822.xls"
  , sheet = "data_detailed_eng" #
  #"master_engl2"
  #,.name_repair = "minimal"
  ) #|> 
  #select(Id,harvest_year,MP,WP,M,N_mineral_spring, N_mineral_autuomn)

master_a1 <- read_excel(
  "data_raw/masterNLESS_Franka100822.xls"
  #"data_raw/masterNLESS_Franka100822.xls"
  , sheet = "Data_udleveret_MasterNLES5" #
  #"master_engl2"
  #,.name_repair = "minimal"
) |> select(colnames(master_a))

master_dif <- master_b[!(master_b$Id %in% master_a$Id),]

table(master_dif$Id)

master <- master_b |> 
  #bind_rows(master_a,master_b)|>
  mutate(merge_id=fct_cross(as.character(Id),
                                as.character(harvest_year),#year
                                sep="_")) |> #select(!year) 
  unique() |> select(!c(harvest_year,Id))


c_mess_master <- merge(conc_nov_site_dmi_dep ,
                       master,
                       by='merge_id',
                       all.x = TRUE
) |> unique() 

c_mess_master_complete <- c_mess_master |> filter(!is.na(M))

summary(c_mess_master_complete)

#saveRDS(c_mess_master_complete, "c_mess_master_complete.RDS") 

#|> 
  # mutate(
  #   start_date=ymd(ifelse(month<4, 
  #                     paste(year-1, "04", "01",sep="-"), 
  #                     paste(year, "04", "01",sep="-")))#, 
  #   #end=ifelse(month<4, make_date(month = 3, day=31, year = year), make_date(month = 3, day=31, year = year+1))
  #   ) 
  # |>  
  # mutate(
  #   day_harv=as.numeric(as.Date(date)-as.Date(paste(harvest_year.x, "04", "01",sep="-"))),
  #   day_leach=as.numeric(as.Date(date)-as.Date(ifelse(month<8,
  #                                                     paste(year-1, "08", "01",sep="-"),
  #                                                     paste(year, "08", "01",sep="-")
  #   ))))


c_mess_master_problems <- c_mess_master |>  
  filter(is.na(harvest_year.y)) |> 
  select(merge_id) #|> 
  #unique()

writexl::write_xlsx(c_mess_master, 
                    "c_mess_master.xlsx")

c_mess_measure_complete <-
  c_mess_master |>  
  filter(!is.na(harvest_year.y)) |> 
  filter(measure_grp == T)


write.table(c_mess_master,"c_mess_master.txt", sep="\t")

c_mess_master |> 
  ggplot(aes(x = afstro_sum, y = meankonc)) +
  geom_point() +
  geom_smooth(method = "loess", se = F) +
  theme(panel.grid = element_blank()) +
  facet_wrap(~site_eng, nrow=4,scales = "free")+
  theme_bw()
