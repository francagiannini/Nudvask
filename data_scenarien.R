library(tidyverse)
library(lubridate)
library(readxl)
library(utils)
library(sf)
library(tmap)
library(tidyverse)


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
  filter(meankonc<150)

conc_nov |> ggplot(aes(y=ident,x=year))+geom_point()

summary(conc_nov)
plot(conc_nov$meankonc)
abline(100,0, col=2)

# Sites -----

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
  ) |> select(!c(StedNavn,sites_dk))


conc_nov_site <- merge(conc_nov,
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
  mutate(obs_id = paste(as.factor(ident), date, sep = "_"))


conc_nov_site |> filter(is.na(site_eng)) |> 
  select(ident) |> 
  unique() |> 
  summarize(n=n())

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
                               paste(year)
    )) |>
    group_by(sted,leach_year) |>
    arrange(date) |>
    mutate(afstro_sum=cumsum(afstroemning))


#hist(wea$afstroemning)

# wea |> ggplot(aes(x=afstroemning))+
#   geom_histogram(aes(position="identity"), colour = "black",bins=80)+
#   #stat_density(geom = "line", aes(colour = "bla"))+
#   ylab("Count")+ xlab("daily afstroemning (mm)" )+
#   geom_rug() +
#   theme_bw()

# #temporal ggplot

wea |>
  #filter(Id<20) |>
  ggplot(aes(x = month, y = afstroemning)) +
  geom_point() +
  geom_smooth(method = "loess", se = F) +
  #coord_cartesian(ylim = c(0,0.9)) +
  theme(panel.grid = element_blank()) +
  facet_wrap(~year, nrow=4)+
  #scale_x_continuous(breaks = seq(1,12,1))+
  theme_bw()


# Merge mess ----

c_mess_nov <- merge(
  conc_nov_site,
  wea,
  by = 'obs_id',
  .name_repair = "unique",
  suffixes = c("", ".y")
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
) |> mutate(day_harv = as.numeric(as.Date(date) - as.Date(
  paste(harvest_year, "04", "01", sep = "-")
)),
day_leach = as.numeric(as.Date(date) - as.Date(ifelse(
  month < 8,
  paste(year - 1, "08", "01", sep =
          "-"),
  paste(year, "08", "01", sep =
          "-")
))))


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

master <- read_excel("data_raw/Scenarier20190909B4_231120_sendt_GBM_anonym_sendtdata_med_id_vers_medNkonc Franca_updated_cdb211122.xls"
  #"data_raw/masterNLESS_Franka100822.xls"
  , sheet = "data_det_eng_2"#"master_engl2"
                     #,.name_repair = "minimal"
) |>  mutate(merge_id=fct_cross(as.character(Id),
                                as.character(harvest_year),#year
                                sep="_")) |> select(!year)


c_mess_master <- merge(c_mess_nov,
                       master,
                       by='merge_id',
                       all.x = TRUE
) |> 
  # mutate(
  #   start_date=ymd(ifelse(month<4, 
  #                     paste(year-1, "04", "01",sep="-"), 
  #                     paste(year, "04", "01",sep="-")))#, 
  #   #end=ifelse(month<4, make_date(month = 3, day=31, year = year), make_date(month = 3, day=31, year = year+1))
  #   ) 
  # |>  
  mutate(
    day_harv=as.numeric(as.Date(date)-as.Date(paste(harvest_year.x, "04", "01",sep="-"))),
    day_leach=as.numeric(as.Date(date)-as.Date(ifelse(month<8,
                                                      paste(year-1, "08", "01",sep="-"),
                                                      paste(year, "08", "01",sep="-")
    ))))


c_mess_master_problems <- c_mess_master |>  
  filter(is.na(harvest_year.y)) #|> 
  #select(merge_id) |> 
  #unique()

writexl::write_xlsx(c_mess_master_problems, "c_mess_master_problems.xlsx")

c_mess_measure_complete <- c_mess_master |>  filter(!is.na(harvest_year.y)) |> filter(measure_grp==T)


write.table(c_mess_master,"c_mess_master.txt", sep="\t")


