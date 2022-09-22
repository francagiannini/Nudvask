library(tidyverse)
library(lubridate)
library(readxl)

## dongs ---- 
# several tratments/experimentals 

foulum_conc <- read.table(
  "data_raw/Foulum_dong_mod.txt", 
  sep="\t", 
  #skip = 3,
  header = T,
  row.names = NULL
) |> mutate(date=lubridate::make_date(day=day, month=month, year=year),
            site='foulum') |>
  pivot_longer(cols=c("Maize","baresoilchem.","baresoilmech.","barley","festulolium",
                      "grassmixtureDK","M.xgiganteus","Opt.rot.1","Opt.rot.2",
                      "Opt.rot.3","Opt.rot.4","triticale"),
               names_to = 'LandUse',
               values_to = 'knoc')



jyndevad_conc <- read.table(
  "data_raw/jyndevad_data_dong.txt", 
  sep="\t", 
  skip = 4#,
  #header = T
)

colnames(jyndevad_conc) <- c(
  'day',	'month',	'year',	
  'M.sinensis_1', 'M.sinensis_2',
  'Cocksfoot_1','Cocksfoot_2',
  'Maize_Opt.rot.1_1', 'Maize_Opt.rot.1_2',
  'Elefantgr_1','Elefantgr_2'
)

jyndevad_conc <- jyndevad_conc |>
  mutate(date=lubridate::make_date(day=day, month=month, year=year),
         site='jyndevad') |> 
  pivot_longer(cols=c(
    'M.sinensis_1', 'M.sinensis_2',
    'Cocksfoot_1','Cocksfoot_2',
    'Maize_Opt.rot.1_1', 'Maize_Opt.rot.1_2',
    'Elefantgr_1','Elefantgr_2' ),
    names_to = 'LandUse',
    values_to = 'knoc')


#[,-1]
## nkonc ----

guldborg_conc <-read.table("data/guldborg_nkonc2015_2018.txt",
                           sep = "\t", header = T) |> 
  mutate(No3_N=as.numeric(ifelse(No3_N<0,0,No3_N)),
         date=dmy(date)) |> 
  rename('knoc' ='No3_N')

jyndevad_conc_2 <-read.table("data/jyderup_nkonc2017_2018.txt",
                             sep = "\t", header = T)|> 
  mutate(No3_N=as.numeric(ifelse(No3_N<0,0,No3_N)),
         date=dmy(date)) |> 
  rename('knoc' ='No3_N')

virk_conc <- read.table("data/Virk_N_nkonc2015_2018.txt",
                        sep = "\t", header = T)|> 
  mutate(no3n=as.numeric(ifelse(no3n<0,0,no3n)),
         date=lubridate::make_date(day=Dato, month=mm, year=yy)
  ) |> 
  rename('knoc' ='no3n',
         'sted' ='NLES5')

ytteborg_conc <- read.table("data/Ytteborg_nkonc281118.txt",
                            sep = "\t", header = T) |> 
  mutate(date=lubridate::make_date(day=dd, month=mm, year=yy))

nknoc <- read.table("data/nkonc_mean217.txt",
                    sep = "\t", header = T) |> 
  mutate(knoc=as.numeric(ifelse(knoc<0,0,knoc))) |> 
  drop_na(knoc)|> 
  mutate(date=lubridate::make_date(day=day, month=month, year=year))

# summary(nknoc)
# summary(virk_conc)
# table(nknoc$sted,nknoc$year)



## stednavn site year and coordinate ----

names1 <- read_excel("data_raw/masterNLESS_Franka100822.xls"
                     , sheet = "Fordelings_data"
                     #,.name_repair = "minimal"
) |> select(strno,StedNavn)#, Indb_aar, brug, forfrugt, afgrode)|> unique()

write.table(names1, "names1.txt")


names2 <- read_excel("data_raw/masterNLESS_Franka100822.xls",
                     sheet = "missing_location") |> 
  select(strno, StedNavn)|> unique()

names <- bind_rows(names1, names2) |> unique()
