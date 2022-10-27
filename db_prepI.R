## packages ----

library(tidyverse)
library(lubridate)
library(readxl)
library(utils)
getwd()

# Concentration marginals ----

### dongs ----
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
               values_to = 'konc')



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
    values_to = 'konc')


#[,-1]
## nkonc ----

guldborg_conc <-read.table("data_raw/guldborg_nkonc2015_2018.txt",
                           sep = "\t", header = T) |> 
  mutate(No3_N=as.numeric(ifelse(No3_N<0,0,No3_N)),
         date=dmy(date)) |> 
  rename('konc' ='No3_N') |> 
  select(c(sted,konc,date))

jyndevad_conc_2 <-read.table("data_raw/jyderup_nkonc2017_2018.txt",
                             sep = "\t", header = T)|> 
  mutate(No3_N=as.numeric(ifelse(No3_N<0,0,No3_N)),
         date=dmy(date)) |> 
  rename('konc' ='No3_N')|> 
  select(c(sted,konc,date))

virk_conc <- read.table("data_raw/Virk_N_nkonc2015_2018.txt",
                        sep = "\t", header = T)|> 
  mutate(no3n=as.numeric(ifelse(no3n<0,0,no3n)),
         date=lubridate::make_date(day=Dato, month=mm, year=yy)
  ) |> 
  rename('konc' ='no3n',
         'sted' ='NLES5')|> 
  select(c(sted,konc,date))

ytteborg_conc <- read.table("data_raw/Ytteborg_nkonc281118.txt",
                            sep = "\t", header = T) |> 
  mutate(date=lubridate::make_date(day=dd, month=mm, year=yy))|> 
  select(c(sted,konc,date))

nkonc <- read.table("data_raw/nkonc_mean217.txt",
                    sep = "\t", header = T) |>
  rename('konc' ='knoc') |> 
  drop_na(konc)|> 
  mutate(date=lubridate::make_date(day=day, month=month, year=year)) |>
  select(c(sted,konc,date))|> 
  
  rbind(ytteborg_conc,virk_conc,jyndevad_conc_2,guldborg_conc) |>
  mutate(konc=as.numeric(ifelse(konc<0,0,konc)))


#nkonc has sted from 1 to 2445

# summary(nkonc)
# summary(virk_conc)
# table(nkonc$sted,nkonc$year)



# Stednavn site year and coordinate ----

names1 <- read_excel("data_raw/masterNLESS_Franka100822.xls"
                     , sheet = "Fordelings_data"
                     #,.name_repair = "minimal"
) |> select(strno,StedNavn)#, Indb_aar, brug, forfrugt, afgrode)|> unique()


names2 <- read_excel("data_raw/masterNLESS_Franka100822.xls",
                     sheet = "missing_location") |> 
  select(strno, StedNavn)|> unique()

names_data <- bind_rows(names1, names2) |> unique()

write.table(names, "names.txt", sep="\t")


names_sum <- names |> group_by(StedNavn) |> 
  summarize(n=n(),
            minID=min(strno),
            maxID=max(strno))

write.table(names_sum,"names_sum.txt", sep="\t")

getwd()

# Percolation Calibration ----

# leach_day_kal <- read.table("data_raw/output_leach_day_kalibrering.tab", 
#                             sep = "\t", 
#                             header = TRUE, 
#                             dec=".",
#                             fill = TRUE,
#                             check.names = FALSE,
#                             row.names = NULL#,
#                             #skip = 1,
#                             #na.strings = "NA"
#                             ) |>
#   select_if(~sum(!is.na(.)) > 0)



# wea <- read.fwf(
#     "data_raw/output_leach_day_kalibrering.tab",
#     #row.names = FALSE,
#     #header = FALSE,
#     sep ="\t",
#     skip = 1,
#     widths=#137
#       c(9, 8,
#              5, 6, 5,
#              8, 8, 8, 8,
#              10, 10, 10, 10, 11,
#              11)
#     )
# 
# 
# 
# colnames(wea) <- c(
#   'eksponr',
#   'saedidentnr',
#   'year',
#   'month',
#   'mday',
#   'nedboer',
#   'ep',
#   'ea',
#   'afstroemning',
#   'drain',
#   'Intpol_newconc',
#   'udvaskday',
#   'sumudvask',
#   'Maaltkonc',
#   'sumafstroem'
# )
# 
# #check
# 
# head(wea)
# summary(wea)
# dim(wea)
# 
# 
# write.table(wea, "data_raw/wea_txt.txt", sep = "\t")

wea <- read.table("data_raw/wea_txt.txt",sep = "\t",header = T) |> 
  mutate(date=lubridate::make_date(day=mday, month=month, year=year)) |> 
  rename('sted' ='eksponr') |> 
  select(!c(saedidentnr, 
            drain,
            Intpol_newconc,
            udvaskday,
            sumudvask))

head(wea)

hist(wea$afstroemning)

wea |> ggplot(aes(x=afstroemning))+
  geom_histogram(aes(position="identity"), colour = "black",bins=80)+
  #stat_density(geom = "line", aes(colour = "bla"))+
  ylab("Count")+ xlab("daily afstroemning (mm)" )+
  geom_rug() +
  theme_bw()

# summary(wea$afstroemning)
# summary(wea[which(wea$afstroemning>50),])
# 
# 
# hist(wea$Maaltkonc)
# summary(wea$Maaltkonc)
# 
# summary(wea[which(wea$Maaltkonc>200),])

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

wea_c <- wea |> mutate(
  # meas_day_inter= Maaltkonc/Intpol_newconc,
  # meas_day=ifelse(Maaltkonc/Intpol_newconc>0,TRUE, FALSE),
  obs_id = paste(sted,date, sep = "_"),
  drain_day = ifelse(afstroemning >0, 1,0) 
)

# Concentration ----

#' conc <- read.fwf(
#'   "data_raw/daily_leaching.txt",
#'   #row.names = FALSE,
#'   #header = FALSE,
#'   sep ="\t",
#'   skip = 1,
#'   widths=#137
#'     c(5,
#'       6, 6, 4, 10,
#'       8, 8,
#'       10,
#'       8, 8, 8, 8, 8)
#' )
#' 
#' colnames(conc) <- c('saedidentnr1',
#'                     'year',
#'                     'month',
#'                     'mday',
#'                     'year_juliandayprop',
#'                     'drain',
#'                     'udvaskday',
#'                     'newconc',
#'                     'n_kooncstart',
#'                     'n_koncslut',
#'                     'sumdrain',
#'                     'sumudleach',
#'                     'weigth_year'
#' )
#' 
#' conc_m <- conc |>
#'   mutate(date=lubridate::make_date(day= mday, month=month, year=year)) |>
#'   group_by(n_kooncstart,saedidentnr1,year) |>
#'   mutate(measure_grp=ifelse(min(date)==date,T,F))
#'   #mutate(id_grp=ifelse(n_kooncstart!=n_koncslut & n_kooncstart==newconc ,TRUE,FALSE))
#' 
#' 
#' obs_sted_year <-
#'  table(conc_m$measure_grp,fct_cross(
#'      as.character(conc_m$saedidentnr1),
#'      as.character(conc_m$year))
#'    ) |> as.data.frame() |> filter(Var1==TRUE & Freq<4)
#' 
#' #'2080.2003' <-  conc_m |> filter(saedidentnr1==2080 & year==2003)  
#'   
#' data.frame(obs_sted_year)
#' 
#' write.table(conc_m, "data_raw/conc_txt.txt", sep = "\t")


conc_c <- read.table("data_raw/conc_txt.txt", sep = "\t",header = T)|> 
  rename('sted'="saedidentnr1") |> 
  mutate(
  obs_id = paste(sted,date, sep = "_")
)

head(conc_c)

length(wea_c$sted %in% conc_c$sted)

# Merging ----

wea_c |> 
  ggplot(aes(x = month, fill = as.factor(drain_day))) +
  geom_bar()+
  scale_x_continuous(breaks = seq(1,12,1))+
  theme_bw()


hist(wea_c$drain_day)

table(wea_c$meas_day)
summary(wea_c$meas_day_inter)
head(wea_c)


c_mess <- merge(wea_c, 
                conc_c, 
                by = 'obs_id',
                .name_repair="unique",
                suffixes = c("",".y")
                #incomparables = NA#, 
                #all.x = TRUE
                ) |>
  mutate(check_drain = drain - afstroemning)|> 
  select('obs_id',
         'sted',
         'year','month','mday','year_juliandayprop',
         'nedboer',
         'ep',
         'ea', 
         'afstroemning',
         'date',
         'drain_day',
         'newconc',
         'sumdrain',
         'measure_grp'
         ) #|> rename('sumdrain'='sumdrain.1')


c_mess |> write.table("data_preproc/my_mess_nless.txt" ,sep="\t")


wea_c[which((
  interaction(wea_c$sted, wea_c$year) %in% interaction(conc_c$sted, conc_c$year)
) == FALSE,
arr.ind = TRUE),]

# ***Start bis***------ 

c_mess <- read.table("data_preproc/my_mess_nless.txt",sep="\t",header = TRUE)

##summary(c_mess$check_drain)

# c_problem <- c_mess |> filter(is.na(sted)) 

# table_check <- as.data.frame(table(c_problem$sted.x,c_problem$year.x)) |> 
#   filter(Freq>0)

# unique(ytteborg_conc$sted)
# 
# wea_ytteborg <- wea_c |> filter(
#   saedidentnr %in% unique(ytteborg_conc$sted))

## sites ----

library(sf)
library(tmap)

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
            )

tmap_mode("view")

sites |>  select(!c(strno,StedNavn)) |> unique() |> 
tm_shape() + 
  tm_dots()+
  tm_text("site_eng", size = 1)

c_mess_site <- merge(c_mess,
                     sites,
                     by.x='sted',
                     by.y='strno'#,
                     #all.x = TRUE
                     )


## first explore -----
c_mess_measure <- c_mess_site |> 
  filter(measure_grp==TRUE)

c_mess_measure |> ggplot()+
  geom_histogram(aes(x=log(newconc),position="identity"), 
                 colour = "black"#,bins=20
                 )+
  # geom_histogram(aes(x=newconc,position="identity"), 
  #                colour = "white"#,bins=20
  # )+
  #stat_density(geom = "line", aes(colour = "bla"))+
  ylab("Count")+ xlab("daily concentration" )+
  geom_rug() +
  theme_bw()

c_mess_site |> 
#c_mess_measure |> 
  #filter(Id<20) |> 
  ggplot(aes(x = month, y = newconc)) +
  geom_point() +
  geom_smooth(method = "loess", se = F) +
  #coord_cartesian(ylim = c(0,0.9)) +
  theme(panel.grid = element_blank()) +
  facet_wrap(~site_eng, nrow=4)+
  #scale_x_continuous(breaks = seq(1,12,1))+
  theme_bw()


c_mess_measure |> ggplot(aes(x = year, y = sted))+
  geom_count(col="tomato3", show.legend=F)+
  scale_x_continuous(breaks=seq(1989,2018,1))+
  #scale_y_continuous(breaks=unique(c_mess_measure$sted))+
  theme_bw()

library(lme4)

VCA<-lme4::lmer(
  newconc~1+(1|month)+(1|year)+(1|site_eng)+(1|sted)+(1|Reference)
  ,na.action=na.omit
  ,REML=T
  #,control=lmerControl(optimizer="bobyqa")
  ,data=c_mess_measure)

#summary(VCA_gral)

vca <- as.data.frame(VarCorr(VCA))

vca |> group_by(grp) |> summarise(
  varprop = vcov / sum(vca$vcov) * 100) |> arrange(
    varprop, grp) 



table(c_mess_site_problem$sted)


master <- read_excel("data_raw/masterNLESS_Franka100822.xls"
                     , sheet = "master_engl2"
                     #,.name_repair = "minimal"
)
