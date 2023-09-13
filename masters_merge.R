
library(tidyverse)


master_b <- readxl::read_excel(
  "data_preproc/master_b.xlsx", sheet = "master_b_priority") |> 
  mutate(merge_id = fct_cross(as.character(Id),
                              as.character(harvest_year),
                              sep = "_")) #|> 
  # select( 
  #   # identification
  #   #"Id",	"harvest_year",	
  #   "merge_id",
  #   # Site and spatial id 
  #   #"site_eng","DMIGRIDNUM","X_CENTRE","Y_CENTRE","X","Y",
  #   # crop sequence
  #   "WC",	"N_fix",	"MP",	"WP",	"M",	"W",	
  #   #soil
  #   "clay", 
  #   #"JB",
  #   #annual leaching
  #   #"N_leaching",
  #   # N inputs and stage
  #   "N_mineral_spring",	"N_mineral_autuomn"	,	
  #   "N_min_year-1",	"N_min_year-2",	
  #   "N_fix_year",	"N_fix_year-1",	"N_fix_year-2",	
  #   "N_org_year",	"N_org_year-1",	"N_org_year-2",	
  #   "N_from_grassing_animals",	"N_topsoil")



master_a <- readxl::read_excel(
  "data_preproc/master_a.xlsx", sheet = "master_a_priority") |> 
  mutate(merge_id = fct_cross(as.character(Id),
                              as.character(harvest_year),
                               sep = "_")) #|> 
  # select( 
  #   # identification
  #   #"Id",	"harvest_year",	
  #   merge_id,
  #   # Site and spatial id 
  #   #"site_eng","DMIGRIDNUM","X_CENTRE","Y_CENTRE","X","Y",
  #   # crop sequence
  #   #WC,	N_fix,	
  #   Mfu,Vfu,	Maw,	Vaw,	
  #   #soil
  #   jbnr, ler,#TN,
  #   #annual leaching
  #   #"N_leaching",
  #   # N inputs and stage
  #   MinH_foraarU, # Mineral N In the spring, the Leaching Year
  #   #MinH_eftU, # MinH_eftU Mineral N In autumn
  #   
  #   N_udb,
  #   N_Org,
  #   N_min,
  #   N_input_afgr_derester,
  #   # plot(master_a$N_udb_orgnefteraar,master_a$N_udb)
  #   #N_udb_orgnefteraar,
  #   
  #   #nlevelMin1, # Mineral N In previous year (Both spring and autumn)
  #   #nlevelMin2, # Mineral N For 2 years before the leaching year (Both spring and autumn)
  #   
  #   #NlevelGod0, # N supplied as organic N in the leaching year
  #   #NlevelGod1, # N supplied as organic N in the year prior to the leaching year
  #   #NlevelGod2, # N supplied as organic N two years prior to the leaching year
  #   Nlevel_0_totN#, # Nlevel0
  # ) 

master_merge <- merge(master_b,master_a,by="merge_id",all = TRUE) |> 
  select(!contains(".y"))
colnames(master_merge)<-gsub(".x","",colnames(master_merge)) 

Mode <- function(x, na.rm = FALSE) {
  # it takes two areguments x, and na.rm
  if(na.rm){ #if na.rm is false it means no need to remove NA values
    x = x[!is.na(x)]
  }
  
  valx <- unique(x)
  return(valx[which.max(tabulate(match(x, valx)))])
}
  
  master_merge <- master_merge |> 
  group_by(site_eng) |>
  mutate(jb_g=Mode(jbnr)) |> ungroup()
  
  table(master_merge$jb_g)

#table(master_merge$site_eng,master_merge$jbnr)

master_merge|> 
  select(is.numeric)  |> 
  cor(use = "pairwise.complete.obs") |> 
  as.data.frame() |> 
  select(#WC,MP,	WP,	M,	W
    "N_mineral_spring",	"N_mineral_autuomn"	,	
     "N_min_year-1",	"N_min_year-2",	
      "N_f_year",	"N_f_year-1",	"N_f_year-2",
      "N_org_year",	"N_org_year-1",	"N_org_year-2",
      "N_from_grassing_animals",	"N_topsoil","clay"
    )|> as.matrix() |> 
  corrplot::corrplot()


  master_merge <-
  master_merge|> 
  mutate(WC=ifelse(is.na(WC),ifelse("Va"==6,1,2),WC),
         MP=ifelse(is.na(MP),VF, MP),
         WP=ifelse(is.na(WP),MF,WP),	
         M=ifelse(is.na(M),MA,M),	
         W=ifelse(is.na(W),Va,W),
         jbnr=ifelse(is.na(jbnr),jb_g,jbnr),
         clay=ifelse(is.na(clay),ler,clay),
         N_topsoil=ifelse(is.na(N_topsoil),TotN1,N_topsoil),
         N_org_year=ifelse(is.na(N_org_year),N_Org,N_org_year),
         ggamma=ifelse(is.na(Gamma),ifelse(jbnr>3,2,1), Gamma)
  )|> 
  select("merge_id",#"Id","harvest_year",
         
         "WC","MP",	"WP",	"M","W",
         
         "N_mineral_spring",	"N_mineral_autuomn"	,	
         "N_min_year-1",	"N_min_year-2",	
         "N_f_year",	"N_f_year-1",	"N_f_year-2",
         "N_org_year",	"N_org_year-1",	"N_org_year-2",
         "N_from_grassing_animals",	
         
         "N_topsoil","clay","ggamma","jbnr"
         ) |> 
    separate(merge_id,c("Id","harvest_year"),sep="_", remove = FALSE)
  

summary(master_merge)

write.table(master_merge, "data_preproc/master_merge_3107.txt", sep = "\t")

writexl::write_xlsx(master_merge,
                    "data_preproc/master_merge_3107.xlsx",# sep = "\t",
                    col_names = TRUE,
                    format_headers = TRUE)
