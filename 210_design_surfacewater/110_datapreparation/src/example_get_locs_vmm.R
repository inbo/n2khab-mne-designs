#example file get_locs_vmm


hab3260_integration= st_read("../110_datapreparation/data/20_output/hab3260_integration.shp")

vmmchem <- inbodb::connect_inbo_dbase(database_name = "D0113_00_VMMData")


# parameter definition:

#con = database connection

#bbox = c(xmin, xmax, ymin, ymax) bbox defined by lambert 72 coordinates.

#stream (character) = river name (in dutch) (eg "Abeek"). Case insensitive

#guess (TRUE/FALSE) = define if stream name is exact or a string of the full river name (eg stream = "Abeek" guess = F returns no data,
#because Abeek is defined as "Abeek - Lossing" in database & geodatabase. Guess = T returns all data with "Abeek" string in this example, including "Abaak - Lossing")

#parameter (character)

#collect_HT3260 (TRUE/FALSE) = if TRUE, returns a data frame from vmm database for river segments that are HT3260 habitat. If habitat type of a river segment changes over time,
# data is only collected for the period where river segment is determined as HT3260. If FALSE, a tibble of all vmmdata for given stream, bbox and variable will be returned without
#linkage to HT3260 geodatabase.

#geodatabase (sf object) = geodatabase with HT3260 data
all_data=get_locs_vmm(con = vmmchem,collect_HT3260=T, geodatabase = hab3260_integration)
vmm_data_NO3=get_locs_vmm(con = vmmchem,collect_HT3260=T,parameter = c("NO3-"), geodatabase = hab3260_integration)
testset=get_locs_vmm(con = vmmchem,stream= "Nete",guess = T, collect_HT3260=F)%>% collect
write.csv(vmm_data_NO3,"./data/20_output/vmm_data_NO3.csv")
vmm_data_oPO4=get_locs_vmm(con = vmmchem,collect_HT3260=T,parameter = c("oPO4"), geodatabase = hab3260_integration)
write.csv(vmm_data_oPO4,"./data/20_output/vmm_oPO4.csv")
vmm_data_NO3=get_locs_vmm(con = vmmchem,collect_HT3260=T,parameter = c("NO3-"), geodatabase = hab3260_integration)
write.csv(vmm_data_NO3,"./data/20_output/vmm_data_NO3.csv")


tbl(vmmchem,"MetingFysicoChemieMeting") %>% glimpse

geodata= hab3260_integration %>% select(-c(LENGTE)) %>% rownames_to_column()
vmm_data=get_locs_vmm(con = vmmchem)%>% glimpse()
vmm_locations = vmm_data %>% select (c(loc_code, x, y, vhas_code, river_name))%>% distinct() %>% collect
vmm_mismatch = vmm_locations %>%
st_as_sf(coords= c("x","y"),crs = 31370)%>%
st_buffer(dist = 10)%>%
mutate(nearest=st_nearest_feature(.,geodata))%>%
st_join(geodata)%>%
mutate(naam = toupper(naam))%>%
mutate(check_location= str_detect(river_name, naam))%>%
filter(nearest==rowname)%>%
filter(check_location == FALSE)
controle_An=vmm_mismatch %>%filter(check_location == FALSE) %>% st_drop
write.csv(controle_An,"punten_tercontrole.csv")
