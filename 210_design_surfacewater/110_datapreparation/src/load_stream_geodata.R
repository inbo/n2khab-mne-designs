library(tidyverse)
library(inbodb)
library(sf)
library(dplyr)
library(tibble)
library(stringr)

#join different shapefile layers into one file
hab3260_V1_0<-st_read("../110_datapreparation/data/10_input/shapefiles_H3260/H3260_v1_0/H3260_v1_0.shp")%>% #v1.0 = periode <=2006 enkel HT3260
    st_transform(crs= 31370) %>%
    rename(naam=NAAM)%>%
    select(c(naam,LENGTE,geometry))%>%
    mutate(period_min = "1899-01-01", period_max ="2006-12-31", source = "H3260_v1_0")
hab3260_V1_2<-st_read("../110_datapreparation/data/10_input/shapefiles_H3260/H3260_v1_2/H3260_v1_2_versie2.shp")%>% #v1.2 = periode 2007
    st_transform(crs= 31370) %>%
    filter(CONCLUSIE == "3260")%>%
    rename(LENGTE=SHAPE_Leng)%>%
    select(c(naam,LENGTE,geometry))%>%
    mutate(period_min = "2007-01-01", period_max ="2007-12-31", source = "H3260_v1_2")
hab3260_V1_3<-st_read("../110_datapreparation/data/10_input/shapefiles_H3260/H3260_v1_3/H3260_v1_3_versie1.shp")%>% #v1.3 = periode 2008-2009
    st_transform(crs= 31370) %>%
    rename(LENGTE=SHAPE_Leng)%>%
    filter(CONCLUSIE == "3260")%>%
    select(c(naam,LENGTE,geometry))%>%
    mutate(period_min = "2008-01-01", period_max ="2009-12-31", source = "H3260_v1_3")
hab3260_V1_4<-st_read("../110_datapreparation/data/10_input/shapefiles_H3260/H3260_v1_4/H3260_v1_4_versie2.shp")%>% #v1.4 = periode 2010-2011
    st_transform(crs= 31370)%>%
    filter(CONCLUSIE == "3260")%>%
    rename(LENGTE=Shape_Leng)%>%
    select(c(naam,LENGTE,geometry))%>%
    mutate(period_min = "2010-01-01", period_max ="2011-12-31", source = "H3260_v1_4")
hab3260_V1_5<-st_read("../110_datapreparation/data/10_input/shapefiles_H3260/H3260_v1_5/H3260_v1_5_versie1.shp")%>% #v1.5 = periode 2012-2015
    st_transform(crs= 31370) %>%
    filter(CONCLUSIE == "3260")%>%
    rename(LENGTE=Shape_Leng)%>%
    select(c(naam,LENGTE,geometry))%>%
    mutate(period_min = "2012-01-01", period_max ="2015-12-31", source = "H3260_v1_5")
hab3260_V1_6<-st_read("../110_datapreparation/data/10_input/shapefiles_H3260/H3260_v1_6//Hab3260.shp")%>% #v1.6 = periode 2016-2017
    st_transform(crs= 31370)%>%
    rename(naam=NAAM)%>%
    select(c(naam,LENGTE,geometry))%>%
    mutate(period_min = "2016-01-01", period_max ="2017-12-31", source = "H3260_v1_6")
hab3260_V1_7<-st_read("../110_datapreparation/data/10_input/shapefiles_H3260/H3260_v1_7/H3260_v1_7_versie2_hab.shp")%>% #v1.7 = periode 2018-2019
    st_transform(crs= 31370)%>%
    filter(CONCLUSIE == "3260")%>%
    rename(LENGTE=Shape_Leng)%>%
    select(c(naam,LENGTE,geometry))%>%
    mutate(period_min = "2016-01-01", period_max ="2017-12-31", source = "H3260_v1_7")

list_hab3260=list(hab3260_V1_0,hab3260_V1_2,hab3260_V1_3,hab3260_V1_4,hab3260_V1_5,hab3260_V1_6,hab3260_V1_7)
hab3260_integration=bind_rows(list_hab3260) %>%
    #rownames_to_column()%>% # moved to get_locs_vmm function
    st_write(paste0(getwd(), "/data/20_output", "/hab3260_integration.shp"))


