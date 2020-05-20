library(tidyverse)
library(inbodb) # see https://inbo.github.io/inbodb

# making the connection object
#################################
vmmchem <- connect_inbo_dbase("D0113_00_VMMData")

# using dplyr verbs to do a quick exploration
#################################################
tbl(vmmchem, "MetingFysicoChemieStaalname") %>% glimpse
tbl(vmmchem, "MetingFysicoChemieMeting") %>% glimpse
tbl(vmmchem, "FysicoChemischeParameter") %>% glimpse
tbl(vmmchem, "Meetpunt") %>% glimpse  ## ERROR!!

# For table Meetpunt, we need a tailored approach:
loc_sqlstring <-
    "SELECT [Code]
      ,[CodeOud]
      ,[Bekken]
      ,[DrinkwaterCategorie]
      ,[Gemeente1]
      ,[Gemeente2]
      ,[GrenspuntMet]
      ,[IsHengelwater]
      ,[IsSterkVeranderd]
      ,[IsZwemwater]
      ,[KaartNaam]
      ,[KaartNummer]
      ,[Lambert72_X]
      ,[Lambert72_Y]
      ,[Land1]
      ,[Land2]
      ,[Omschrijving]
      ,[OwlCategorie]
      ,[OwlCode]
      ,[OwlOrde]
      ,[OwlStatus]
      ,[OwlTypeCode]
      ,[Provincie1]
      ,[Provincie2]
      ,[Regio1]
      ,[Regio2]
      ,[SaliniteitCategorie]
      ,[StromingCategorie]
      ,[VhagCode]
      ,[VhasCode]
      ,[VhasZoneCode]
      ,[VhazCode]
      ,[VhazNaam]
      ,[ViswaterCategorie]
      ,[WaterKwaliteit]
      ,[Waterlichaam]
      ,[Waterloop]
      ,[WaterloopCategorie]
      ,[WaterloopType]
      ,[Wgs84_Lat]
      ,[Wgs84_Lon]
      ,[Zuiveringsgebied]
      ,[CreateUser]
      ,[CreateDate]
      ,[UpdateUser]
      ,[UpdateDate]
      ,[DataSource]
      ,[Lambert72_Geom].STAsText() as Lambert72_wkt
      ,[Wgs84_Geog].STAsText() as Wgs84_wkt
  FROM Meetpunt"
# now we can run:
tbl(vmmchem, sql(loc_sqlstring)) %>% glimpse

# Suggested columns to use
########################################

tbl(vmmchem, sql(loc_sqlstring)) %>%
    select(loc_code = Code,
           x = Lambert72_X,
           y = Lambert72_Y,
           vhas_code = VhasCode,
           vhag_code = VhagCode,
           wbody_code = OwlCode,
           wbody_order = OwlOrde,
           wbody_typecode = OwlTypeCode,
           wbody_name = Waterlichaam,
           river_name = Waterloop)

tbl(vmmchem, "MetingFysicoChemieStaalname") %>%
    select(event_id = Id,
           loc_code = MeetpuntCode,
           date = Datum) %>% # optionally use the VWxxx variables to add
                             # more information per sample (to be joined with
                             # other table)
    mutate(date = sql("CAST(date AS date)"))

tbl(vmmchem, "MetingFysicoChemieMeting") %>%
    mutate(loq = ifelse(WaardeTeken %in% c("<", ">"),
                        Waarde,
                        NA),
           below_loq = ifelse(WaardeTeken == "<", 1, 0),
           above_loq = ifelse(WaardeTeken == ">", 1, 0),
           value = ifelse(below_loq == 1, 0,
                          ifelse(above_loq == 1, loq,
                                 Waarde))
    ) %>%
    mutate(below_loq = sql("CAST(below_loq AS bit)"),
           above_loq = sql("CAST(above_loq AS bit)")) %>%
    select(event_id = StaalnameId,
           variable = ParameterCode,
           value,
           below_loq,
           above_loq,
           loq
           )

tbl(vmmchem, "FysicoChemischeParameter") %>%
    select(variable = Code,
           unit = EenheidSymbool)

# inner_join the above and you should have a useful result


