library(tidyverse)
library(inbodb) # see https://inbo.github.io/inbodb

# making the connection object
#################################"
aquachem <- connect_inbo_dbase("M0003_00_Aquachem")

# using dplyr verbs to do a quick exploration
#################################################
tbl(aquachem, "FactResultAqua") %>% glimpse
tbl(aquachem, "DimWaterhabitat") %>% glimpse
tbl(aquachem, "DimAnalysis") %>% glimpse
tbl(aquachem, "DimComponent") %>% glimpse
tbl(aquachem, "DimUnit") %>% glimpse
tbl(aquachem, "DimSample") %>% glimpse
tbl(aquachem, "DimStatus") %>% glimpse

# example without using dplyr verbs (Jo Loos):
############################################"
example_string <- "SELECT fra.*
  , dwh.*
  , da.AnalysisLabName
, dc.Component
, du.LimsUnit
FROM dbo.FactResultAqua fra
INNER JOIN dbo.DimWaterhabitat dwh ON dwh.WaterhabitatKey = fra.WaterhabitatKey
INNER JOIN dbo.DimAnalysis da ON da.AnalysisKey = fra.AnalysisKey
INNER JOIN dbo.DimComponent dc ON dc.ComponentKey = fra.ComponentKey
INNER JOIN dbo.DimUnit du ON du.UnitKey = fra.UnitKey
WHERE dwh.CODE = 'AN_MOL_004'
AND fra.FieldSamplingDate = '2016-08-16'
AND dc.Component not in ('Veldcode', 'Opmerking', 'Uitvoerder', 'Labo_ID', 'Datum bemonstering')"

# Problem for tidy work in R is the duplicated occurrence of specific column names, as seen from:
DBI::dbGetQuery(aquachem, example_string) %>% # this effectively downloads all results
    colnames %>%
    table %>%
    .[. > 1] %>%
    names # this gives the duplicated column names,
          # which would cause an error when using tbl(),
          # hence this query must be made more strict

# Let's make a lazy query object, not restricted to one site, only retaining
# essential information for analytical workflows. Also applying tidyverse-styled
# column names, which prepares for functions & later package. This object can be
# further built upon with dplyr verbs (filter, select, etc), before submission
# to the database.
################################################################################

lentic_chem_lazyqry <-
    tbl(aquachem, "FactResultAqua") %>%
    select(loc_id = WaterhabitatKey, # for joining; will be dropped
           loc_code = CODE,
           project = meetnet,
           date = FieldSamplingDate,
           type = HabtypeVel,
           ana_key = AnalysisKey, # for joining; will be dropped
           sample_key = SampleKey, # for joining; will be dropped
           variable = Component,
           value = ResultNumeric,
           unit = Unit,
           value_char = ResultFormatted,
           below_loq = IsBelowLOQ,
           above_loq = IsAboveLOQ,
           inferred = IsInferred,
           sample_remark = FieldSampleRemark
           ) %>%
        mutate(loq = ifelse(below_loq == 1 | above_loq == 1,
                            str_sub(value_char, 2, 100),
                            NA)) %>%
        mutate(loq = sql("CAST(loq AS float)"),
               date = sql("CAST(date AS date)")) %>%
        semi_join(tbl(aquachem, "DimSample") %>%
                      select(sample_key = SampleKey,
                             sample_status = SampleStatus,
                             sample_id = LabSampleID) %>%
                      filter(sample_status == "A",
                             str_sub(sample_id, 1, 1) != "D"),
                  by = "sample_key") %>%
        inner_join(tbl(aquachem, "DimAnalysis") %>%
                      select(ana_key = AnalysisKey,
                             protocol = SAPcode),
                  by = "ana_key") %>%
        inner_join(tbl(aquachem, "DimWaterhabitat") %>%
                       select(loc_id = WaterhabitatKey,
                              loc_remark = Opmerking,
                              loc_keep = weerhouden,
                              loc_reason_notkept = Reden_NW,
                              x = INSIDE_X,
                              y = INSIDE_Y) %>%
                       mutate(loc_keep = ifelse(loc_keep == "ja",
                                            1,
                                            0)) %>%
                       mutate(loc_keep = sql("CAST(loc_keep AS bit)")),
                   by = "loc_id") %>%
        select(-value_char, -ana_key, -loc_id, -sample_key) %>%
        select(project,
               loc_code,
               loc_remark,
               type,
               loc_keep,
               loc_reason_notkept,
               x,
               y,
               date,
               sample_remark,
               everything()
               )

# what does this do?
#######################"

# 1. when printing, it runs on the server and downloads first rows:
lentic_chem_lazyqry

# 2. it is just a wrapper for SQL code:
lentic_chem_lazyqry %>% show_query

# 3. alternative for printing:
lentic_chem_lazyqry %>% glimpse

# 4. it can be further built upon (before submitting to database):
my_selection <-
    lentic_chem_lazyqry %>%
    filter(loc_code == "AN_MOL_004",
           date == as.Date("2016-08-16"))

class(my_selection)

my_selection %>% count # number of rows (the count runs on database)
my_selection

# 5. download query results as tibble
localdata <- my_selection %>% collect
localdata


# There's also a DimPond table; this one needs a bit special handling because of
# special variables. If the table is needed at all.
################################################################################

dimpond_sql <-      # (credits to Jo Loos)
    "SELECT TOP (1000) [PondKey]
,[datumhuishkenm]
,[gebied]
,[percinsbz]
,[codesbz]
,[naamsbz]
,[codesbzdeelgeb]
,[stroomgebied]
,[bekken]
,[deelbekken]
,[GDB_ARCHIVE_OID]
,[created_user]
,[created_date]
,[last_edited_user]
,[last_edited_date]
,[globalid]
,[centroidx]
,[centroidy]
,[vhazone]
,[codeplas]
,[shape].STAsText() as shape_wkt
FROM DimPond"

# direct execution:
DBI::dbGetQuery(aquachem, dimpond_sql) %>%
    as_tibble

# use in a lazy query:
tbl(aquachem, sql(dimpond_sql)) %>% glimpse

