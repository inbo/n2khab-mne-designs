    library(tidyverse)
    library(inbodb)
    library(sf)
    library(assertthat)
    library(dplyr)
    library(tibble)
    library(stringr)



    get_locs_vmm <-function(con,
    bbox = NULL,
    parameter = NULL,
    stream = NULL,
    guess = FALSE,
    collect_HT3260 = FALSE,
    geodatabase=NULL) {

        assert_that(is.null(bbox) | all(sort(names(bbox)) ==
                                            c("xmax", "xmin", "ymax", "ymin")),
                    msg = "You did not correctly specify bbox.")


        if (!is.null(bbox)) {
            assert_that(bbox["xmax"] >= bbox["xmin"],
                        bbox["ymax"] >= bbox["ymin"])
        }

        if (collect_HT3260 == TRUE){

        assert_that((!is.null(geodatabase)), msg = "Cannot collect HT3260 data if no geodatabase is specified. Please specify geodatabase")

        if (is.null(parameter)) {
            assert_that((askYesNo("No parameters were specified. Collecting data might take a while. Do you want to continue?",
                                     default = F, prompts = gettext(c("Yes", "No","Cancel")))==TRUE), msg = "Script halted by user")}
        if (is.null(bbox) & is.null(stream)) {
            assert_that((askYesNo("No streams or bbox specified. Collecting data might take a while. Do you want to continue?",
                                  default = F, prompts = gettext(c("Yes", "No","Cancel")))==TRUE), msg = "Script halted by user")}

    }

        locs <-
            tbl(vmmchem, "Meetpunt") %>%
            select(loc_code = Code,
                   x = Lambert72_X,
                   y = Lambert72_Y,
                   vhas_code = VhasCode,
                   vhag_code = VhagCode,
                   wbody_code = OwlCode,
                   wbody_order = OwlOrde,
                   wbody_typecode = OwlTypeCode,
                   wbody_name = Waterlichaam,
                   river_name = Waterloop)%>%
            filter(!is.na(x))%>%

            inner_join (tbl(vmmchem, "MetingFysicoChemieStaalname") %>%
                            select(event_id = Id,
                                   loc_code = MeetpuntCode,
                                   date = Datum), by = "loc_code") %>% # optionally use the VWxxx variables to add
            # more information per sample (to be joined with
            # other table)
            mutate(date = sql("CAST(date AS date)")) %>%

            inner_join(tbl(vmmchem, "MetingFysicoChemieMeting") %>%
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
                           ), by = "event_id")%>%

            inner_join(tbl(vmmchem, "FysicoChemischeParameter") %>%
                           select(variable = Code,
                                  unit = EenheidSymbool),by = "variable")


        if (!is.null(bbox)) {
            bbox_xmin <- unname(bbox["xmin"])
            bbox_xmax <- unname(bbox["xmax"])
            bbox_ymin <- unname(bbox["ymin"])
            bbox_ymax <- unname(bbox["ymax"])
                locs=locs %>%
                filter(.data$x >= bbox_xmin,
                       .data$x <= bbox_xmax,
                       .data$y >= bbox_ymin,
                       .data$y <= bbox_ymax)
        }

        if (!is.null(parameter)) {
            locs=locs %>% filter (variable %in% parameter)}else {locs}

        if (!is.null(stream)) {if (guess == FALSE){
                locs=locs %>%
                filter(river_name %like% stream)}else{
                    locs = locs%>% filter(river_name %like% paste0("%",stream,"%"))}}
        if(nrow(locs%>% collect(n=1))==0){stop("No locs where found. If a stream was specified, check spelling (case insensitive) or try guess = T")}

        if (collect_HT3260 == FALSE){locs}else{geodata=geodatabase
            geodata=geodata%>%rownames_to_column()
           collected_locs= locs%>% select(loc_code, x, y)%>% distinct()%>%collect%>%
                st_as_sf(coords= c("x","y"),crs = 31370)
            locs_buffer=st_buffer(x=collected_locs,dist = 10)%>%
                #find closest stream + buffer to point
                mutate(nearest=st_nearest_feature(.,geodata))%>%
                #join points with stream + buffer
                st_join(geodata)%>%
                filter(nearest==rowname)
            db_locs_input<-locs_buffer
            if (dim(db_locs_input)[1]==0) {stop("No data to collect. There is no match between given stream, bbox, parameter and the HT3260 database. If a stream was specified, try guess = T")
            }else{
            link_data=locs%>% filter (loc_code %in% !!db_locs_input$loc_code)%>% collect %>%
                st_as_sf(coords= c("x","y"),crs = 31370)
                link_data_buffer=st_buffer(x=link_data,dist = 10)%>%
                mutate(nearest=st_nearest_feature(.,geodata))%>%
                st_join(geodata)%>%
                mutate(naam = toupper(naam))%>%
                #check if stream names of VMMData and Geodatafiles are equal
                mutate(check_location= str_detect(river_name, naam))%>%
                #remove unnamed locations
                filter(!is.na(naam))%>%
                #only keep the closest stream (if point is within different stream buffers)
                filter(nearest==rowname)%>%
                filter(date >= period_min & date <= period_max)}
                if(FALSE %in% link_data_buffer$check_location){warning("VMM database and geodatabase use different name for same stream segment")
                difference = link_data_buffer %>% filter (check_location == FALSE) %>%
                    select(c(loc_code,vhas_code, river_name, naam, source))%>% unique
                print(paste0(capture.output(st_set_geometry(difference,NULL)), collapse = "\n"))}

            link_data_buffer

        }
    }




