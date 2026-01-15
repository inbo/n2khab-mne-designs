
#' retrieve fysicochemical data from the local VMM-database
#' The function can only be used from within the INBO network.
#' @export get_chem_vmm
#' @import dplyr
#' @import purrr
#' @import sf
#' @import assertthat
#' @import stringr
#' @importFrom tibble rownames_to_column
#' @param con connection to local VMM database
#' @param bbox search within bbox
#' @param variable define variable(s)
#' @param stream define river name
#' @param guess set to TRUE will search for stream string
#' @param mask search within shapefile (.shp)
#' @param buffer_mask create additional mask around buffer
#' @param buffer_stream create search buffer around river segments (not all VMM water sample points intersect with stream segments)
#' @param stream_geodatabase collect data from stream segment geodatabase. Geodatabase needs to contain vhag_code (vhag_code) AND/OR river name (NAAM) and spatial data of river segment.
#' @param geodatabase link to the geodatabase
#' @param full_output give complete output

get_chem_vmm <-function(con,
                        bbox = NULL,
                        variable = NULL,
                        stream = NULL,
                        guess = FALSE,
                        mask = NULL,
                        buffer_mask = NULL,
                        stream_geodatabase = FALSE,
                        buffer_stream = NULL,
                        geodatabase=NULL,
                        full_output = FALSE) {

    assert_that(is.null(bbox) | all(sort(names(bbox)) ==
                                        c("xmax", "xmin", "ymax", "ymin")),
                msg = "You did not correctly specify bbox.")


    if (!is.null(bbox)) {
        assert_that(bbox["xmax"] >= bbox["xmin"],
                    bbox["ymax"] >= bbox["ymin"])
    }

    if (stream_geodatabase == TRUE){

        assert_that((!is.null(geodatabase)), msg = "Cannot collect data if no geodatabase is specified. Please specify geodatabase")


        if (!is.null(mask)) {
            assert_that(inherits(mask, "sf"),
                        msg = "mask must be an sf object.")
            assert_that(st_crs(mask) == st_crs(31370),
                        msg = "The CRS of mask must be Belgian Lambert 72 (EPSG-code 31370).")}

    }

    locs <-
        tbl(con, "Meetpunt") %>%
        dplyr::select(loc_code = Code,
               x = Lambert72_X,
               y = Lambert72_Y,
               vhas_code = VhasCode,
               vhag = VhagCode,
               wbody_code = OwlCode,
               wbody_order = OwlOrde,
               wbody_typecode = OwlTypeCode,
               wbody_name = Waterlichaam,
               river_name = Waterloop)%>%
        filter(!is.na(x))%>%

        dplyr::inner_join (tbl(con, "MetingFysicoChemieStaalname") %>%
                        dplyr::select(event_id = Id,
                               loc_code = MeetpuntCode,
                               date = Datum), by = "loc_code") %>% # optionally use the VWxxx variables to add
        # more information per sample (to be joined with
        # other table)
        mutate(date = sql("CAST(date AS date)")) %>%

        dplyr::inner_join(tbl(con, "MetingFysicoChemieMeting") %>%
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
                       dplyr::select(event_id = StaalnameId,
                              variable_code = ParameterCode,
                              value,
                              below_loq,
                              above_loq,
                              loq
                       ), by = "event_id")%>%

        dplyr::inner_join(tbl(con, "FysicoChemischeParameter") %>%
                       dplyr::select(variable_code = Code,
                              unit = EenheidSymbool),by = "variable_code")


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

    if (!is.null(variable)) {
        locs=locs %>% filter (variable_code %in% variable)}else {locs}
    if(nrow(locs%>% collect(n=1))==0){stop("No locs where found with specified variable(s). Check variable spelling")}

    if (!is.null(stream)) {if (guess == FALSE){
        locs=locs %>%
            filter(river_name %like% stream)}else{
                locs = locs%>% filter(river_name %like% paste0("%",stream,"%"))}}
    if(nrow(locs%>% collect(n=1))==0){stop("No locs where found. Check stream spelling (case insensitive) or try guess = T")}

    if (!is.null(buffer_mask) & (!is.null(mask)))
    {mask_expand <-
        mask %>%
        st_buffer(dist = buffer_mask)
    } else {
        mask_expand <-
            mask}

    if (!is.null(mask)) {
        bbox_mask = (st_bbox(mask_expand))
        bbox_xmin=unname(bbox_mask$xmin)
        bbox_xmax=unname(bbox_mask$xmax)
        bbox_ymin=unname(bbox_mask$ymin)
        bbox_ymax=unname(bbox_mask$ymax)
        bbox_filter =locs%>%  filter(!is.na(x) | !is.na(y))%>% filter(between(x,bbox_xmin, bbox_xmax)) %>% filter(between(y, bbox_ymin, bbox_ymax)) %>% dplyr::select(x,y,loc_code)%>% distinct%>%
            collect %>%
            rownames_to_column()%>%
            st_as_sf(coords= c("x","y"),crs = 31370)
        filter_locations = bbox_filter %>%  filter(rowname %in% (within_shape = st_contains(mask_expand, bbox_filter)%>% unlist))%>% dplyr::select (loc_code)%>% st_drop_geometry()%>% distinct
        locs <- locs %>% filter (loc_code %in% !!filter_locations$loc_code)%>% arrange(.data$loc_code)}

    if (stream_geodatabase == FALSE){locs}else{geodata=geodatabase
    assert_that(!is.null(buffer_stream),msg = "please define buffer width (in m) for river segments")
    geodata=geodata%>%rownames_to_column() %>% mutate(vhag_code = as.character(vhag_code))
    collected_locs= locs%>% dplyr::select(loc_code, x, y,vhag,river_name)%>% distinct()%>%collect%>%
        st_as_sf(coords= c("x","y"),crs = 31370)

    locs_buffer=st_buffer(x=collected_locs,dist = buffer_stream)%>%
        #find closest stream + buffer to point
        mutate(nearest=st_nearest_feature(.,geodata))%>%
        #join points with stream + buffer
        st_join(geodata)%>%
        filter(nearest==rowname)
    db_locs_input<-locs_buffer
    if (dim(db_locs_input)[1]==0) {stop("No data to collect. There is no match between given stream, bbox, variable and the database. If a stream was specified, try guess = T")
    }else{
        link_data=locs%>% filter (loc_code %in% !!db_locs_input$loc_code)%>%collect %>%
            st_as_sf(coords= c("x","y"),crs = 31370)
        link_data_buffer=st_buffer(x=link_data,dist = 10)%>%
            mutate(x = unlist(purrr::map(link_data$geometry,1)))%>%
            mutate(y = unlist(purrr::map(link_data$geometry,2)))%>%
            mutate(nearest=st_nearest_feature(.,geodata))%>%
            st_join(geodata)%>%
            mutate(
              # since following two fields are going to be used in pattern
              # matching, NAs are not allowed and need a quickfix
              vhag_code = ifelse(is.na(vhag_code), "missing", vhag_code),
              NAAM = ifelse(is.na(NAAM), "MISSING", NAAM),
              NAAM = toupper(NAAM)
            )%>%
            #check if stream names of VMMData and Geodatafiles are equal
            mutate(check_vhag= str_detect(vhag, vhag_code))%>%
            mutate(check_name= str_detect(river_name, NAAM))%>%
            filter(check_name == TRUE | check_vhag == TRUE) %>%
            filter(nearest==rowname)
        if (full_output == FALSE) {link_data_buffer =link_data_buffer %>% st_drop_geometry() %>% dplyr::select (c(loc_code,x,y,NAAM, variable_code, value, unit,date, rank, vhag_code,check_vhag,check_name))} else {link_data_buffer}

        if (!as.numeric(nrow(link_data_buffer)) == 0) {link_data_buffer}else{message("no data found within criteria")}}

    }}


