
#' @title Retrieve locations from the aquachem database
#' @description The function can only be used from within the INBO network.
#' @export get_locs_aquachem
#' @import dplyr
#' @import sf
#' @import assertthat
#' @import stringr
#' @param con connection to the aquachem database, default is connect_aquachem().
#' @param mask search within shapefile (.shp)
#' @param buffer create and include additional buffer around mask (in m)
#' @param bbox search within bbox, format = c("xmax", "xmin", "ymax", "ymin")
#' @param province search within province
#' @param town search within town
#' @param habtype_f search habitattype
#' @param collect collect data from lazy query


get_locs_aquachem <-function(con =connect_aquachem(),
         mask = NULL,
         buffer = 0,
         bbox = NULL,
         province = NULL,
         town = NULL,
         habtype_f = NULL,
         collect = FALSE) {

    if (!is.null(mask)) {
        assert_that(inherits(mask, "sf"),
                    msg = "mask must be an sf object.")
        assert_that(sf::st_crs(mask) == sf::st_crs(31370),
                    msg = "The CRS of mask must be Belgian Lambert 72 (EPSG-code 31370).")}

    assert_that(is.number(buffer), msg = "Buffer is not numeric")

    assert_that(is.null(bbox) | all(sort(names(bbox)) ==
                                        c("xmax", "xmin", "ymax", "ymin")),
                msg = "You did not correctly specify bbox.")

    if (!is.null(bbox)) {
        assert_that(bbox["xmax"] >= bbox["xmin"],
                    bbox["ymax"] >= bbox["ymin"])}


    locs <-
        tbl(con, "FactResultAqua") %>%
        dplyr::select(loc_id = WaterhabitatKey, # for joining; will be dropped
               loc_code = CODE,
               project = meetnet,
               date = FieldSamplingDate,
               habfield = HabtypeVel,
               ana_key = AnalysisKey, # for joining; will be dropped
               sample_key = SampleKey, # for joining; will be dropped
               variable = Component,
               value = ResultNumeric,
               unit = Unit,
               value_char = ResultFormatted, # to derive loq; will be dropped
               below_loq = IsBelowLOQ,
               above_loq = IsAboveLOQ,
               inferred = IsInferred,
               sample_remark = FieldSampleRemark,
               x = INSIDE_X,
               y = INSIDE_Y
        ) %>%
        mutate(loq = ifelse(below_loq == 1 | above_loq == 1,
                            str_sub(value_char, 2, 100),
                            NA)) %>%
        mutate(loq = sql("CAST(loq AS float)"),
               date = sql("CAST(date AS date)")) %>%
        dplyr::semi_join(tbl(con, "DimSample") %>%  # unneeded at a future time
                      dplyr::select(sample_key = SampleKey,
                             sample_status = SampleStatus) %>%
                      filter(sample_status == "A"),
                  by = "sample_key") %>%
        dplyr::inner_join(tbl(con, "DimAnalysis") %>% # unneeded at a future time
                       dplyr::select(ana_key = AnalysisKey,
                              protocol = SAPcode),
                   by = "ana_key") %>%
        dplyr::left_join(tbl(con,"DimWaterhabitat") %>%
            dplyr::select(provincie = Provincie,
                   gemeente = Gemeente,
                   loc_code = CODE,
                   sbz = SBZ,
                   x_check = X, #coordinate not matching with FactResultAqua ?
                   y_check = Y,
                   area = SHAPE_Area),
            by = "loc_code") %>%
        dplyr::select(gemeente, provincie, loc_code, habfield, x, y,x_check,y_check,area) %>% dplyr::distinct()

    if (!is.null(bbox)) {
        bbox_xmin <- unname(bbox["xmin"])
        bbox_xmax <- unname(bbox["xmax"])
        bbox_ymin <- unname(bbox["ymin"])
        bbox_ymax <- unname(bbox["ymax"])

        locs <-
            locs %>%
            filter(.data$x >= bbox_xmin,
                   .data$x <= bbox_xmax,
                   .data$y >= bbox_ymin,
                   .data$y <= bbox_ymax)
    }

    if (buffer != 0) {
        assert_that(!is.null(mask), msg = "no mask specified, add a mask or remove buffer")
        mask_expand <-
            mask %>%
            sf::st_buffer(dist = buffer)
    } else {
        mask_expand <-
            mask
    }

    if (!is.null(mask)) {
        bbox_mask = (sf::st_bbox(mask_expand))
        bbox_xmin=unname(bbox_mask$xmin)
        bbox_xmax=unname(bbox_mask$xmax)
        bbox_ymin=unname(bbox_mask$ymin)
        bbox_ymax=unname(bbox_mask$ymax)
        bbox_filter =locs%>% dplyr::select(loc_code,x, y)%>% filter(!is.na(x) | !is.na(y))%>% filter(between(x,bbox_xmin, bbox_xmax)) %>% filter(between(y, bbox_ymin, bbox_ymax)) %>% dplyr::select (x,y,loc_code)%>%
        collect %>%
        rownames_to_column()%>%
        sf::st_as_sf(coords= c("x","y"),crs = 31370)
        filter_locations = bbox_filter %>%  filter(rowname %in% (within_shape = sf::st_contains(mask_expand, bbox_filter)%>% unlist))%>% dplyr::select (loc_code)%>% sf::st_drop_geometry()%>% unlist()
        locs <- locs %>% filter (loc_code %in% filter_locations)%>%
        arrange(.data$loc_code)}


    if (!is.null(province)) {
        locs <-
            locs %>%
            filter(.data$provincie %in% province)}

    if (!is.null(town)) {
        locs <-
            locs %>%
            filter(.data$gemeente %in% town)}

    if (!is.null(habtype_f)) {
        locs <-
            locs %>%
          filter(.data$habfield %like% paste0("%",habtype_f,"%"))}

    if(collect == FALSE){locs} else {locs = locs%>% collect()}

    locs

}
