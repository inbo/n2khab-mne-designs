library(tidyverse)
library(inbodb)
library(sf)
library(assertthat)


get_locs_aquachem <-function(con,
         mask = NULL, #still need to include
         buffer = 0, #still need to include
         bbox = NULL,
         province = NULL,
         town = NULL,
         habtype_f = NULL,
         collect = FALSE) {

    if (!is.null(mask)) {
        assert_that(inherits(mask, "sf"),
                    msg = "mask must be an sf object.")
        assert_that(st_crs(mask) == st_crs(31370),
                    msg = "The CRS of mask must be Belgian Lambert 72 (EPSG-code 31370).")}

    assert_that(is.number(buffer))
    assert_that(is.null(bbox) | all(sort(names(bbox)) ==
                                        c("xmax", "xmin", "ymax", "ymin")),
                msg = "You did not correctly specify bbox.")

    if (!is.null(bbox)) {
        assert_that(bbox["xmax"] >= bbox["xmin"],
                    bbox["ymax"] >= bbox["ymin"])
    }

    locs <-
        tbl(con, "FactResultAqua") %>%
        select(loc_id = WaterhabitatKey, # for joining; will be dropped
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
        semi_join(tbl(con, "DimSample") %>%  # unneeded at a future time
                      select(sample_key = SampleKey,
                             sample_status = SampleStatus) %>%
                      filter(sample_status == "A"),
                  by = "sample_key") %>%
        inner_join(tbl(con, "DimAnalysis") %>% # unneeded at a future time
                       select(ana_key = AnalysisKey,
                              protocol = SAPcode),
                   by = "ana_key") %>%
        left_join(tbl(con,"DimWaterhabitat") %>%
            select(provincie = Provincie,
                   gemeente = Gemeente,
                   loc_code = CODE,
                   sbz = SBZ,
                   x_check = X, #coordinate not matching with FactResultAqua ?
                   y_check = Y),
            by = "loc_code") %>%
        select(gemeente, provincie, loc_code, habfield, x, y,x_check,y_check) %>% distinct()

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
            st_buffer(dist = buffer)
    } else {
        mask_expand <-
            mask
    }

    if (!is.null(mask)) {
        bbox_mask = (st_bbox(mask_expand))
        bbox_xmin=unname(bbox_mask$xmin)
        bbox_xmax=unname(bbox_mask$xmax)
        bbox_ymin=unname(bbox_mask$ymin)
        bbox_ymax=unname(bbox_mask$ymax)
        bbox_filter =locs%>% select(loc_code,x, y)%>% filter(!is.na(x) | !is.na(y))%>% filter(between(x,bbox_xmin, bbox_xmax)) %>% filter(between(y, bbox_ymin, bbox_ymax)) %>%
        collect %>%
        rownames_to_column()%>%
        st_as_sf(coords= c("x","y"),crs = 31370)
        filter_locations = bbox_filter %>%  filter(rowname %in% (within_shape = st_contains(mask_expand, bbox_filter)%>% unlist))%>% select (loc_code)%>% st_drop_geometry()%>% unlist()
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
            filter(.data$habfield %in% habtype_f)}

    if(collect == FALSE){locs} else {locs = locs%>% collect()}


    return(locs)

}
