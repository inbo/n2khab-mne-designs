library(tidyverse)
library(inbodb)
library(sf)
library(assertthat)



get_locs_aquachem <-function(con,
         mask = NULL, #still need to include
         join_mask = FALSE, #still need to include
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
    
    if (!is.null(mask)) {
      
            nr_dropped_locs <-
        locs %>% collect %>%
        filter(is.na(.data$x) | is.na(.data$y)) %>%
        count %>%
        .$n
      
    
      
      if (nr_dropped_locs > 0) {
        warning("Dropped ",
                nr_dropped_locs,
                " locations from which x or y coordinates were missing.\n")
      }
      
      locs <-
        locs %>%
        filter(!is.na(.data$x), !is.na(.data$y)) %>%
        arrange(.data$loc_code)
  
        
      #watina::as_points(warn_dupl = FALSE)
      
      if (buffer != 0) {
        mask_expand <-
          mask %>%
          st_buffer(dist = buffer)
      } else {
        mask_expand <-
          mask
      }
      
      if (join_mask ) {
        
        locs <-
          locs %>%
          st_join(mask_expand,
                  left = FALSE) %>%
          st_drop_geometry #issue with mask expand ask Floris what subsetting does, check Watina package
        
      } else {
          locs <-
          locs %>%
          .[mask_expand,] %>%
          st_drop_geometry
        
      }
      

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
    }

  #add filter by mask and buffer

    return(locs)

}
