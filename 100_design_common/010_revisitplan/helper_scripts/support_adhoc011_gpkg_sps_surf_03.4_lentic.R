# 2026-07-01 Writing a geopackage with the spatial sampling units of
# SURF_03.4_lentic
#
# The results have been made with the RData file at tag rep_0.17.0, by
# running:
#
# Rscript -e 'bookdown::render_book("index.Rmd", "bookdown::html_document2",
# params = list(save_rdata = TRUE))'

# First run setup chunk
#
# Then run:

load(file.path(datapath, "binary/results/objects_panflpan5.RData"))

scheme_moco_ps_stratum_sppost_spsamples_sf %>%
  filter(scheme == "SURF_03.4_lentic") %>%
  st_write(file.path(
    datapath,
    "binary/results/SURF_03.4_lentic_spatial_sampling_units.gpkg"
  ))
