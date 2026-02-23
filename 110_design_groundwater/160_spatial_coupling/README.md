# Analysis: Spatial Coupling of Groundwater Installations

## Motivation

This subfolder contains an analysis of the spatial correlation of existing groundwater installations which are available in the `WATINA` database.
It was started in November 2024 and revisited/extended repeatedly in response to comments and additional requests of various stakeholders.
The motivating research question for this analysis is as follows.

> Spatial sampling of the MNE GRTS samples will return locations which are at a given distance to existing groundwater monitoring installations.
> At which distance is a re-use of existing installations justified?


The inverse task is to quantify the difference (or: correlation) of quasi-simultaneous measurements on a pair of existing locations.

Several analysis trajectories have been explored initially, but some were discarded (e.g. network-based modeling, for computational complexity and data stratification).
A crucial step was geospatial clustering and the cluster-wise processing of the data to compute pairwise differences.
After these initial trials, the method of choice are [variograms](https://tutorials.inbo.be/tutorials/spatial_variograms/), with some custom processing tweaks specific for the present data sets.


## Data Sources

Since the initial module of MNE covers eutrofication via groundwater, the primary measurement of interest is *water chemistry*.
Thus, we attempt our analyses on water chemistry measurements from `WATINA`, covering the chemical compounds ammonium $NH_4$, nitrate $NO_3$, and phosphate $PO_4$.

However, the chosen method (variograms) turns out to be limited for the occasionally low concentrations and historically quantized measurements (measurement accuracy, detection limits, ...).


Therefore, we primarily focus on *water levels* as a proxy for evaluating the question above.
Water levels are much more numerous, pair-wise differences are continuous and Gauss-distributed around zero, and thus the data is technically much more accessible with our method of choice.


There are dedicated **scripts** (`qmd` files / [quarto](https://quarto.org)) for each of these data sources, which roughly follow the trivial data flow (query, prepare, analyze, visualize).


## Script Overview

The numbering of the scripts was well-intended, but could not stand the test of time.
I apologize for not cleaning up by renaming, which I find futile because this analysis has been transient and parallelized and ever-growing.
Instead, you will find tables below which give an overview of the relevant scripts.


### Scripts Analyzing Water *Levels*

(Files marked with an exclamation mark are the most crucial outcomes of the data pipeline.)


|  | qmd file                                                      | purpose                                                                                                                                                     |
|-------|:----------------------------------------------------------|:------------------------------------------------------------------------------------------------------------------------------------------------------------|
|       | `100_download_and_plot_waterlevel_for_vgram`              | Data download and storage to `cache` folder (for overnight execution).                                                                                      |
|       | `101_quickcheck_validation`                               | To inspect the effectively used validation codes in `WATINA`.                                                                                               |
|       | `110_variograms_daily_waterlevel`                         | Initial exploration of water level variograms; includes general procedural definitions and choices (e.g. Matérn regression, Mean Absolute Difference, ...). |
|       |                                                           | This is merely explorative and not to be used.                                                                                                              |
|       | `111a_waterlevel_subsets`                                 | (Filtered) subset variograms with respect to slope, distance from water bodies, and filter depth difference.                                                |
|       | `111b_waterlevel_complements`                             | The complementary subset variograms.                                                                                                                        |
|       | `111c_quantile_seasons`                                   | Filtered waterlevel by quantiles ("high" and "low" water level subsets, to mimic xG3 parameters).                                                           |
|       | `112a_resample_bootstrap_dw`                              | Bootstrapping by resampling (`mTAW` variograms).                                                                                                            |
|       | `112b_resample_bootstrap_dm`                              | Bootstrapping by resampling (`mMaaiveld` variograms).                                                                                                       |
|       | `113a_cluster_distance_bias`                              | Cluster bias figure: how do clusters contribute to certain distance bins?                                                                                   |
|       | `113b_cluster_loo`                                        | "Leave-One-Out" analysis, filtering the larger of clusters.                                                                                                 |
|       | `114_waterlevel_differentials`                            | Variogram on temporal derivatives of water levels (*obsolete/unmaintained:* technical challenges of differentials).                                         |
|       | `115*_steekproefkader`                                    | Analysis of the preliminary MNE sample locations to evaluate impact of a chosen distance threshold (with `115a*` download/preparation script).              |
| **!** | `116_review_slopefilter`                                  | Refine previously explored filters and binning choices, which also includes the final filter settings.                                                      |
|       | `117_review_ecoregion`                                    | Segmenting the data by ecoregion instead of soilclass.                                                                                                      |
| **!** | `118_summary`                                             | Textual overview of the analysis outcome, which re-loads and explains regressions from previous files.                                                      |
|       | `300_spatialcoupling_waterlevels_extract_regressions` | Example script to extract previously stored regressions and retrieve details about the difference-distance relation captured by variograms.                 |
|       | `301_waterlevel_ridgelines`                           | Distribution of difference values per distance bin (detail).                                                                                                |
|       | `136_count_couples`                                   | Counting and mapping all pairs of installations which were included in the analysis.                                                                        |


Note that script `100_download_and_plot_waterlevel_for_vgram.qmd` has to be run prior to the others.
**This script is time-consuming (overnight) and causes considerable computational load on WATINA.**
However, I apply de-serialization: intermediate blocks of data are stored on disk and reloaded on demand (`arrow`/ parquet).
This `cache` can be shared internally.
Likewise, script `118_summary.qmd` depends on the previous scripts which have lower index numbers, and will crash unless those were executed beforehand (with the implication that all previous scripts must be updated first if data changes).


### Scripts Analyzing Water *Chemistry*

Analogous to water levels, there is a download script to acquire the data, and consecutively executed analysis scripts.

|       | qmd file                                       | purpose                                                                              |
|-------|:-----------------------------------------------|:-------------------------------------------------------------------------------------|
|       | `120_waterchemistry_reload`                | Technical script to re-load chemistry data from `WATINA`.                            |
|       | `121_inspect_chemvars`                     | Data exploration, e.g. to reverse-extract detection limit changes                    |
|       | `130_chemistry_variograms`                 | Explorative prototype for variogram computation.                                     |
|       | `132_chemistry_ridgeplots`                 | Distribution of difference values per distance bin.                                  |
| **!** | `133_chemistry_variograms_filterdepth`     | Chemistry variograms after relevant homogenizing of filter depth and length.         |
|       | `134_chemistry_variograms_longrange`       | Same variograms, but with a longer maximum pair distance.                            |
|       | `135_resample_bootstrap_waterchemistry`    | Bootstrapping by resampling of chemistry measurements.                               |
|       | `302_chemistry_variograms_ecoregion`       | Variograms of water chemistry, per ecoregion.                                        |
|       | `303_chemistry_ridgelines_ecoregion`       | Distribution of water chemistry differences, per bin and ecoregion.                  |
|       | `304a_assemble_detection_limits.org`           | Hard-coded, extracted detection limits.                                              |
|       | `304b_chemistry_variograms_detectionlimit` | Variograms after excluding measurements close to detection limit.                    |
|       | `136_count_couples`                        | Counting and mapping all pairs of installations which were included in the analysis. |


### Auxiliary Files

| file                       | purpose                                                                                                   |
|:---------------------------|:----------------------------------------------------------------------------------------------------------|
| `auxiliary_data_queries.R` | Shorthand/convenience functions to re-load cached data.                                                   |
| `regression_tools.R`       | Machinery for variogram regression, incl. storing and loading results.                                    |
| `spatial_helpers.R`        | Helper functions for spatial analysis aspects.                                                            |
| `water_sources.R`          | Assembling water bodies from different data sources for calculationg "water distance" of an installation. |
| `watina_migration.R`       | Database helper functions to connect to the current `WATINA` data warehouse.                                                                                                           |
| `zenodo_helper.R`          | Script to download relevant data sets from zenodo.                                                                                                           |
    

## Milestones

See `305_summarize_extra_analyses.org` for a timeline of analysis outcomes and design choices.


|         |                                                                                                              |
|--------:|:-------------------------------------------------------------------------------------------------------------|
| 2024-11 | Initiation of the analysis.                                                                                  |
| 2025-05 | Initial round-up of the analysis; afterwards: internal presentation and discussion.                          |
| 2026-01 | Revision of water chemistry data; identification of a bug regarding data validation; overhaul and extension; |
|         | still limited insight due to detection limits and concentration measurement quantization.                    |
| 2026-02 | Current version of this README.                                                                              |


