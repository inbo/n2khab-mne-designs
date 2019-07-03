## Setup

### Repo organisation

- read about it in the [README](../../README.md) of the repo
- VLOPS evaluation for MNE is in the `310_design_atmosphere/120_vlops_evaluation_mne` path, where an RStudio file is present.
Can be used for a bookdown project, R-scripts etc.
- data sources go into a `data` folder right below `310_design_atmosphere`, so that these data versions can be shared between multiple sub-projects

### File & directory management

#### Locally

- read about it in the [README](../../README.md) of the repo
- all data sources below `data/10_raw` and `data/20_processed` are put into their own subfolder, carrying the name of the data source.
In more simple cases, the name of data source file(s) (without extension) in that subfolder is the same as the subfolder itself.
E.g. the data source `habitatmap_stdized.gpkg` is found below `data/20_processed/habitatmap_stdized/`.
Data source names and the associated R objects are as much as possible internationalized: English names of folder & files and of variable names of the R-object.
- these data source names do not carry version names: the versioning of at least binary and/or large data sources which are worth distributing on their own (can be raw or processed) is done separately.
- _specific choices here_ for local data management: directly below folder `310_design_atmosphere` a folder
`data` is to be made, with 2 subfolders `10_raw` and `20_processed`.
**Raw** data sources go in the fist one, **processed** data sources in the second.
    - **large and/or binary** data sources are **git-ignored**.
    They are mostly either _distributed_ (see below) raw data sources, or _non-distributed_, locally reproducible processed data sources.
    In the latter case, just keep the code to reproduce the data inside the current repo.
    In a few cases also processed data sources are worth distributing (when processing steps take very long): see below for the place where code is to be maintained.
    - **text** data sources (not too large) can be versioned by git within the current repo.
- in your workflow, save the relative path to `data` into an object at a top-level or in the beginning of a bookdown project
    
#### Distributed data sources

- an illustration of ways of distribution can be found [here](https://drive.google.com/open?id=1xZz9f9n8zSUxBJvW6WEFLyDK7Ya0u4iN) and a list of distributed data sources and their versions is [here](https://docs.google.com/spreadsheets/d/1E8ERlfYwP3OjluL8d7_4rR1W34ka4LRCE35JTxf3WMI) (under construction).
- public distribution and versioning is managed by @florisvdh & few colleagues and is under construction.
Cf. these [tasks](https://github.com/inbo/n2khab-monitoring/projects/1).
- typically, the [n2khab](https://github.com/inbo/n2khab) package provides functions to get each distributed (N2KHAB-relevant), locally saved data source into the R environment in a standard & internationalized way.
Important text datasets of general use are distributed by the package itself.
- **INBO-level** distribution of binary and/or large data-sources is a convenient intermediate step, and is the first stopping place for cooperation at INBO.
Current setup is still temporary but to be followed for now:
    - data sources are distributed at INBO either via a [gdrive](https://drive.google.com/drive/folders/162F2mz30bqjG7ZuPlv5WsAVQK8nXJOI5?usp=sharing) folder (for externally managed datasets which are already somewhere on INBO's Q-drive) or under a dedicated folder on Q: `Prjdata/Projects/PRJ_Natura2000/n2khab-binaire-databronnen` (use citrix or a local network mount)
    - so, put the _to-be-distributed_ data sources under the dedicated Q-folder.
        - This is to be done at least for **raw** data sources, for reproducibility reasons.
        - For **processed** data sources this is only done when the processing steps take too long for convenient cooperation/reproduction.
        In that case (distributing a processed data source), is is advised to make the script or bookdown project to reproduce it inside the [n2khab-preprocessing](https://github.com/inbo/n2khab-preprocessing) repo, where that stuff is kept together.
        - Best practice is to write functions in the [n2khab](https://github.com/inbo/n2khab) package that read in a data source from a local folder (see current examples in the package), and use these in your script.
        - Add the necessary lines in the [data source list](https://docs.google.com/spreadsheets/d/1E8ERlfYwP3OjluL8d7_4rR1W34ka4LRCE35JTxf3WMI) to define the data sources and their versions.
    
### Content related information

#### Aim

As discussed, and you can read about it in a [powerpoint presentation](https://drive.google.com/open?id=1abWt-L3MAF0NeHEQgyi-e6hShiceX2pL) given to VMM on 12/03/2019, somwhere from the middle of the presentation and further.

Essentially, we want to estimate '(spatiotemporele) gemiddelden / kwantielen van tmd + tzd en achterliggende componenten, i.e. per typegroep (set van habitattypes) gedurende een zesjaarperiode voor al de Vlaamse locaties waar de typegroep voorkomt'.

This relates to two schemes within the MNE: ATM_03.1 and ATM_04.1.
You can read about schemes and the relation between schemes and types here: `?n2khab::schemes`; `?n2khab::scheme_types`.
Typegroups are defined within the data_source `scheme_types`.

The aim now is to see if the error margins of the above estimates fall within the [required ones in this gsheet](https://docs.google.com/spreadsheets/d/1IMlLh9EPPRLLoDNWkDzbVJU2cVvst7C8NyJRM2NvujM/edit?pli=1#gid=229047732).

Uncalibrated calculations are provided as well as measurement data.
As VMM calibrates these results with the measurement data of 17 fixed stations (linear regression through origin; see [here](https://drive.google.com/open?id=1OUST__Wtc-_xMaEbLRsNfstkM-q3gzLM)), it seems wise to do leave-one-out calibrations in order to compare measurement data of one station with model predictions.
Further, there are many extra measurement locations for NH3, however these don't span a calendar year and hence may not be useful in calibration of NH3.

Total eutrophying deposition is 'tmd' (totaal vermestend) and total acidifying deposition is 'tzd' (totaal verzurend).
The first one is represented in the above gsheet as 'BD_Ntot' and the second one as 'BD_acidif'. See [this list](https://docs.google.com/spreadsheets/d/1gJb2nY-Cs-SCNMpz0sGFslb0j69mzNx5BJu9tQXpHfc/edit?pli=1#gid=0) for more on these abbreviations, but these two top-level variables still seem to be missing.

#### Data

VMM data are currently not in a distributed form, but those (subsets) that are used in scripts should be distributed (for reproducibility) as distributed raw data, so they should be given an English name, defined in the data source list and put inside the dedicated Q-folder (see higher).

VMM data, often with several subsets and with accompanying metadata, original zip-file etc:

- uncalibrated VLOPS point calculations for 2016 are [here](https://drive.google.com/open?id=1TusczXBJJJykqm12H7zZMRU_yLcaaD6l).
This is for the point layer which INBO has provided and which can be reproduced by the bookdown project in `310_design_atmosphere/110_vlops_datapreparation`.
From this trial by VMM, it seems that the number of calculated points is still excessively high to do this routinely.
- measurement data of 2016 are [here](https://drive.google.com/open?id=14WF4rEhOhpKY_Y9zxqtgLJoJZeACmb3U)
- [VLOPS results](https://drive.google.com/open?id=17Kvgx1SCGWoUAORnnVbkx8gLSaxgJfkQ) of concentrations, dry deposition, wet deposition, total deposition at the 1 km^2^ level, both calibrated and uncalibrated for years 1990 + 2000-2017 (however for 2017 the emission data of 2016 were used, only meteo is from 2017)

Reproducible processed data sets (non-distributed):

- `typegroups_atm3.1_4.1_pol`: a geopackage with one polygon layer, reproduced by the R-script in the current folder `310_design_atmosphere/120_vlops_evaluation_mne`.
It has all applicable polygons for the scheme on eutrophication (3.1) and the one on acidification (4.1), and gives the typegroup membership for each polygon (1 if it is a member, 0 if not).
Multiple typegroup memberships are possible for the same polygon: because of the two different schemes, but also because multiple types may reside in the same polygon.
You may also want to organise this R script within another structure.
Some of its code repeats some of the code from `310_design_atmosphere/110_vlops_datapreparation`.
- `vlops_pointreceptors_mne`: geospatial point layer, produced by the bookdown project in `310_design_atmosphere/110_vlops_datapreparation`

Important distributed data by INBO:

- habitatmap_stdized (see `?n2khab::read_habitatmap_stdized`)
- GRTSmaster_habitats and a few derived processed data sources.
(See also: `vignette("v030_GRTSmh", package = "n2khab")`)
- reference lists (see `vignette("v010_reference_lists", package = "n2khab")`)


#### Background on models and prediction errors

Everything was collected [here](https://drive.google.com/open?id=1JEgpDfuSPG7xen3Bb0gsF48vs0MKUj03) during previous months.








