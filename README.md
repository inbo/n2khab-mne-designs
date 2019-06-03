## About this repository

This repo aims at providing the design of the monitoring schemes that together compose the monitoring programme for the natural environment (MNE) in Flanders.
The programme focuses on Natura 2000 habitat types and optionally, the so-called Regionally Important Biotopes.

A lot of the textual content of this repo is in Dutch because of the primarily Flemish audience.
However variables, functions and scripts are primarily in English in order to ease internationalization.

## Repository structure

The main folders consider the following topics:

- `010_making_choices_website`: website on choices (selections) for MNE
- `020_framework_design_monitoring`: a framework for design choices, inference strategy and other aspects of the monitoring workflow (data management, data quality, analytical, reporting, revision, QAQC)
- `110_design_groundwater`: design of the monitoring subprogramme for the groundwater compartment
- `210_design_surfacewater`: same for the surfacewater compartment
- `310_design_atmosphere`: same for the atmospheric compartment
- `410_design_soil`: same for the soil compartment
- `510_design_inundationwater`: same for the inundationwater compartment

The most important folders are those on the design itself.
_Their subfolders_ are likewise ordered as `010_...` and so on:

- numbers < 100 refer to **main parts of the design**
- numbers >= 100 refer to **supporting topics and analyses**

Subfolders can contain their own RStudio project on the topic.

For rather large subfolders (e.g. a report on a design), it is advised to further distinguish between a `src` and a `data` folder:

- `src` contains R-scripts and/or bookdown projects
- `data` is used to keep data sources, used or produced by code in the script.
This can be data organised in the same way as in the [n2khab-preprocessing](https://github.com/inbo/n2khab-inputs) repo and hence _git-ignored_ here, as well as _specific_ data sources which are either versioned here (textual data) or in a Zenodo repository (for binary data, hence _git-ignored_ here).
Regarding the first type of data (i.e. general n2khab data), it can as well be useful to have a separate `data` folder outside of the specific subfolder or even repo, in order to share the same data versions with other subfolders (i.e. other analyses) or even repositories.

## How to contribute to this repository?

1. Decide to which branch (c.q. pull request) you want to contribute (**reference branch**).
1. In your local repo, make your own new branch after having checked out the reference branch. In this way, the new branch is derived from the reference branch.
    - _Alternatively_, make your changes on the remote repo (at github.com), starting from the reference branch, and commit your changes as a new pull request. This workflow avoids the need of 1) having git installed locally and 2) managing your local repo. However, the possibilities of working with git are more limited.
1. Make the commits that you want to make, **in your branch**.
1. Push your local brach to the remote repo (github.com).
1. In the remote repo, start a pull request for this branch (+ request review, add clarification etc.). _Make sure to correctly set the reference branch for this pull request!_
1. When approved, your branch will be merged with the reference branch in the remote repo (at github.com).
1. Pull the reference branch and clean up your local repo in order to keep up with the remote.

More info on git workflows at INBO: <https://inbo.github.io/tutorials/tags/git/>


## General information on the MNE

The Flemish monitoring programme for the natural environment (MNE) will fulfill obligations of the Flemish Decree on the conservation of nature and the natural environment.
No long-term monitoring programme yet existed with this focus.
As environmental pressures severely hinder the achievement of a favourable conservation status for most of these habitat types, monitoring of their environmental characteristics is imperative to guide Flemish nature policy.

MNE aims at drawing conclusions on both state and trend of environmental characteristics of (groups of) habitat types at a regional level.
The programme will allow to prioritize, underpin and evaluate environment-oriented nature policy measures at the Flemish scale by generating representative long-term data of known quality.
Hence, its primary function is to provide quantitative diagnostics of relevant environmental issues.
In addition, the monitoring results will aid in assessing the environmental subcriteria of the conservation status of habitats and provide reliable information for the monitoring reports for the European Commission (Habitats Directive article 17).
To this end, each environmental compartment (groundwater, surface water, atmosphere and soil) will be served by a specific MNE monitoring subprogramme aligned with the six-year cycles of the Natura 2000 policy.

MNE will provide solid conclusions for (groups of) habitat types at the Flemish scale, but will be based on a selection of sites in space and time.
Therefore, a statistical approach is needed to achieve the desired (or acceptable) level of precision, significance and power.


