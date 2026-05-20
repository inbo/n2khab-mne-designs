# REP (development version)

tag `rep_xxxxxxxxxxxx`

# REP 0.16.0 (2026-05-20)

tag `rep_0.16.0`

Since this version, a REP changelog has been established as a new file `NEWS.md`.
The tag messages contain the version-specific section of this changelog.

This version introduces few changes to results. Spatial samples and
spatiotemporal samples are unaffected. The FAG calendar is unaffected for the 
part that is designed by this REP version (i.e. that part is the same as in REP
0.15.0), while the appended (and included) occasions from older REP versions
have been extended.

Changes to results
------------------

- A fix has been applied to the base sampling frame precursor
  `stratum_grts_phab_cellcenter_n2khab_collapsed`, as it contained a few
  duplicate rows by accident. As a consequence of fixing this, some population
  sizes (resulting from counts in the sampling frames) have received tiny
  adjustments. However these changes did not affect the resulting samples.
- Short-term planned sampling from both REP 0.13.1 and REP 0.14.0 has been
  picked up in `fag_stratum_grts_calendar` more extensively than was the case in
  REP 0.15.0:
  - Not only including REP 0.14.0 as before, but also REP 0.13.1.
  - Spatial sampling units present in REP 0.14.0 but absent from the FAG 
    calendar for the current spatiotemporal samples, are allowed as well.
  - From REP 0.13.1, only the extra spatial sampling units are
    admitted that are missing both from REP 0.14.0 and from current
    spatiotemporal samples.
  - Note that the appended older FAG occasions can be seen in
    `cal_old_continuation`.
- A few more REP objects are included in the RData file. The most important 
  objects from the RData file are now documented by the
  {[mnedesigndata](https://inbo.github.io/mnedesigndata)} package.
- The VBI overlaps have been updated to take into account the overlap with the
  circular VBI plots with radius 18 m (not just their center), and overlaps are
  no longer restricted to the forest strata from the MNE samples.
  
Additions
---------

- Add helper script to compare GRTS-ranges between spatial samples of different
  types.
- Mark some unresolved problems for future solving.

Maintenance
-----------

- Drop the `phab_correct` YAML parameter since we only apply its `TRUE` state.
- Set explicit data source versions when calling {n2khab} functions.
- Modernize and harden some outdated {dplyr} function syntax (joins, 
  `recode_values()`).
- Style R code with the {styler} package.

Helper script supporting fieldwork organization
-----------------------------------------------

In the helper script that supports data creation to organize fieldwork:

- Fix the `schemes_served_all` column of the object
  `fag_stratum_grts_calendar_shortterm_attribs` and derived objects.
- Add a `wait_mhq` column in the short-term fieldwork calendar to
  hold the MHQ-only FAG occasions.
- Column order of `scheme_moco_ps_stratum_targetpanel_spsamples` was
  slightly adjusted.
- More attribute columns have been kept in the shortterm objects, and
  their order has been slightly adjusted.
- Names of replacement cell objects were given (sf points object) or
  adjusted (SpatRaster object).
- The priorities of targetpanels PS1PANEL02 and PS1PANEL03 in GW_03.3
  have been set to their normal state by switching them. Before, they
  had been switched with regard to anticipating the bird season. This
  update specifically affects `fieldwork_shortterm_prioritization_by_stratum`
  and a few derived (summarizing) objects.
- Update code with regard to VBI overlaps, in accordance with the REP updates.
- Replace previous `scheme_ps_oldtargetpanel` column by
  `scheme_ps_oldtargetpanels` (plural). It collapses values of the old column
  (by string concatenation) that belong to the same FAG occasion in the
  short-term fieldwork calendar, effectively removing duplicates.
- Fix the `scheme_ps_targetpanels` column of the short-term fieldwork calendar
  for FAG occasions adopted from older REP versions: it now always adopts the
  value of `scheme_ps_oldtargetpanels` in those cases (before, it was still the
  value from the current spatiotemporal samples if the involved sampling unit
  was a member of it).
- Update code with regard to the uptake of extra sampling units of multiple old
  FAG calendars, in accordance with the REP updates.
- Add a `wait_obsolete_types` column in the short-term fieldwork calendar to
  hold specific combinations of field activity group and panel set for types
  '6410_ve' and '6510_hus', which will be obsoleted. Also, drop the priorities
  of the involved FAG occasions if these exclusively apply to panel set 2 across
  schemes.
  
# REP 0.15.0 (2026-03-12)

tag `rep_0.15.0`

Since this version, POC has been renamed as REP, which stands for 'revisitplan'.

This version contains following updates.

- bring SURF to a 6-year cycle
- reset year_first to 2026 for GW
- start panels in 1st cycle on year_first
- pick up short-term planned sampling from REP 0.14.0
- extend and streamline the list column scheme_moco_ps of the FAG calendar:
    These tibbles now always contain schemes that are served by
    an auxiliary FAG in the same FAG cycle ID, regardless of the
    simplification step. In order to distinguish between linked
    occasions at the same time as the active occasion, or at a later
    time, extra columns are added in the scheme_moco_ps tibbles.

- in the helper script that supports data creation to organize fieldwork:
  - limit scope of this file and rename it
  - adjust for 2026 and generalize some object names
  - updates with regard to updated object structures in REP's RData file
  - update the priorities for shortterm fieldwork
  - derive the orthophoto objects from the prioritization by stratum object

- add several interactive helper scripts

This version serves as updated input for fieldwork preparation and optimization.

# REP 0.14.0 (2025-10-16)

tags `rep_0.14.0`, `poc_0.14.0`

Tag `rep_0.14.0` is an alias to tag `poc_0.14.0`. REP stands for 'revisitplan',
which is the new name for this workflow.

From here on, the name 'POC' is phased out and not used in later version
names.

This version contains following updates.

- adjustments of sample sizes at type level, mostly in GW and SOIL schemes, in order
  to prevent disturbance of some types in local areas:
  - sample size truncation:
    - GW schemes: truncate a type's domain-specific target sample size at 20% of the
      population size in that domain
    - SOIL schemes: truncate a type's domain-specific sample size at 1/3 of the
      population size in that domain
  - drop 7140_oli and 2190_mp from the densification submodules
  - reduce the _resulting_ target sample size of specific types by 50%, depending on
    the scheme
    - GW: 1310_pol, 1330_hpr, 2190_overig, 6430_mr, 6430_hw, 6510_hua, 6510_hus, 7230
    - SOIL: 1310_pol, 6510_hua, 6510_hus, 7230
  - for types with sample size 0 or 1 for panel set 1 in included domains, increase to
    sample size 2 (as done at stratum level for Flanders, before)
  - overall sample size increase to compensate the type-specific reductions

- in the helper script that supports data creation to organize fieldwork:
  - in the fieldwork 2025 objects, include all _first_ GW FAGs per sampling unit that
    match "INST|LEVREAD|SPATPOSIT", despite the fact they happen later. This allows
    these FAGs to be executed earlier (in the case of INST + SPATPOSIT), or used as a
    reference to add more preceding occasions (LEVREAD). Before, these FAGs were only
    used if they happen in 2026.
  - in the fieldwork 2025 prioritization objects, add a 'wait_floating' column.
    It currently represents the 7140_mrd stratum, for which another approach will
    be needed in the groundwater schemes, more similar to watersurfaces. Until this
    has been elaborated in the sampling frames & spatial samples, these locations
    should not be equipped with measurement devices.
  - in the fieldwork 2025 prioritization objects, add a 'wait_any' column that is
    TRUE if any of the wait_* columns is TRUE. This columns will ease filtering.
  - fix a count of forest strata in fieldwork 2025 that were previously
    misjudged as not being part of MHQ samples in the fieldwork

- add several interactive helper scripts

This version serves as updated input for fieldwork preparation and optimization.

# REP 0.13.1 (2025-09-08)

tag `poc_0.13.1`

This version contains following updates. Those marked with (*) enable fixes in the
application of the response design in forest units.

- (*) VBI-locations that overlap with sampling units are stored and saved in the RData
  file
- in the helper script that supports data creation to organize fieldwork:
  - don't advertise the shorter object flavour of fieldwork 2025 prioritization,
    as it is misleading wrt (terrestrial) fieldwork amount, which better uses
    the 'by_stratum' flavour
  - (*) add an 'in_mhq_samples' column in several objects, including the 2025 fieldwork
    object, to detect more forest cell units that should have a large part of their
    surface as a no-entry zone.
    Before, only the 'last_type_assessment_in_field' column was used (TRUE means MHQ),
    BUT we must also use the 'in_mhq_samples' column (TRUE also means MHQ) to detect
    plots that are in MHQ monitoring. So for forests use:
    is_forest & (last_type_assessment_in_field | in_mhq_samples)
  - (*) include code that looks into vbi_overlaps to show a few other locations where
    care must be taken. Here, the no-entry zone should be based on the VBI coordinates
    as the center. Since these locations are not centered on the MNE sampling units,
    this is not used here as attributes of sampling units.
- accommodate selective updates of GRTS thresholds
- add several interactive helper scripts

This version introduces almost no changes to results. Spatial samples,
spatiotemporal samples and FAG calendar are unaffected.

# REP 0.13.0 (2025-08-08)

tag `poc_0.13.0`

This version contains following updates:

- next to domains, which can overlap, the concept of 'domain partition' (column
domain_part) is introduced. Domain partitions are mutually exclusive in space.
It is used to uniquely characterize each GRTS address, preferring its
membership to the smallest (innermost) domain it belongs to, whereas the
remainder of the larger (including) domain is regarded as the domain partition
labelled with suffix '_remainder'.
  - The membership of GRTS addresses to domain partitions is defined in a new
    object domainpart_grts_n2khab (similar to domain_grts_n2khab), which also
    defines the domain_part levels.
  - Note that both sp_poststratum and domain_part are needed in inferences
    about a larger domain, while domain_part is needed for inferences (or just
    counts) about a specific domain partition. This is further explained in the
    text. While values of sp_poststratum have some overlap with domain_part, don't
    use sp_poststratum to characterize a GRTS address; it is contingent on scheme x
    module combo x panel split x stratum, and serves specific purposes. For the same
    address it can sometimes present different values in different cases.
  - Also note that domains that are not used to drive sampling, are used in
    domain_part, not in sp_poststratum.

- in the helper script that supports data creation to organize fieldwork:
  - fix regex of wait_watersurface
  - implement domain_part in location attributes and objects that use it
  - implement a new, refined version of fieldwork 2025 priorities (values 1-9
    instead of 1-5), also including domain_part (useful for subprioritization)
    and a selection of groundwater FAGs, planned in 2026 but allowed to perform
    earlier.
  - complete the generation of replacement cells by fixing the cases where the
    polygon was missing (due to the fact that the habitatmap polygon doesn't contain
    the type, which may point at bugs in data)
  - define a classification of types (hence strata) to determine the applicable
    rule for maximum piezometer/well depth (discussed with @TomDeDobbelaer and
    @karenwuyts).

Apart from the helper script, this version introduces almost no changes to POC
results, just an extra column in domains, the new object
domainpart_grts_n2khab, and some minor updates wrt functions in RData file.


# REP 0.12.0 (2025-07-01)

tag `poc_0.12.0`

This version contains following updates:

- enhance n2khab_data verification (also adding DOI, better messages etc) and
move it to a function. Include related objects in the RData file to make this
portable.
- add a shell script with a typical command to render the bookdown project.
- include some minor fixes.

This version introduces zero changes to results, including spatial samples,
spatiotemporal samples and FAG calendar.

# REP 0.11.0 (2025-06-26)

tag `poc_0.11.0`

This version contains following updates:

- upgrade two n2khab_data resources that directly feed the base sampling frame:
  - habitatmap_terr_2023_v1 to habitatmap_terr_2024_v99_interim
  - watersurfaces_hab_v6 to watersurfaces_hab_v6.1_interim
  In this process, a final reset has been performed of the GRTS partition
  thresholds.
  From inspection, it appears that the spatiotemporal sample stabilization
  mechanisms (see poc_0.10.0) work well.
- cope with new combinations of stratum x spatial poststratum x panel set in the
  samples, in the storage of virtual panel addresses: this is now dealt with
  automatically, not overwriting already existing ones.

This version serves as updated input for fieldwork preparation and optimization.

# REP 0.10.0 (2025-06-26)

tag `poc_0.10.0`

This version contains following updates:

- stabilize randomization processes where possible in order to protect against
unnecessary changes in space & time whenever the sampling frame or the sampling
approach is updated:
  - stabilize the GRTS thresholds object as git2rdata file and use it.
  - favour temporal GRTS sampling over local pivotal sampling of date intervals
in getting a randomized well-spread selection of generic panels to distribute
the remainder of a spatial sample over panels. The addresses are stored for
48 virtual panels and used to stabilize this panel membership among different
spatial sample sizes, within stratum x spatial poststratum x panel set, hence
across schemes. At the same time, this maximizes spatiotemporal synergy across
schemes.
- streamline (input & output) data management wrt versioning, reproducibility and
sharing:
  - harden the use of data source versions in order to improve reproducibility:
    - set and check the data source versions from n2khab_data.
    - track local copies of text files from other repositories.
  - in each POC run, write the base sampling frame as a parquet file (untracked).
  - in each POC run, write the spatial samples and the spatiotemporal samples as
    (untracked) git2rdata files. These can be versioned in n2khab-sample-admin.
  - provide versioned textfiles to easily spot changes in results:
    - a file with object checksums (and also add these in the appendices)
    - a file with examples extracted from the spatiotemporal sample
- in the helper script that supports data creation to organize fieldwork, add
code to support orthophoto assessments.

This version serves as updated input for fieldwork preparation and optimization.

# REP 0.9.0 (2025-06-17)

tag `poc_0.9.0`

This version contains following updates:

- add an extra type in two densification submodules: 2190_mp in GW & SOIL schemes.
- implement two panel sets for a split panel design in GW, SURF & SOIL schemes,
and provision for a high number of spare units, since this gap between panel
sets 1 and 2 also serves as buffer for future sample densifications in one
or both panel sets. Each panel set is characterized by its own revisit design.
Panel set 1 gets 3/4 of the spatial sampling units and must represent the main
variation, and/or the variation of main interest. Panel set 2 gets the remaining
1/4.
  - update the fieldwork organization code snippets accordingly, including
priority rules for fieldwork 2025.
  - already provision for more locations and less repetitions in future
updates of SURF_03.4_* schemes.
- effectively implement a split panel design in GW_03.3 (notations follow
McDonald 2003):
  - panel set 1 receives a periodic rotational pattern for the year level,
	organized quarterly as [1,3,1,19].
  - panel set 2 receives a serially alternating pattern for the year level,
	organized quarterly as [1,1,1,21].
  These panel designs refer to the target field activity (main data collection
method of the target variable in a scheme), but their consequences for the
auxiliary field activities have also been implemented.
- improve the determination of GRTS partition thresholds for panel sets or
module combinations. Also, take measures in drawing spare units in the case of
overlapping domains between modules.
- only allow two different panel designs (~ panel sets or module combinations)
in a single scheme, as the population size of many strata is limited. This
prepares for fixing the GRTS partition thresholds for the future. This decision
has also led to dropping some modules from the params$modules default, only keeping
pan_effectmon_flanders and pan_effectmon_sac5_custommeas.
- add a small increase of the smallest sample sizes at the Flemish level (mostly
in panel set 2)
- fix the fact that READDIVER FAGs were still allowed to happen in the same date
interval as their corresponding INST FAG. Now they can only happen in a date interval
that starts at least 3 months later.
- for now, implicitly assume that habitat mapping in terrestrial cells must happen
in all MNE LOCEVALTERR occasions except 7220. This has led to stop dropping
LOCEVALTERR occasions based on available recent assessment from biotic monitoring,
since the cell mapping is never available.

This version serves as updated input for fieldwork preparation and optimization. It
also served in answering SV896.

Reference:

McDonald T.L. (2003). Review of Environmental Monitoring Methods: Survey Designs.
  Environmental Monitoring and Assessment 85 (3): 277–292.
  https://doi.org/10.1023/A:1023954311636.

# REP 0.8.0 (2025-05-27)

tag `poc_0.8.0`

This version contains following updates:

- update the helper script that supports data creation to organize fieldwork:
  - with regard to generating local replacement cells for terrestrial units:
    - update condition to split polygon in terr local replacement
    - add cells from next level3-cell if first level3-cell has not enough
      candidates
    - filter replacement cells in polygon to exclude those 'reserved by samples
      (in stratum)'
  - include field activity sequences
- fix a bug in determining absence assessments to be excluded from
  the base sampling frame
- update some terms (panel split -> panel set; main field method ->
  main field activity)
- update sample sizes of submodules

This version serves as updated input for fieldwork preparation and optimization.

# REP 0.7.0 (2025-05-19)

tag `poc_0.7.0`

This version contains following updates:

- add a targetpanel column in the spatiotemporal samples object
- improve the usage of spatial subset in the panel labels (only if needed)
- implement updated biotic FAG timings of dune types
- track scheme, module combo & panel split in the final FAG calendar:
  which ones does a FAG occasion (row) serve?
- only optionally keep the ADHOC activities in the FAG calendar
- add a helper script that supports data creation to organize fieldwork:
  - create relevant sampling unit attributes
  - get sampling unit geometries
  - get local replacement cells for terrestrial strata
  - FAG occasions, field activities and field variables

This version serves as updated input for fieldwork preparation and optimization.

# REP 0.6.0 (2025-05-07)

tag `poc_0.6.0`

This version contains following updates:

- spatial subset (in_aquatic_subset) is now always explicit, regardless of FAG (23eefcfa)
- flexibility has been added wrt module x compartment first year & month of implementation
- updates to the base sampling frame:
  - an updated version of mhq_terr_popunits has been implemented
  - for forest types, assessed absences from maximum 12 y ago are used instead of 6 y ago
- FAG-calendar:
  - date intervals of FAGs with LOCEVALxxx or LSVIxxx activities have been made type-specific in most cases.
    Previously, these intervals were from 1 Jan to 31 Dec; now they are typically much more narrow.
  - the determination of (to be dropped) 'superfluous revisits' for FAGs with LOCEVALxxx activities got two new
    features: 1) the used FAG cycle has been made type-dependent (3, 6 or 12 y); 2) this determination now also
    takes into account existing historical assessments.
  - the concept of simplifiable FAGs has been extended to also include LOCEVALAQ and SURF{LOT,LENT}EVALSAMPLPOINT
    FAGs. When the former and the latter are both present in the same FAG cycle at a single location, the former is
    promoted to the latter so that this counts in subsequent dropping of superfluous revisits, where only the first
    occasion is kept.

This version serves as updated input for fieldwork preparation and optimization.

# REP 0.5.0 (2025-04-29)

tag `poc_0.5.0`

This version contains following updates:

- implement the submodule concept to be able to optionally extend spatial sample sizes of specific strata
- add densification submodules in panfl & pan5 modules to increase sample size of specific strata in specific schemes
- always define (number) the panels starting from a fixed year_origin being 2024, in order to align the monitoring
cycle with the reporting cycle
- drop forests from MHQ schemes
- fix for MHQ schemes: use correct (much lower) sample size)

Regarding modules panfl & pan5, this version was used to update spatial samples for VMM.

# REP 0.4.1 (2025-03-21)

tag `poc_0.4.1`

This version fixes the missingness of certain watersurfaces units (see 684da803).

Regarding modules panfl & pan5, this version was used to generate input data for to support the challenge to
delineate areas or determine directions where piezometer and observation well should be installed in the
vicinity of an aquatic population unit in groundwater schemes; see n2khab-mne-monitoring repo.

# REP 0.4.0 (2025-03-19)

tag `poc_0.4.0`

This version contains some important updates:

- phab-correction of the base sampling frame
- inclusion of Flemish MHQ assessments in the base sampling frame, where these meet the MNE requirements
- comparison (in separate notebook) of Flemish MHQ sample coverage by the base sampling frame and by the MNE samples for modules panfl & pan5, either without or with phab-correction of the base sampling frame
- adding more attributes & filters in creating a spatial samples object for VMM

Regarding modules panfl & pan5, this version was used to deliver spatial samples for VMM.

# REP 0.3.1 (2025-02-13)

tag `poc_0.3.1`

This version contains two important fixes:

- allow module combos without MHQ schemes (3b18e10)
- fix the cycle duration of aquatic MHQ schemes as applied to calculate the spatial sample
sizes for one cycle

This version serves as input for fieldwork optimization and for spatial samples for VMM.

# REP 0.3.0 (2025-02-03)

tag `poc_0.3.0`

This version contains many updates, notably a module-agnostic and much more elaborated approach (many more field activities) to create the revisit design, spatial unit FAG calendar & spatiotemporal samples.

Regarding modules panfl & pan5, this version was used for:

- input for fieldwork optimization
- example spatial samples for VMM
- example spatial samples for code wrt spatial coupling of piezometers

# REP 0.2.0 (2024-07-30)

tag `poc_0.2.0`

Version 0.2.0, used for fieldwork optim 2024-07-12.

This version was tailored to produce an RData file for fieldwork
optimization (in dir 100_design_common/030_...), specifically
for the pan_phase_1_flanders module.

It should be noted that this version does not (yet) support handling of
different modules at the same time, since the code added for the revisit
design is currently still heavily customized for pan_phase_1_flanders.

# REP 0.1.0 (2024-06-26)

tag `poc_0.1.0`

Version 0.1.0, used to create DCP MBAA 2024-06-21
