################################################################################

# Miscellaneous functions

execshell <- function(commandstring, intern = FALSE) {
    if (.Platform$OS.type == "windows") {
        res <- shell(commandstring, intern = TRUE)
    } else {
        res <- system(commandstring, intern = TRUE)
    }
    if (!intern) cat(res, sep = "\n") else return(res)
}


####
# A logging function for debugging parallel code in an interactive session.

log_socket <- function(text, ..., .cat = FALSE, .socket) {
    msg <- sprintf(paste0(as.character(Sys.time()), ": ", text, "\n"), ...)
    if (.cat) cat(msg)
    write.socket(.socket, msg)
}

# insert something like this in your code:
# log_socket("Processing block %d of %d", i, j, .socket = logsocket)
####




#####################################################################
#corvif() FUNCTION.
#From:
    #Mixed effects models and extensions in ecology with R. (2009).
    #Zuur, AF, Ieno, EN, Walker, N, Saveliev, AA, and Smith, GM. Springer.
#Modified by @florisvdh

corvif <- function(dataz) {
    dataz <- as.data.frame(dataz)

    #vif part
    form    <- formula(paste("fooy ~ ",paste(strsplit(names(dataz)," "),collapse=" + ")))
    dataz   <- data.frame(fooy=1 + rnorm(nrow(dataz)) ,dataz)
    lm_mod  <- lm(form,dataz)

    vif <- myvif(lm_mod)
    covariates <- rownames(vif)
    vif %>%
        dplyr::as_tibble() %>%
        dplyr::select(last_col()) %>%
        dplyr::mutate(covariate = covariates) %>%
        dplyr::rename(vif = 1) %>%
        dplyr::select(2, 1)
}

#Support function for corvif. Will not be called by the user
myvif <- function(mod) {
    v <- vcov(mod)
    assign <- attributes(model.matrix(mod))$assign
    if (names(coefficients(mod)[1]) == "(Intercept)") {
        v <- v[-1, -1]
        assign <- assign[-1]
    } else warning("No intercept: vifs may not be sensible.")
    terms <- labels(terms(mod))
    n.terms <- length(terms)
    if (n.terms < 2) stop("The model contains fewer than 2 terms")
    if (length(assign) > dim(v)[1] ) {
        diag(tmp_cor)<-0
        if (any(tmp_cor==1.0)){
            return("Sample size is too small, 100% collinearity is present")
        } else {
            return("Sample size is too small")
        }
    }
    R <- cov2cor(v)
    detR <- det(R)
    result <- matrix(0, n.terms, 3)
    rownames(result) <- terms
    colnames(result) <- c("GVIF", "Df", "GVIF^(1/2Df)")
    for (term in 1:n.terms) {
        subs <- which(assign == term)
        result[term, 1] <- det(as.matrix(R[subs, subs])) * det(as.matrix(R[-subs, -subs])) / detR
        result[term, 2] <- length(subs)
    }
    if (all(result[, 2] == 1)) {
        result <- data.frame(GVIF=result[, 1])
    } else {
        result[, 3] <- result[, 1]^(1/(2 * result[, 2]))
    }
    return(result)
}
#END VIF FUNCTIONS


################################################################################

# Functions used to retrieve file metadata

get_latest_filecommit <- function(filepath, reporoot, repostatus) {
    label <-
        execshell(paste0("git log -n 1 --pretty=format:'`%H` (%ai)' -- '",
                         filepath, "'"),
           intern = TRUE)
    gitpath <- fs::path_rel(filepath, reporoot)
    if (gitpath %in% repostatus$unstaged) {
        paste("UNCOMMITTED! Modified relative to latest file commit:",
              label) %>%
            return
    } else return(label)
}

get_vc_datahash <- function(datafile) {
    file_extension <- substr(datafile, nchar(datafile) - 3, 1000000L)

    if (substr(file_extension, 1, 1) != ".") {
        datafile <- paste0(datafile, ".yml")
    } else {
        if (file_extension != ".yml") {
        stop("You must provide a vc-data object name or the yml file name.")
        }
    }

    yaml::read_yaml(datafile) %>%
    .$..generic %>%
    .$data_hash
}

enclose <- function(x, y) paste0(y, x, y)



################################################################################

# Functions used to aid model building


invsqrt <- function(x) 1 / sqrt(x)


################################################################################

# Functions used to aid reproducible scenario simulation


#' Split spatial population size proportional to the distribution along a spatial factor
#'
#' @param spfact Currently "soilclass" and "ecoregion" are supported
split_popsize <- function(df, spfact) {
    left_join(df,
              switch(spfact,
                     "soilclass" = targetpop_soilclass_distr,
                     "ecoregion" = targetpop_ecoregion_distr) %>%
                  filter(scheme == scheme_sel) %>%
                  select(type, {{spfact}}, proportion),
              by = "type") %>%
        mutate(type = droplevels(type)) %>%
        group_by(type) %>%
        mutate(population_size = (first(population_size) * proportion) %>% round) %>%
        ungroup %>%
        select(-proportion) %>%
        relocate(population_size, .after = last_col())
}


#' Uncount population_size but limit population_size, taking into account distribution over spatial factor (for each type)
#' @param fixed If TRUE, substitute sum(population_size) over the spatial factor with the limit value (insisting on equal population_sizes, apart from rounding error)
uncount_limited <- function(df, limit, fixed = FALSE) {
    df %>%
        group_by(type) %>%
        mutate(population_size =
                   round(ifelse(fixed, limit, min(sum(population_size), limit)) *
                             population_size / sum(population_size))) %>%
        ungroup %>%
        uncount(population_size)
}



#' Define location IDs and unfold temporal dimension
add_st <- function(type_attrib, time = 1:12) {
    type_attrib %>%
    mutate(location =
           str_c("simloc_",
                 str_pad(seq_len(n()),
                         8, pad = "0")) %>%
           as.factor) %>%
    relocate(location) %>%
    crossing(time = time)
}



#' Simulate populations from posterior mean fixed and hyperparameter values
#'
#' This function also omits the long-term trend component.
#'
#'
#' @param npop number of populations to simulate.
#' They only differ by their used random effect values, which still originate
#' from the same set of parameters (posterior means of fixed and
#' hyperparameters)
#' @param design_matrix defines the size and fixed + random level configuration
#' of one (and each) population
#' @param var_stratum variable by which residual distribution has been split
#' @param var_stratum_ variable by which temporal or spatial variation is
#' stratified; it can have less levels than the orginal variable it is based on,
#' because certain levels don't have enough timeseries data.
#' @param keep_ranef should the result contain the separate
#' random effects and residuals?
#' @param keep_predfixed should the result contain the total fixed effect?
simulate_detrended_pops <-
    function(design_matrix,
             npop = 20,
             var_time,
             keep_ranef = FALSE,
             keep_predfixed = FALSE,
             seed = NULL) {
        # colnames(model$model.matrix)
        # rownames(model$summary.fixed)
        # model$names.fixed
        # model$.args$data
        # latent_names <- model$misc$configs$contents$tag
        if (!is.null(seed)) set.seed(seed)

        design_matrix %>%
            nest(design_matrix = -c(type, modelname)) %>%
            rowwise %>%
            mutate(
                model = list(modelname %>% as.character %>% str2lang %>% eval)) %>%
            ungroup %>%
            mutate(
                design_matrix = map2(design_matrix, model,
                                      function(dm, model) {
                      var_stratum_ <-
                          if (any(str_detect(colnames(model$model.matrix),
                                             "stratum_(light|heavy|peat)"))) {
                              "soilclass"
                          } else if (any(str_detect(colnames(model$model.matrix),
                                                    "stratum_.*(polders|Kempen)"))) {
                              "ecoregion"
                          } else "type"

                      var_stratum <-
                          if (any(str_detect(names(model$summary.random),
                                             "light|heavy|peat"))) {
                              "soilclass"
                          } else if (any(str_detect(names(model$summary.random),
                                                    "polders|Kempen"))) {
                              "ecoregion"
                          } else "type"

                    dm %>%
                        rename(type = modelterm_type) %>%
                        `colnames<-`(colnames(.) %>% replace(. == "time", var_time)) %>%
                        mutate(type =
                                   type %>%
                                   factor(levels = levels(model$.args$data$type)),
                               stratum_ =
                                   .[[var_stratum_]] %>%
                                   factor(levels = levels(model$.args$data$stratum_)),
                               stratum =
                                   .[[var_stratum]] %>%
                                   factor(levels = levels(model$.args$data[[var_stratum]]))) %>%
                        {if (any(is.na(.$stratum_))) select(., -stratum_) else .}
                }),
                formula_fixed =
                    map2(design_matrix, model, function(dm, model) {
                        model$.args$formula %>%
                            as.formula %>%
                            terms %>%
                            attr("term.labels") %>%
                            .[!str_detect(.,
                                          # keeping only fixed effects, and excluding long-term
                                          # trend:
                                          "f\\(|:.*year|I\\(.*year|year_std$")] %>%
                            {if ("stratum_" %in% colnames(dm)) . else .[!str_detect(., "stratum_")]} %>%
                            paste(collapse = " + ") %>%
                            {if ("(Intercept)" %in% model$names.fixed) {
                                paste("~", .) } else paste("~-1 +", .)
                            } %>%
                            as.formula}),
                model_matrix = map2(design_matrix, formula_fixed,
                                    ~model.matrix(.y, data = .x)),
                # calculate fixed part
                prediction_fixed = map2(model_matrix, model,
                                       function(mm, model) {
                                           if(any(rownames(model$summary.fixed)[rownames(model$summary.fixed) %in% colnames(mm)] != colnames(mm))) stop("The order of model matrix columns does not match that of the fixed effects.")
                                           pars_fixed <- model$summary.fixed[
                                               rownames(model$summary.fixed) %in% colnames(mm), "mean"]
                                           as.numeric(mm %*% pars_fixed)

                                       }),
                design_matrix = map(design_matrix, ~rename(., modelterm_type = type))
            ) %>%
            select(-formula_fixed, -model_matrix, -model) %>%
            nest(design_modelres = -modelname) %>%
            mutate(design_modelres = map(design_modelres,
                                    ~unnest(., c(design_matrix, prediction_fixed)))) %>%
            crossing(population = str_c("population_", str_pad(1:npop, 5, pad = "0")) %>% as.factor) %>%
            rowwise %>%
            mutate(
                model = list(modelname %>% as.character %>% str2lang %>% eval)) %>%
            ungroup %>%
# different populations only need to be accommodated from this point on (they share their fixed prediction). Also there's the need to implement modelnames (within population)
            mutate(
                design_modelres =
                    map2(design_modelres, model,
                         function(design_modelres, model) {
                             design_modelres %>%
                                 # spatial noise
                                 group_by(location) %>%
                                 {if ("loc_code" %in% names(model$summary.random)) {
                                 mutate(., ranef_loc =
                                            rnorm(1,
                                                  sd = invsqrt(model$summary.hyperpar["Precision for loc_code", "mean"])))} else .} %>%
                                 {if ("cluster_id" %in% names(model$summary.random)) {
                                     mutate(., ranef_clus =
                                                rnorm(1,
                                                      sd = invsqrt(model$summary.hyperpar["Precision for cluster_id", "mean"])))} else .} %>%
                                 {if (any(str_detect(names(model$summary.random),
                                                     "cluster_stratum_"))) {
                                     group_by(., location, stratum_) %>%
                                         mutate(spatial_noise =
                                                    rnorm(1,
                                                          sd = invsqrt(model$summary.hyperpar[str_c("Precision for cluster_stratum_", stratum_), "mean"])))} else .} %>%
                                 # temporal noise
                                 {if (var_time %in% names(model$summary.random)) {
                                     group_by(., .data[[var_time]]) %>%
                                     mutate(ranef_time =
                                                rnorm(1,
                                                      sd = invsqrt(model$summary.hyperpar[str_c("Precision for ", var_time), "mean"])))} else .} %>%
                                 {if (any(str_detect(names(model$summary.random),
                                                     str_c(var_time, "_stratum_")))) {
                                     group_by(., .data[[var_time]], stratum_) %>%
                                         mutate(temporal_noise =
                                                    rnorm(1,
                                                          sd = invsqrt(model$summary.hyperpar[str_c("Precision for ", var_time, "_stratum_", stratum_), "mean"])))} else .} %>%
                                 # residual noise
                                 group_by(stratum) %>%
                                 mutate(resid_noise =
                                            rnorm(n(),
                                                  sd = invsqrt(model$summary.hyperpar[str_c("Stratum ", stratum, ": Precision of residuals"), "mean"]))) %>%
                                 ungroup %>%
                                 # calculate response
                                 mutate(response =
                                            rowSums(across(c(
                                                prediction_fixed,
                                                starts_with("ranef"),
                                                ends_with("noise"))))
                                 ) %>%
                                 select(-modelterm_type, -stratum_, -stratum)

            }
            )) %>%
            select(-model) %>%
            unnest(design_modelres) %>%
            relocate(population) %>%
            relocate(modelname, .after = last_col()) %>%
            {if (keep_ranef) . else {
                select(., -starts_with("ranef"), -ends_with("noise"))
            }} %>%
            {if (keep_predfixed) . else {
                select(., -prediction_fixed)
            }} %>%
            arrange(population, location, .data[[var_time]])

    }




#' Simulate spatial samples from given populations and sample definitions (including artificial trend)
#'
#' @param sample_definition Data frame that defines the constitution of a sample for each scenario.
#' Minimal columns needed: scenario, trend_12yearly_multiplier, type, n_finitepop_spatial
#' @param population_data Data frame with data of multiple (full) population realizations, with specific required columns: population, type, location, {{var_time}}, prediction_fixed, response and modelname.
#' @param var_time String. The name of the time variable in `population_data`.
#' @param npops Number of populations to select from population_data (the first `npops` populations are used)
#' @param pops Optional character vector of population names to select.
#' If specified, npops is ignored.
#' @param nsamples_per_pop Number of samples to take per population (without replacement)
#' @param sampling If TRUE (default), return repeated spatial samples from each population.
#' If FALSE, simply return the full data of each (selected) population,
#' with the artificial trend of each scenario added.
#'
simulate_trended_spatial_samples <- function(sample_definition,
                                             population_data,
                                             var_time,
                                             npops = length(unique(population_data$population)),
                                             pops = NULL,
                                             nsamples_per_pop = 20,
                                             sampling = TRUE,
                                             seed = NULL){
    if (!is.null(seed)) set.seed(seed)

    pops_missing <- missing(pops)

    if (!pops_missing && !missing(npops)) {
        warning("If you specify the populations with pops, then npops is ignored.")
    }

    prediction_spatial_term <-
        names(population_data) %>%
        str_subset("prediction_fixed|spatial_|_loc|_clus") %>%
        paste(collapse = " + ")

    trended_pop_data <-
        sample_definition %>%
        nest(scen_attrib = -scenario) %>%
        crossing(population_data %>%
                     {if (!pops_missing) {
                         filter(., population %in% pops)
                     } else {
                         filter(., str_sub(population, start = -5L) %>%
                                    as.numeric <= npops)
                     }} %>%
                     select(-c(modelname,
                               matches("ranef_time|temporal_noise|resid_noise"))) %>%
                     nest(pop_data = -population)) %>%
        # adding trend to predicted value per location (this intermediate result is
        # below called spatial_term):
        mutate(pop_data = map2(scen_attrib, pop_data, function(s, p) {
            p %>%
                inner_join(s %>% select(-popsize_spatial),
                           by = "type") %>%
                mutate(prediction_spatial = eval(str2lang(prediction_spatial_term)),
                       spatial_term = prediction_spatial *
                           (trend_12yearly_multiplier^(.data[[var_time]]/12)),
                       response =
                           response -
                           prediction_spatial +
                           spatial_term) %>%
                select(-c(prediction_fixed,
                          matches("spatial_noise|_loc|_clus"),
                          prediction_spatial,
                          trend_12yearly_multiplier)) %>%
                relocate(response, .after = last_col())
        }))

    if (!sampling) {
        return(
            trended_pop_data %>%
                select(-scen_attrib) %>%
                unnest(pop_data) %>%
                select(-n_finitepop_spatial))
    }
        # simulating repeated spatial samples:
    trended_pop_data %>%
        mutate(sample_data = list(tibble(spatial_sample =
                                             str_c("sample_",
                                                   str_pad(1:nsamples_per_pop, 4, pad = "0")) %>%
                                             factor)),
               sample_data = map2(pop_data,
                                  sample_data,
                                  function(p, df) {
                                      df %>%
                                          mutate(units = map(spatial_sample, ~
                                             p %>%
                                             distinct(type,
                                                      location,
                                                      n_finitepop_spatial) %>%
                                             nest(locs = location) %>%
                                             mutate(sample = map2(locs, n_finitepop_spatial,
                                                                  ~slice_sample(.x, n = .y))) %>%
                                             select(-locs, -n_finitepop_spatial) %>%
                                             unnest(sample)
                                          ),
                                          data = map2(spatial_sample, units, ~
                                                          p %>%
                                                          select(-n_finitepop_spatial,
                                                                 -spatial_term) %>%
                                                          semi_join(.y,
                                                                    by = c("type",
                                                                           "location"))
                                          )
                                          ) %>%
                                          select(-units) %>%
                                          unnest(data)
                                  })) %>%
        # drop scen_attrib and pop_data:
        select(-scen_attrib, -pop_data) %>%
        unnest(sample_data)
}




#' Visualize selected data of 1 population with scenario trends added
#'
#'
visualize_trends <- function(sample_definition,
                             population_data,
                             population,
                             ntypes = 9,
                             var_time,
                             seed = NULL) {

    if (!is.null(seed)) set.seed(seed)
    assertthat::assert_that(assertthat::is.string(population))

    sample_definition %>%
        # select 1 scenario per artificial trend:
        nest(data = -c(scenario, trend_12yearly_multiplier)) %>%
        group_by(trend_12yearly_multiplier) %>%
        slice_head %>%
        unnest(data) %>%
        # add the trends to 1 simulated population:
        simulate_trended_spatial_samples(population_data = population_data,
                                         pops = population,
                                         var_time = var_time,
                                         sampling = FALSE) %>%
        # use only 2 locations per type and ntypes types
        select(-population) %>%
        nest(type_data = -type) %>%
        slice_sample(n = ntypes) %>%
        mutate(type_data = map(type_data, function(df) {
            df %>%
                nest(location_data = -location) %>%
                slice_sample(n = 2) %>%
                unnest(location_data)})) %>%
        unnest(type_data) %>%
        arrange(type, location, scenario) %>%
        split(~type, drop = TRUE) %>%
        map(~ggplot(., aes(x = .data[[var_time]],
                           y = response,
                           colour = scenario,
                           group = scenario:location)) +
                geom_line() +
                facet_grid(location ~ type)
        ) %>%
        wrap_plots(ncol = 3, guides = "collect") +
        plot_annotation(title = population) &
        theme(legend.position = "bottom")
}



#' Compute sample status
#'
#' Computes status (a global quantity) of a target variable, notably its
#' spatial or spatiotemporal mean with standard error and error margin.
#' Note that the target variable can also be a location specific trend estimate,
#' in which case the mean trend estimate will emerge.
#'
#' @param statusdata Data frame with at least columns "scenario", "population",
#' "spatial_sample", "type", "location", "population_size", and a column with
#' the value of the target variable (conveniently named as "targetvar").
#' @param level Level at which the status estimate must be computed.
#' For levels higher than type, the design is always stratified according to
#' type.
#' @param weighted_mean Logical; only relevant for level higher than type.
#' Should means be calculated as usual in a stratified design, i.e.
#' weighing types according to their relative population size?
#' The default (FALSE) gives equal weight to types and standard error is
#' calculated accordingly (equal weights of the type standard errors).
#' This is done since it is the global quantity of primary interest.
#' @param targetvar String. Name of the target variable.
#' @param extra_se_var String (NULL by default).
#' Name of a variable containing the standard errors
#' associated with the values of targetvar, and which should be incorporated
#' into the standard error of the spatial or spatiotemporal mean.
#' @param typeresult The outputted dataframe of the function for level="type"
#' can be inputted again; will be used to shortcut calculations if
#' weighted_mean = FALSE and level is higher than type.
#' In this case, statusdata is not needed.
#'
compute_status_persample <- function(statusdata = NULL,
                                     level = c("typegroup", "type", "overall"),
                                     weighted_mean = FALSE,
                                     targetvar = "targetvar",
                                     extra_se_var = NULL,
                                     typeresult = NULL) {

    add_rel <- function(df) {
        mutate(df,
               twosided_errmarg80_rel =
                   twosided_errmarg80 / abs(mean),
               twosided_errmarg90_rel =
                   twosided_errmarg90 / abs(mean))
    }

    if (is.null(typeresult) | level == "type" | weighted_mean) {

    mean_se <-
        statusdata %>%
        {if (!weighted_mean | level == "type") {
            nest(., data = -c(scenario,
                              population,
                              spatial_sample,
                              type, # type is required
                              starts_with("type"))) # typegroup is possibly present
        } else if (level == "overall") {
            nest(., data = -c(scenario, population, spatial_sample))
        } else {
            nest(., data = -c(scenario, population, spatial_sample, typegroup))
        }} %>%
        mutate(design = map(data, ~svydesign(ids = ~1,
                                             strata = if(!weighted_mean) NULL else ~type,
                                             fpc = ~population_size,
                                             data = .)),
               mean_svystat = map(design, ~svymean(paste0("~", targetvar) %>%
                                                       as.formula, .)),
               deg_freedom = map_dbl(design, ~degf(.)),
               mean = map_dbl(mean_svystat, ~coef(.)),
               se = map_dbl(mean_svystat, ~SE(.)))

    if (!is.null(extra_se_var)) {
        mean_se <-
            mean_se %>%
            mutate(mean_local_variance = map_dbl(design,
                                            ~svymean(paste0("~I(",
                                                            extra_se_var,
                                                            "^2)") %>%
                                                             as.formula, .) %>%
                                                coef),
                   variance_spatial_mean = se^2,
                   se = sqrt(mean_local_variance + variance_spatial_mean))
    }

    mean_se <-
        mean_se %>%
        select(-data, -design, -mean_svystat)

    if (weighted_mean | level == "type") {
        return(
            mean_se %>%
                mutate(twosided_errmarg80 = se * qt(1 - 0.2 / 2, df = deg_freedom),
                       twosided_errmarg90 = se * qt(1 - 0.1 / 2, df = deg_freedom)) %>%
                add_rel
        )
    }

    } else {
        mean_se <- typeresult
    }

    # calculating unweighted mean of types (overall or typegroup level):
    mean_se %>%
        group_by(scenario, population, spatial_sample) %>%
        {if (level == "typegroup") group_by(., typegroup, .add = TRUE) else .} %>%
        summarise(mean = mean(mean),
                  se = sqrt(sum(se^2)) / n()) %>%
        ungroup %>%
        mutate(twosided_errmarg80 = se * qnorm(1 - 0.2 / 2),
               twosided_errmarg90 = se * qnorm(1 - 0.1 / 2)) %>%
        add_rel
}


#' Summarize simsample statistics within and among populations
#'
#' For a given sample statistic, calculates mean & percentiles of its
#' distribution obtained by multiple sample
#' simulations (1 value per sample), stratified by populations.
#' The calculation is done for each scenario and type(group) in turn.
#'
#' @param merge_pops Results can be given per population or as an overall average (the default).
#' @param conflevel_pctiles The confidence level (between 0 and 1) that defines lower and upper percentiles (with probabilitie arranged symmetrically around P = 0.5).
#' @param qual_std Optional vector of one or more values of a targeted quality standard, i.e. one or more target values of the statistic, for which left-sided probabilities will be estimated from the ECDF.
#' @param plot Logical. Optionally returns a plot on condition that merge_pops = FALSE.
#' @param ylim NULL or a numeric vector of length 2. If a vector (has a default), is applied as y-limits in the plot.
#' @param facet_scales String. Always applied but only relevant if ylim = NULL.
#' @param ... Arguments passed to `facet_wrap()`.
#'
summarise_status_of_samples <- function(multisample_stats,
                                        statistic,
                                        conflevel_pctiles = 0.9,
                                        qual_std = NULL,
                                        merge_pops = TRUE,
                                        plot = FALSE,
                                        ylim = c(0, 1),
                                        facet_scales = "free_x",
                                        ...) {
    stopifnot(between(conflevel_pctiles, 0, 1))
    stopifnot(is.numeric(qual_std) || is.null(qual_std))
    result <-
        multisample_stats %>%
        nest(data = -c(scenario, contains("type"))) %>%
        mutate(summ = map(data, function(df) {
            df %>%
                select(population, !!statistic) %>%
                group_by(population) %>%
                summarise(avg = mean(.data[[statistic]]),
                          pctile_l = quantile(.data[[statistic]], (1 - conflevel_pctiles) / 2),
                          pctile_u = quantile(.data[[statistic]], 1 - (1 - conflevel_pctiles) / 2),
                          if (!is.null(qual_std)) {
                              across(.data[[statistic]],
                                 map(qual_std, ~function(x) ecdf(x)(.)) %>%
                                     set_names(str_c("p(stat<", qual_std, ")")),
                                 .names = "{.fn}")
                              }) %>%
                {if(merge_pops) summarise(., across(-population, ~mean(.))) else .}
        })) %>%
        select(-data) %>%
        unnest(cols = summ) %>%
        relocate(contains("type")) %>%
        {if(any(str_detect(colnames(.), "type"))) {
            arrange(., across(contains("type")), scenario) } else {
                arrange(., scenario)}}

    if(plot & !merge_pops) {
        result %>%
            unite("scen_pop", scenario, population,
                  remove = FALSE) %>%
            mutate(scen_pop = factor(scen_pop) %>% fct_rev) %>%
            {ggplot(., aes(x = scen_pop,
                       y = avg,
                       ymin = pctile_l,
                       ymax = pctile_u,
                       colour = scenario)) +
            geom_errorbar() +
            geom_point(size = 0.5, colour = "black", alpha = 0.4) +
            {if (!is.null(ylim)) lims(y = ylim) else NULL} +
            coord_flip() +
            {if("type" %in% colnames(result)) {
                facet_wrap(~type, scales = facet_scales, ...)
            } else if("typegroup" %in% colnames(result)) {
                facet_wrap(~typegroup, scales = facet_scales, ...)
            } else NULL} +
            theme(axis.text.y = element_blank(),
                  axis.ticks.y = element_blank()) +
            labs(x = "simulated populations", y = statistic)}
    } else return(result)
}





#' Calculate power for each population and power distribution among populations
#'
#' For a given sample statistic, calculates median and percentiles of
#' power, where each power value is based on the simulated samples from one
#' population. The power is for a two-sided test for comparison with zero.
#' The calculation is done for each scenario and type(group) in turn.
#'
#' @param multisample_stats dataframe with at least the sample means as column 'mean' and associated errormargin(s)
#' @param scenario_def Dataframe that defines the scenarios (one row per scenario).
#' Has at least columns "scenario", "avg_nrlocs_pertype_infpop" and a column trend_*.
#' One row per scenario.
#' @param merge_pops Results can be given as power per population or
#' aggregated (the default).
#' Ignored if plot is TRUE.
#' @param plot Logical. Optionally returns a plot.
#' @param ... Arguments passed to `facet_wrap()`.
#'
calculate_power_of_scenarios <- function(multisample_stats,
                                         scenario_def = NULL,
                                         merge_pops = TRUE,
                                         plot = FALSE,
                                         ...) {
    result <-
        multisample_stats %>%
        mutate(across(matches("errmarg\\d{2}$"),
                      ~mean - . > 0 | mean + . < 0,
                      .names = "trend_sign_{.col}")) %>%
        rename_with(.cols = matches("^trend_sign_"),
                    .fn = ~str_remove(., "twosided_errmarg")) %>%
        group_by(across(c(contains("type"), scenario, population))) %>%
        summarise(across(matches("^trend_sign_"), ~sum(.)/n())) %>%
        rename_with(~str_replace(., "trend_sign", "power_at_conflevel"))

    if(!plot) {
        return(
            result %>%
                {if (!merge_pops) . else {
                    summarise(., across(matches("^power_at_conflevel"),
                                        ~str_c(median(.) %>% round(2), " (",
                                               quantile(., 0.25) %>% round(2), " | ",
                                               quantile(., 0.75) %>% round(2), ")")))
                }})
    } else {
        assertthat::assert_that(!missing(scenario_def))
        trend_colname <-
            colnames(scenario_def) %>%
            {.[str_detect(., "trend_")]}
        type_typegroup_colname <-
            colnames(result) %>%
            {.[str_detect(., "type")]}
        result %>%
            ungroup %>%
            inner_join(scenario_def, by = "scenario") %>%
            rename_with(~str_remove(., "power_at_")) %>%
            pivot_longer(cols = matches("^conflevel"),
                         names_to = "conflevel",
                         values_to = "power") %>%
            mutate({{trend_colname}} := factor(.data[[trend_colname]])) %>%
            (function(df) {
                df_summ <-
                    df %>%
                    group_by(across(c(scenario,
                                      avg_nrlocs_pertype_infpop,
                                      matches("^trend_"),
                                      contains("type"),
                                      conflevel))) %>%
                    summarise(power = median(power))

                ggplot(df, aes(x = avg_nrlocs_pertype_infpop,
                               y = power,
                               colour = .data[[trend_colname]],
                               group = .data[[trend_colname]])) +
                    geom_jitter(alpha = 0.4, width = 3, height = 0) +
                    geom_line(data = df_summ) +
                    geom_point(data = df_summ,
                               shape = 3,
                               colour = "black") +
                    facet_wrap(formula(ifelse(length(type_typegroup_colname) == 0,
                                              "~conflevel",
                                              str_c("~",
                                                    type_typegroup_colname,
                                                    " + conflevel"))),
                               ...) +
                    xlab("average number of locations per type\n(before adjusting for spatial variance, population size and typegroup size)") +
                    theme(legend.position = "bottom")
            })
    }
}








