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



add_st <- function(type_attrib, time = 1:12) {
    type_attrib %>%
    mutate(location =
           str_c("simloc_",
                 str_pad(seq_len(n()),
                         8, pad = "0")) %>%
           as.factor) %>%
    relocate(location) %>%
    tidyr::expand(nesting(location, type, modelterm_type, modelname),
                  time = time)
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
#' @param var_stratum_ variable by which temporal variation is stratified; it
#' can have less levels than the orginal variable it is based on, because
#' certain levels don't have enough timeseries data
#' @param keep_ranef should the result contain the separate
#' random effects and residuals?
#' @param keep_predfixed should the result contain the total fixed effect?
simulate_detrended_pops <-
    function(design_matrix,
             npop = 20,
             var_time,
             var_stratum,
             var_stratum_,
             keep_ranef = FALSE,
             keep_predfixed = FALSE,
             seed = NULL) {
        # colnames(model$model.matrix)
        # rownames(model$summary.fixed)
        # model$names.fixed
        # model$.args$data
        # latent_names <- model$misc$configs$contents$tag
        if (!is.null(seed)) set.seed(seed)
        if (var_stratum == "type") var_stratum <- "modelterm_type"

        design_matrix %>%
            nest(design_matrix = -c(type, modelname)) %>%
            rowwise %>%
            mutate(
                model = list(modelname %>% as.character %>% str2lang %>% eval)) %>%
            ungroup %>%
            mutate(
                design_matrix = map2(design_matrix, model, function(dm, model) {
                    dm %>%
                        rename(type = modelterm_type) %>%
                        `colnames<-`(colnames(.) %>% replace(. == "time", var_time)) %>%
                        mutate(stratum_ =
                                   .data[[var_stratum_]] %>%
                                   factor(levels = levels(model$.args$data$stratum_))) %>%
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
# different samples only need to be accommodated from this point on (they share their fixed prediction). Also there's the need to implement modelnames (within sample)
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
                                 group_by(.data[[var_stratum]]) %>%
                                 mutate(resid_noise =
                                            rnorm(n(),
                                                  sd = invsqrt(model$summary.hyperpar[str_c("Stratum ", .data[[var_stratum]], ": Precision of residuals"), "mean"]))) %>%
                                 ungroup %>%
                                 # calculate response
                                 mutate(response =
                                            rowSums(across(c(
                                                prediction_fixed,
                                                starts_with("ranef"),
                                                ends_with("noise"))))
                                 ) %>%
                                 select(-modelterm_type, -stratum_)

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
#' @param population_data Data frame with data of multiple (full) population realizations, with specific required columns: population, type, location, hydroyear_std, prediction_fixed, response and modelname.
#' @param npops Number of populations to select from population_data (the first `npops` populations are used)
#' @param nsamples_per_pop Number of samples to take per population (without replacement)
#'
simulate_trended_spatial_samples <- function(sample_definition = sample_definition,
                                             population_data = simpops,
                                             npops = length(unique(population_data$population)),
                                             nsamples_per_pop = 20,
                                             seed = NULL){
    if (!is.null(seed)) set.seed(seed)

    sample_definition %>%
        nest(scen_attrib = -scenario) %>%
        crossing(population_data %>%
                     filter(str_sub(population, start = -5L) %>%
                                as.numeric <= npops) %>%
                     select(-modelname) %>%
                     nest(pop_data = -population)) %>%
        # adding trend to prediction_fixed (this intermediate result is
        # below called fixed_term):
        mutate(pop_data = map2(scen_attrib, pop_data, function(s, p) {
            p %>%
                inner_join(s %>% select(-popsize_spatial),
                           by = "type") %>%
                mutate(fixed_term = prediction_fixed *
                           (trend_12yearly_multiplier^(.data[[timevar]]/12)),
                       response = response - prediction_fixed + fixed_term) %>%
                select(-prediction_fixed, -trend_12yearly_multiplier) %>%
                relocate(response, .after = last_col())
        })) %>%
        # simulating repeated spatial samples:
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
                                                                 -fixed_term) %>%
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



