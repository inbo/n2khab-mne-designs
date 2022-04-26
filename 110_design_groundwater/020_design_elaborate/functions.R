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

# Functions to aid model diagnosis & evaluation

#' Plot a gstat variogram object
#' @param vg A gstat variogram object
plot_vg <- function(vg) {
    vg %>%
        rename(semivariance = gamma,
               distance = dist) %>%
        ggplot(aes(x = distance, y = semivariance, fill = np, label = np)) +
        geom_smooth(colour = "grey70", se = FALSE) +
        geom_point(size = 3, shape = 21) +
        ylim(0, NA) +
        scale_fill_viridis_c("Number of\npointpairs", option = "D", direction = -1) +
        geom_text(vjust = -1, size = 3, colour = "grey30") +
        theme_bw() +
        theme(legend.position = "bottom",
              legend.key.width = unit(0.08, "npc"))
}


#' Return observed values of a multiresponse model from R-INLA
#'
#' @param y The response element of the data element stored in the inla object's .args element
extract_observed <- function(y) {
    apply(y, 1, function(x) x[!is.na(x)]) %>%
        as.numeric
}

#' Give the error 'model objects are missing'
error_missing_modelobjects <- function() {
    stop("Please rerun this report the first time setting appropriate ",
         "loadmodels_* parameters as FALSE, and with fit_simmodels and ",
         "write_simmodels as TRUE. ",
         "The model objects are created in the appendices.\n",
         "Probably you will also want to set simulate_obs and ",
         "write_scenariofiles as TRUE for similar reasons (next chapter).\n",
         "Be prepared for a long period (potentially hours) of fitting ",
         "and simulating. After that, the objects are created, and you can ",
         "recompile the report with default parameter settings.")
}



#' Multimodel evaluation plot
#'
#' Prints plots of observed vs. fitted, given a model object, a model diagnosis
#' object (dataframe with columns observed, fitted and resid) and a model name.
#'
plot_modelevaluation <- function(model, diagn, mn) {
    p <-
        diagn %>%
        ggplot(aes(x = observed, y = fitted)) +
        geom_point(size = 0.3) +
        geom_abline(colour = "red") +
        coord_equal() +
        labs(subtitle = paste0("RMSE: ",
                               diagn$resid^2 %>%
                                   mean %>%
                                   sqrt %>%
                                   round(3),
                               "\nNr of equivalent replicates: ",
                               summary(model)$neffp["Number of equivalent replicates", ]
        )) +
        theme(plot.subtitle = element_text(hjust = 1))
    if (interactive()) print(p) else {
        modelname_hyphen <- str_replace_all(mn, "_|\\.", "-")
        knit_expand(text = c(paste("```{r modelfit-{{modelname_hyphen}},",
                                   "fig.cap = 'Globale fit,",
                                   "_root-mean-square error_ (RMSE)",
                                   "en aantal _equivalent replicates_",
                                   "voor model `{{mn}}`.',",
                                   "out.width='70%',",
                                   "warning=FALSE}"),
                             "print(p)",
                             "```")) %>%
            {knit_child(text = .,
                        quiet = TRUE,
                        envir =  environment())} %>%
            cat(sep = '\n')
    }

}


################################################################################

# Functions used to aid reproducible scenario simulation


#' Split spatial population size proportional to the distribution along a spatial factor
#'
#' @param spfact Currently "soilclass" and "ecoregion" are supported
split_popsize <- function(df, spfact, scheme_sel) {
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
        filter(population_size > 0) %>%
        select(-proportion) %>%
        relocate(population_size, .after = last_col()) %>%
        {if (scheme_sel != "GW_03.3") . else {
            # for GW_03.3, stratum "Ecoregio van de krijtgebieden" (Voerstreek)
            # is missing from the data used to fit the model. Hence, we replace
            # this stratum by the spatially adjacent "Ecoregio van de
            # krijt-leemgebieden", in order to get model results for those
            # locations as well:
            mutate(.,
                   ecoregion =
                       fct_recode(ecoregion,
                                  "Ecoregio van de krijt-leemgebieden" =
                                      "Ecoregio van de krijtgebieden")
                       ) %>%
                group_by(across(-population_size)) %>%
                summarise(population_size = sum(population_size)) %>%
                ungroup
        }}
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
#' @param design_matrix defines the size and fixed + random level configuration
#' of one (and each) population
#' @param sp_fact character vector of variable name(s) that represent the spatial variables in the design matrix
#' @param var_time the name of the temporal variable in design_matrix
#' @param ... arguments passed to simulate_detrended_pops_singlemodel()

simulate_detrended_pops <-
    function(design_matrix,
             spfact,
             var_time,
             seed = NULL,
             ...) {

        if (!is.null(seed)) set.seed(seed)

    design_matrix_nested <-
        design_matrix %>%
        rename(orig_type = type) %>%
        nest(design_matrix = -modelname) %>%
        add_model_column("modelname") %>%
        mutate(
            design_matrix = map2(design_matrix, model,
                                 function(dm, model) {

                 # var_stratum_: variable by which temporal or spatial variation
                 # is stratified; it can have less levels than the orginal
                 # variable it is based on, because certain levels don't have
                 # enough timeseries data

                 var_stratum_ <-
                     if (any(str_detect(colnames(model$model.matrix),
                                        "stratum_+\\D?(light|heavy|peat)"))) {
                         "soilclass"
                     } else if (any(str_detect(colnames(model$model.matrix),
                                               "stratum_+\\D?.*(polders|Kempen)"))) {
                         "ecoregion"
                     } else "type"

                 # var_stratum: variable by which residual distribution has been split
                 var_stratum <-
                     if (any(str_detect(rownames(model$summary.hyperpar),
                                        "^Stratum.*(light|heavy|peat)"))) {
                         "soilclass"
                     } else if (any(str_detect(rownames(model$summary.hyperpar),
                                               "^Stratum.*(polders|Kempen)"))) {
                         "ecoregion"
                     } else "type"

                 mdata <- model$.args$data

                 dm %>%
                     rename(type = modelterm_type) %>%
                     `colnames<-`(colnames(.) %>% replace(. == "time", var_time)) %>%
                     mutate(
                         type =
                             type %>%
                             factor(levels =
                                        levels(mdata[str_detect(names(mdata), "^type")][[1]])),
                         stratum_ =
                             .[[var_stratum_]] %>%
                             as.character %>%
                             str_remove_all("[\\P{Letter}]") %>%
                             factor(levels = levels(mdata[str_detect(names(mdata), "^stratum_+\\D?$")][[1]])),
                         stratum =
                             .[[var_stratum]] %>%
                             factor(levels = levels(mdata[str_detect(names(mdata), paste0("^", var_stratum, "(_\\D)?$"))][[1]]))
                     )
                                 }),
            likelihood_family = map(model,
                             ~unique(.$.args$family)),
            link = map(likelihood_family,
                       ~map_chr(.,
                                ~inla.models()$likelihood[[.]]$link[2]))) %>%
        arrange(modelname)

    if (length(design_matrix_nested$likelihood_family[[1]]) == 1L) {
        # single model approach
        simulate_detrended_pops_singlemodel(design_matrix_nested =
                                                  design_matrix_nested,
                                              var_time = var_time,
                                              ...)
    } else {
        # joint model approach
        suffixes_joint <-
            str_sub(design_matrix_nested$likelihood_family[[1]], 1, 1) %>%
            str_c("_", .)
        result_list <-
            map(seq_along(suffixes_joint),
                function(x) {
                    simulate_detrended_pops_singlemodel(
                        design_matrix_nested = design_matrix_nested,
                        var_time = var_time,
                        suffixes_joint = suffixes_joint,
                        index_joint = x,
                        ...
                )})
        result_list[[1]] %>%
            inner_join(result_list[[2]],
                       by = c("population",
                              "location",
                              "type",
                              spfact,
                              var_time,
                              "modelname"
                              )) %>%
            ## below part is to be extended if more joint models are to be
            ## supported
            {if (all(c("response_b", "response_g") %in% colnames(.))) {
                # response of hurdle gamma model:
                mutate(., response = -response_b * response_g)
            } else stop("Currently only the hurdle gamma model is supported. ",
                        "Please extend the main function to calculate the ",
                        "response for other joint models.")}
    }
    }







#' Simulate populations from posterior mean fixed and hyperparameter values
#'
#' This function also omits the long-term trend component.
#' Only intended for single models, i.e. not for a joint model.
#'
#' @param npop number of populations to simulate.
#' They only differ by their used random effect values, which still originate
#' from the same set of parameters (posterior means of fixed and
#' hyperparameters)
#' @param suffixes_joint Only relevant in the context of a joint model.
#' The vector of likelihood_family suffixes that is used in the
#' model effect names to denote the submodel (following the format like '_b'
#'  for bernouilli, '_g' for gamma)
#' @param index_joint numeric index indicating the submodel (1 for first,
#' 2 for second, etc)
#' @param uln_lp Upper quantile limit used in truncating a normal distribution
#' with zero mean that is used inside the linear predictor.
#' The distribution will be truncated symmetrically at both sides, i.e. all
#' values outside [-uln_lp, uln_lp] will be replaced by new values inside the
#' interval.
#' Note that this parameter is used for random effects inside the linear
#' predictor, hence must be regarded on the scale of the linear predictor.
#' Also note this parameter will equally be applied to each submodel, in case
#' of a joint model.
#' A `NULL` or `NA` value will be converted to the default value.
#' @param uln Upper quantile limit used in truncating a normal distribution
#' that is used for the distribution of the response.
#' A `NULL` or `NA` value will be converted to the default value.
#' @param lln Lower quantile limit used in truncating a normal distribution
#' that is used for the distribution of the response.
#' A `NULL` or `NA` value will be converted to the default value.
#' @param ulg Upper quantile limit used in truncating a gamma distribution
#' that is used for the distribution of the response.
#' The distribution will only be right-truncated.
#' A `NULL` or `NA` value will be converted to the default value.
#' @param keep_linpred_parts Should the result keep the separate
#' components of the linear predictor (total fixed effect, random effects and
#' residuals)?
#' Note that these results are given on the scale of the linear predictor.
#' @param keep_resp_parts Should the result keep the final components that
#' allowed the calculation of the response (linear predictor, likelihood
#' family & associated parameters, link function)?
#' @inheritParams simulate_detrended_pops
#'
simulate_detrended_pops_singlemodel <-
    function(design_matrix_nested,
             npop = 20,
             var_time,
             suffixes_joint = "",
             index_joint = 1,
             uln_lp = Inf,
             uln = Inf,
             lln = -Inf,
             ulg = Inf,
             keep_linpred_parts = FALSE,
             keep_resp_parts = FALSE) {
        # colnames(model$model.matrix)
        # rownames(model$summary.fixed)
        # names(model$summary.random)
        # rownames(model$summary.hyperpar)
        # model$names.fixed
        # model$.args$data
        # model$.args$family
        # inla.models()$likelihood[["gamma"]]$link
        # latent_names <- model$misc$configs$contents$tag
        suffix <- suffixes_joint[index_joint]
        append_suffix <- function(x) str_c(x, suffix, "$")
        sd_extract2 <- function(m, r) sd_extract(m,
                                                 r,
                                                 suffixes_joint,
                                                 index_joint)

        if (is.na(uln_lp) || is.null(uln_lp)) uln_lp <- eval(formals()$uln_lp)
        if (is.na(uln) || is.null(uln)) uln <- eval(formals()$uln)
        if (is.na(lln) || is.null(lln)) lln <- eval(formals()$lln)
        if (is.na(ulg) || is.null(ulg)) ulg <- eval(formals()$ulg)

        design_matrix_nested %>%
            mutate(
                formula_fixed =
                    map2(design_matrix, model, function(dm, model) {
                        model$.args$formula %>%
                            as.formula %>%
                            terms %>%
                            attr("term.labels") %>%
                            .[!str_detect(.,
                                          # keeping only fixed effects, and excluding long-term
                                          # trend:
                                          "f\\(|:.*year|I\\(.*year|year_std(_\\D)?$")] %>%
                            # select terms using suffix
                            .[str_detect(., str_c(suffix, "$"))] %>%

                            {if ("stratum_" %in% colnames(dm)) . else .[!str_detect(., "stratum_")]} %>%
                            paste(collapse = " + ") %>%
                            {if ("(Intercept)" %in% model$names.fixed) {
                                paste("~", .) } else paste("~-1 +", .)
                            } %>%
                            as.formula}),
                design_matrix = map2(design_matrix, formula_fixed,
                                     function(dm, ff) {
                                         if (str_detect(paste(ff, collapse = " "),
                                                        "intercept")) {
                                             dm <- mutate(dm, intercept = 1)
                                         }
                                         if (nchar(suffix) > 0) {
                                             cnind <- colnames(dm) != "orig_type"
                                             colnames(dm)[cnind] <-
                                                 str_c(colnames(dm)[cnind], suffix)
                                         }
                                         dm
                                     }
                                    ),
                model_matrix =
                  map2(design_matrix, formula_fixed,
                       function(dm, ff) {
                         model.matrix(ff, dm,
                                      # forcing dummy variables for all factor
                                      # levels (the ones without a model
                                      # parameter are removed in a next step):
                                      contrasts.arg =
                                        map(dm %>%
                                              select(where(is.factor) &
                                                     any_of(attr(terms(ff),
                                                                 "term.labels"))),
                                            contrasts,
                                            contrasts = FALSE))
                       }
                       ),
                # calculate fixed part
                "linpred_fixed{suffix}" := map2(model_matrix, model,
                                        function(mm, model) {
                                            nr_fe <- sum(rownames(model$summary.fixed) %in% colnames(mm))
                                            # allowing a difference of 1, which
                                            # can occur in a second submodel of
                                            # a joint model, where the manually
                                            # added intercept is used to
                                            # represent the first level of a
                                            # fixed effect:
                                            if (ncol(mm) - nr_fe > 1) {
                                                message("The preliminary model matrix has ", ncol(mm) - nr_fe, " columns that don't occur in the model's fixed effects.\nThis will be corrected.")
                                            }
                                            mm <-
                                                mm[, colnames(mm) %in%
                                                       rownames(
                                                           model$summary.fixed
                                                           )
                                                   ]
                                            # needed when factor level order differs:
                                            mm <- mm[, rownames(model$summary.fixed)[rownames(model$summary.fixed) %in% colnames(mm)]]
                                            if(any(rownames(model$summary.fixed)[rownames(model$summary.fixed) %in% colnames(mm)] != colnames(mm))) stop("The order of model matrix columns does not match that of the fixed effects.")
                                            pars_fixed <- model$summary.fixed[
                                                rownames(model$summary.fixed) %in% colnames(mm), "mean"]
                                            as.numeric(mm %*% pars_fixed)

                                        }),
                design_matrix =
                    map(design_matrix,
                        ~rename_with(
                            .,
                            .cols = matches("^type(_\\D)?$"),
                            .fn = ~str_c("modelterm_", .)
                        ) %>%
                            rename(type = orig_type) %>%
                            select(-starts_with("intercept")))
            ) %>%
            select(-formula_fixed, -model_matrix, -model) %>%
            nest(design_modelres = -c(modelname, likelihood_family, link)) %>%
            mutate(design_modelres = map(design_modelres,
                                    ~unnest(., c(design_matrix,
                                                 str_c("linpred_fixed",
                                                       suffix))))) %>%
            add_model_column("modelname") %>%
            # extracting model-level standard deviations:
            mutate(
                model_sd = map(model, function(model) {
                    c(
                        ranef_loc_sd = sd_extract2(model, "loc_code_?[a-z]*"),
                        ranef_clus_sd = sd_extract2(model, "cluster_id[a-z]*"),
                        ranef_time_sd = sd_extract2(model, var_time)
                    )
                }),
                design_modelres =
                    map(design_modelres,
                        ~nest(., design_modelres =
                                  -str_c(c("stratum", "stratum_"),
                                         suffix)))
            ) %>%
            unnest(design_modelres) %>%
            # extracting `stratum_`-level standard deviations and
            # `stratum`-level likelihood parameters:
            mutate(
                model_sd_strat = pmap(list(model,
                                           .data[[str_c("stratum", suffix)]],
                                           .data[[str_c("stratum_", suffix)]]),
                function(model, stratum, stratum_) {
                    c(
                        ranef_clus_stratum_sd =
                            sd_extract2(model, str_c("cluster_stratum_",
                                                     stratum_)),
                        ranef_time_stratum_sd =
                            sd_extract2(model, str_c(var_time,
                                                     "_stratum_",
                                                     stratum_)),
                        llhfam_param2_gaussian =
                            sd_extract(model,
                                       str_c("^Stratum ",
                                             stratum,
                                             ": Precision of residuals",
                                             "( \\(in log scale\\))?"),
                                       "", 1, "")^2,
                        llhfam_param2_gamma =
                            model$summary.hyperpar[str_c("Stratum ",
                                                         stratum,
                                                         ": Gamma shape"),
                                                   "mean"]
                    )
                })
            ) %>%
            select(-model) %>%
            crossing(population = str_c("population_", str_pad(1:npop, 5, pad = "0")) %>% as.factor) %>%
            add_model_column("modelname") %>%
# different populations only need to be accommodated from this point on (they share their fixed prediction)
            # filter(population %in% c("population_00001", "population_00002")) %>%  # DEBUGGING ONLY
            # slice(1:2) %>% # DEBUGGING ONLY
            mutate(
                design_modelres =
                    pmap(list(design_modelres,
                              model,
                              model_sd,
                              model_sd_strat,
                              likelihood_family,
                              link),
                         function(design_modelres,
                                  model,
                                  model_sd,
                                  model_sd_strat,
                                  likelihood_family,
                                  lnk) {
                     design_modelres %>%
                         # spatial noise
                         group_by(across(str_c("location", suffix))) %>%

                         {if (!is.na(model_sd["ranef_loc_sd"])) {
                             mutate(., "ranef_loc{suffix}" :=
                                        rtrunc(1, spec = "norm",
                                               sd = model_sd["ranef_loc_sd"],
                                               a = -uln_lp, b = uln_lp)
                                    )} else .} %>%

                         {if (!is.na(model_sd["ranef_clus_sd"])) {
                             mutate(., "ranef_clus{suffix}" :=
                                        rtrunc(1, spec = "norm",
                                               sd = model_sd["ranef_clus_sd"],
                                               a = -uln_lp, b = uln_lp)
                             )} else .} %>%

                         {if (!is.na(model_sd_strat["ranef_clus_stratum_sd"])) {
                             mutate(., "spatial_noise{suffix}" :=
                                        rtrunc(1, spec = "norm",
                                               sd = model_sd_strat["ranef_clus_stratum_sd"],
                                               a = -uln_lp, b = uln_lp)
                             )} else .} %>%

                         group_by(across(c(str_c(var_time, suffix)))) %>%

                         # temporal noise
                         {if (!is.na(model_sd["ranef_time_sd"])) {
                                 mutate(., "ranef_time{suffix}" :=
                                            rtrunc(1, spec = "norm",
                                                   sd = model_sd["ranef_time_sd"],
                                                   a = -uln_lp, b = uln_lp)
                                 )} else .} %>%

                         {if (!is.na(model_sd_strat["ranef_time_stratum_sd"])) {
                                 mutate(., "temporal_noise{suffix}" :=
                                            rtrunc(1, spec = "norm",
                                                   sd =  model_sd_strat["ranef_time_stratum_sd"],
                                                   a = -uln_lp, b = uln_lp)
                                 )} else .} %>%

                         ungroup %>%
                         # calculate linpred and first parameter of likelihood family (usually: the expected response)
                         mutate("linpred{suffix}" :=
                                    rowSums(across(c(
                                        str_c("linpred_fixed", suffix),
                                        starts_with("ranef"),
                                        ends_with(str_c("noise", suffix))))),
                                "link{suffix}" :=
                                    factor(lnk[index_joint]),
                                "llhfam{suffix}" :=
                                    factor(likelihood_family[index_joint]),
                                "llhfam_param1{suffix}" :=
                                    .data[[str_c("linpred",suffix)]]  %>%
                                    {switch(
                                        lnk[index_joint],
                                        "identity" = .,
                                        "log" = exp(.),
                                        "logit" = exp(.) / (1 + exp(.))
                                           )}
                         ) %>%
                         # second parameter of likelihood family
                         mutate("llhfam_param2{suffix}" :=
                                    switch(
                            likelihood_family[index_joint],
                            "gaussian" =
                                model_sd_strat["llhfam_param2_gaussian"],
                            "lognormal" =
                                model_sd_strat["llhfam_param2_gaussian"],
                            "binomial" = NA,
                            "gamma" =
                                model_sd_strat["llhfam_param2_gamma"]
                                           )
                         ) %>%
                         mutate("response{suffix}" :=
                                    switch(
                                        likelihood_family[index_joint],
                                        "gaussian" =
                                            rtrunc(n(), spec = "norm",
                                                   mean = .data[[str_c("llhfam_param1", suffix)]],
                                                   sd = sqrt(.data[[str_c("llhfam_param2", suffix)]]),
                                                   a = lln, b = uln),
                                        "lognormal" =
                                            rtrunc(n(), spec = "norm",
                                                   mean = .data[[str_c("llhfam_param1", suffix)]],
                                                   sd = sqrt(.data[[str_c("llhfam_param2", suffix)]]),
                                                   a = lln, b = uln) %>%
                                            exp,
                                        "binomial" =
                                            rbinom(n(),
                                                   size = 1,
                                                   .data[[str_c("llhfam_param1", suffix)]]),
                                        "gamma" =
                                            rtrunc(n(), spec = "gamma",
                                                   shape = .data[[str_c("llhfam_param2", suffix)]],
                                                   scale = .data[[str_c("llhfam_param1", suffix)]] / .data[[str_c("llhfam_param2", suffix)]],
                                                   a = 0, b = ulg)
                                        )) %>%
                         select(-str_c("modelterm_type", suffix)) %>%
                         rename_with(
                             .cols = matches(str_c(suffix, "$")) &
                                 !(str_c("linpred_fixed", suffix):last_col()),
                             .fn = ~str_remove(., str_c(suffix, "$"))
                             )

            }
            )) %>%
            select(-c(model, model_sd, model_sd_strat, likelihood_family, link),
                   -str_c(c("stratum_",
                            "stratum"),
                          suffix)) %>%
            unnest(design_modelres) %>%
            relocate(population) %>%
            relocate(modelname, .after = type) %>%
            {if (keep_linpred_parts) . else {
                select(.,
                       -starts_with("ranef"),
                       -ends_with(str_c("noise", suffix)),
                       -str_c("linpred_fixed", suffix))
            }} %>%
            {if (keep_resp_parts) . else {
                select(.,
                       -str_c("linpred", suffix),
                       -starts_with("llhfam"),
                       -starts_with("link"))
            }} %>%
            arrange(population, location, .data[[var_time]])

    }





#' Add model list column in tibble based on modelname column
#'
#' @param x tibble
#' @param modelname_column variable name that refers the column with modelnames
#'
add_model_column <- function(x, modelname_column) {
    x %>%
        rowwise %>%
        mutate(
            model = list(.data[[modelname_column]] %>% as.character %>% str2lang %>% eval)) %>%
        ungroup
}









#' Extract sd parameter from INLA-model based on regex
#'
#' Also supports submodels of joint model, where for the second (and further)
#' submodel a Beta is provided to rescale the corresponding random effect of the
#' first submodel.
#'
#' @param regex_no_suffix string that will be used within a regex. This should
#'   correspond to the identifying random effect name
#' @inheritParams simulate_detrended_pops_singlemodel
#'
sd_extract <- function(model,
                       regex_no_suffix,
                       suffixes_joint,
                       index_joint,
                       prefix = "Precision for.*") {

    if (is.na(regex_no_suffix) ||
        !any(str_detect(
            names(model$marginals.hyperpar),
            str_c(regex_no_suffix, suffixes_joint[index_joint], "$"))
        )) return(NA)

    selection_1 <-
        names(model$marginals.hyperpar) %>%
        str_detect(str_c(prefix,
                         regex_no_suffix,
                         suffixes_joint[1],
                         "$"))
    if (sum(selection_1) != 1) {
        stop("Regex '",
             regex_no_suffix,
             "' is insufficiently unique. ",
             "Following 'Precision' matches occur: \n",
             paste(names(model$marginals.hyperpar)[selection_1],
                   collapse = ", "))
        }

    if (index_joint == 1) {
        sink(tempfile())
        sd_estim <-
            inla.tmarginal(invsqrt,
                           model$marginals.hyperpar[selection_1][[1]]) %>%
            inla.zmarginal %>%
            .[["mean"]]
        sink()
    } else {
        selection <-
            rownames(model$summary.hyperpar) %>%
            str_detect(str_c("Beta for.*",
                             regex_no_suffix,
                             suffixes_joint[index_joint],
                             "$"))
        if (sum(selection) != 1) {
            stop("Regex '",
                 regex_no_suffix,
                 "' is insufficiently unique. ",
                 "Following 'Beta' matches occur: \n",
                 paste(rownames(model$summary.hyperpar)[selection],
                       collapse = ", "))
        }
        sink(tempfile())
        sd_estim <-
            inla.tmarginal(invsqrt,
                           model$marginals.hyperpar[selection_1][[1]]) %>%
            inla.zmarginal %>%
            {.[["mean"]] * abs(model$summary.hyperpar[selection, "mean"])}
        sink()
    }

    return(sd_estim)

}






#' Generate quantiles of any truncated distribution
#'
#' The code is taken from:
#' Nadarajah S. & Kotz S. (2006). R Programs for Truncated Distributions.
#' Journal of Statistical Software 16: 1–8.
#' https://doi.org/10.18637/jss.v016.c02.
#'
#' @param p Vector of probabilities
#' @param spec String that defines the distribution, substitutable in p***() and
#' q***() functions
#' @param a Lower quantile limit to define the truncation
#' @param b Upper quantile limit to define the truncation
#' @param ... Further arguments passed to p***() and q***() functions
#'
qtrunc <- function(p, spec, a = -Inf, b = Inf, ...)
{
    tt <- p
    G <- get(paste("p", spec, sep = ""), mode = "function")
    Gin <- get(paste("q", spec, sep = ""), mode = "function")
    tt <- Gin(G(a, ...) + p*(G(b, ...) - G(a, ...)), ...)
    return(tt)
}

#' Generate quantiles of any truncated distribution
#'
#' This is a modified take on qtrunc(), where the truncation is based on
#' probabilities instead of quantiles.
#'
#' The code is derived from:
#' Nadarajah S. & Kotz S. (2006). R Programs for Truncated Distributions.
#' Journal of Statistical Software 16: 1–8.
#' https://doi.org/10.18637/jss.v016.c02.
#'
#' @param p_a Lower probability limit to define the truncation
#' @param p_b Upper probability limit to define the truncation
#' @inheritParams qtrunc
#'
qtrunc2 <- function(p, spec, p_a = 0, p_b = 1, ...)
{
    G <- get(paste0("p", spec), mode = "function")
    Gin <- get(paste0("q", spec), mode = "function")
    tt <- Gin(p_a + p*(p_b - p_a), ...)
    return(tt)
}

#' Random generation for any truncated distribution
#'
#' The code is taken from:
#' Nadarajah S. & Kotz S. (2006). R Programs for Truncated Distributions.
#' Journal of Statistical Software 16: 1–8.
#' https://doi.org/10.18637/jss.v016.c02.
#'
#' @param n Requested number of random deviates
#' @inheritParams qtrunc
#'
rtrunc <- function(n, spec, a = -Inf, b = Inf, ...)
{
    x <- u <- runif(n, min = 0, max = 1)
    x <- qtrunc(u, spec, a = a, b = b, ...)
    return(x)
}


#' Random generation for any truncated distribution
#'
#' This is a modified take on rtrunc(), where the truncation is based on
#' probabilities instead of quantiles.
#'
#' The code is taken from:
#' Nadarajah S. & Kotz S. (2006). R Programs for Truncated Distributions.
#' Journal of Statistical Software 16: 1–8.
#' https://doi.org/10.18637/jss.v016.c02.
#'
#' @param n Requested number of random deviates
#' @inheritParams qtrunc
#'
rtrunc2 <- function(n, spec, p_a = 0, p_b = 1, ...)
{
    u <- runif(n, min = 0, max = 1)
    x <- qtrunc2(u, spec, p_a = p_a, p_b = p_b, ...)
    return(x)
}









#' Simulate spatial samples from given populations and sample definitions (including artificial trend)
#'
#' @param sample_definition Data frame that defines the constitution of a sample for each scenario.
#' Minimal columns needed: scenario, trend_12yearly_multiplier, type, n_finitepop_spatial
#' @param population_data Data frame with data of multiple (full) population realizations, with specific required columns: population, type, location, {{var_time}} and response.
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

    trended_pop_data <-
        sample_definition %>%
        select(-popsize_spatial) %>%
        nest(scen_attrib = -scenario) %>%
        crossing(population_data %>%
                     {if (!pops_missing) {
                         filter(., population %in% pops)
                     } else {
                         filter(., str_sub(population, start = -5L) %>%
                                    as.numeric <= npops)
                     }} %>%
                     select(population,
                            location,
                            type,
                            .data[[var_time]],
                            response) %>%
                     group_by(location) %>%
                     mutate(prediction_spatial = median(response)) %>%
                     ungroup %>%
                     nest(pop_data = -population)) %>%
        # adding trend to predicted value per location (this intermediate result is
        # below called spatial_term):
        mutate(pop_data = map2(scen_attrib, pop_data, function(s, p) {
            p %>%
                inner_join(s, by = "type") %>%
                mutate(spatial_term = prediction_spatial *
                           (trend_12yearly_multiplier^(.data[[var_time]]/12)),
                       response =
                           response -
                           prediction_spatial +
                           spatial_term) %>%
                select(-c(prediction_spatial,
                          spatial_term,
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
                                             select(sample) %>%
                                             unnest(sample)
                                          ),
                                          data = map2(spatial_sample, units, ~
                                                          p %>%
                                                          select(-n_finitepop_spatial) %>%
                                                          semi_join(.y,
                                                                    by = "location")
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
#' @param conflevel_pctiles The confidence level (between 0 and 1) that defines lower and upper percentiles (with probabilities arranged symmetrically around P = 0.5).
#' @param qual_std Optional vector of one or more values of a targeted quality standard, i.e. one or more target values of the statistic, for which left-sided probabilities will be estimated from the interpolated ECDF.
#' @param density_left Left side cutoff in density calculation for the statistic, when calculating probabilities.
#' The default is optimized for (absolute or relative) error margins, which are always positive.
#' @param plot Logical. Optionally returns a plot on condition that merge_pops = FALSE.
#' @param ylim NULL or a numeric vector of length 2. If a vector (has a default), is applied as y-limits in the plot.
#' @param facet_scales String. Always applied but only relevant if ylim = NULL.
#' @param ... Arguments passed to `facet_wrap()`.
#'
summarise_status_of_samples <- function(multisample_stats,
                                        statistic,
                                        conflevel_pctiles = 0.9,
                                        qual_std = NULL,
                                        density_left = 0,
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
                          pctile_l = quantile(.data[[statistic]], (1 - conflevel_pctiles) / 2, na.rm = TRUE),
                          pctile_u = quantile(.data[[statistic]], 1 - (1 - conflevel_pctiles) / 2, na.rm = TRUE),
                          if (!is.null(qual_std)) {
                              across(.data[[statistic]],
                                 map(!!qual_std,
                                     function(qs) {
                                         function(x) {
                                             lower_tailprob(
                                                 x, qs, !!density_left
                                             )
                                         }
                                         }) %>%
                                     set_names(str_c("p(stat≤", !!qual_std, ")")),
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





#' Calculate a left tail probability for a given quantile from a vector of observations
#'
#' First calculates a density (PDF) from the observation vector. Then a
#' cumulative distribution function (CDF) is derived from that. Finally, the
#' CDF value is returned for the given quantile.
#'
#' @note
#' Inspired by https://stackoverflow.com/a/6976450 and https://stackoverflow.com/a/43570620.
#'
#' @param x Numeric vector of observations
#' @param q A quantile (i.e. in the scale of the observations) for which the
#' corresponding probability will be calculated
#' @inheritParams summarise_status_of_samples
#'
lower_tailprob <- function(x, q, density_left) {
    stopifnot(length(q) == 1L, is.numeric(q))
    pdf <-
        density(x,
                bw = "SJ",
                from = density_left,
                cut = 20)
    cdf <- cumsum(pdf$y * diff(pdf$x[1:2]))
    cdf <- cdf / max(cdf)
    approxfun(pdf$x, cdf, rule = 1:2)(q)
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
#' Only used (and needed) if `plot = TRUE`.
#' @param qual_std Optional numeric vector of targeted power values (quality standards)
#' for which right-sided probabilities will be estimated from the ECDF.
#' The vector elements _must_ be named after the power_conf column names that appear
#' with `qual_std=NULL`.
#' @param merge_pops Results can be given as power per population or
#' aggregated (the default).
#' Ignored if plot is TRUE.
#' @param plot Logical. Optionally returns a plot.
#' @param ... Arguments passed to `facet_wrap()`.
#'
calculate_power_of_scenarios <- function(multisample_stats,
                                         scenario_def = NULL,
                                         qual_std = NULL,
                                         merge_pops = TRUE,
                                         plot = FALSE,
                                         ...) {
    stopifnot(is.numeric(qual_std) || is.null(qual_std))

    result <-
        multisample_stats %>%
        mutate(across(matches("errmarg\\d{2}$"),
                      ~mean - . > 0 | mean + . < 0,
                      .names = "trend_sign_{.col}")) %>%
        rename_with(.cols = matches("^trend_sign_"),
                    .fn = ~str_remove(., "twosided_errmarg")) %>%
        group_by(across(c(contains("type"), scenario, population))) %>%
        summarise(across(matches("^trend_sign_"), ~sum(.)/n())) %>%
        rename_with(~str_replace(., "trend_sign_", "power_conf"))

    if(!plot) {
        return(
            if (!merge_pops) ungroup(result) else {
                result %>%
                    pivot_longer(cols = matches("^power_conf"),
                                 names_to = "conflevel",
                                 values_to = "power") %>%
                    {if (is.null(qual_std)) . else {
                        inner_join(., tibble(conflevel = names(qual_std),
                                             threshold = qual_std),
                                   by = "conflevel")
                        }} %>%
                    group_by(across(c(conflevel,
                                      matches("^threshold$"))), .add = TRUE) %>%
                    summarise(quartiles = str_c(median(power) %>% round(2),
                                                " (",
                                                quantile(power, 0.25) %>% round(2),
                                                " | ",
                                                quantile(power, 0.75) %>% round(2),
                                                ")"),
                              prob_threshold_exceedance = if (!is.null(qual_std)) {
                                  1 - ecdf(power)(first(threshold))}
                                  ) %>%
                    ungroup %>%
                    (function(df) {
                        if (is.null(qual_std)) {
                            pivot_wider(df,
                                        names_from = conflevel,
                                        values_from = quartiles)
                        } else {
                        spec1 <-
                            build_wider_spec(df,
                                         names_from = c(conflevel, threshold),
                                         values_from = c(quartiles,
                                                         prob_threshold_exceedance)) %>%
                            mutate(.name = ifelse(str_detect(.name, "^quartiles"),
                                                  conflevel,
                                                  str_c("p(", conflevel, "≥",
                                                        threshold, ")")
                                                  ))
                        pivot_wider_spec(df, spec = spec1)}})
            })
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
            rename_with(~str_remove(., "power_")) %>%
            pivot_longer(cols = matches("^conf"),
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










#' Read specific vc-data where special characters were converted to underscore
#'
#' This is tailored to have as input results of the scenario-evaluation
read_vc_special <- function(...) {
    read_vc(...) %>%
        as_tibble %>%
        rename(`n/N` = n_N) %>%
        rename_with(~str_replace_all(., "^p_", "p(")) %>%
        rename_with(~str_replace_all(., "_$", ")")) %>%
        rename_with(~str_replace_all(., "(conf\\d+)_", "\\1≥")) %>%
        rename_with(~str_replace_all(., "(stat)_", "\\1≤"))
}



#' Make summary plot of scenario evaluation
#'
#' @param input A dataframe as returned by read_vc_special
#' @param stat Optional string for filtering the input dataframe.
#' Required in case `input` has a column named `statistic`, which defines the
#' statistic looked at.
plot_summary <- function(input, stat = NULL) {
    if (!is.null (stat)) {
        input <-
            input %>%
            filter(statistic == stat) %>%
            rename_with(~str_replace_all(., "(?<=p\\()stat", stat)) %>%
            select(-statistic)
    }
    input %>%
        pivot_longer(cols = starts_with("p("),
                     names_to = "distribution_threshold",
                     values_to = "probability") %>%
        drop_na %>%
        {if (any(str_detect(unique(.$distribution_threshold), "errmarg"))) {
            mutate(., distribution_threshold =
                       factor(distribution_threshold) %>%
                       fct_rev)
        } else .} %>%
        ggplot(aes(x = total_n_finitepop_spatial,
                   y = probability,
                   shape = distribution_threshold,
                   linetype = distribution_threshold,
                   colour = typegroup)) +
        geom_line() +
        geom_point() +
        facet_wrap(~trend_12yearly_multiplier, labeller = "label_both")
}

