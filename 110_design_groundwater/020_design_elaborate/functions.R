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

simulate_modelterms <-
function(model,
         design_matrix,
         simulate_response = FALSE,
         nr_nodes = 4,
         hyperparname_loc = "Precision for loc_code",
         hyperparname_cluster = "Precision for cluster_id",
         hyperparname_tempnoise = "Precision for hydroyear_std_short",
         seed_main = 123456,
         seed_node_base = 1e9,
         seed_inla_base = 5e8) { # has at least columns location, type, modelterm_type, time, temporal_increase

    latent_extra <-
        colnames(design_matrix)[!str_detect(colnames(design_matrix),
                                            "location|type|modelterm_type|time|temporal_increase")] %>%
        {if (length(.) > 0) paste(collapse = "|") else .} %>%
        {model$names.fixed[str_detect(model$names.fixed, .)]}
    modelterms_type <-
        design_matrix %>%
        distinct(type, modelterm_type) %>%
        rename(modelterm = modelterm_type)
    nr_nodes <- min(nr_nodes, nrow(modelterms_type))
    nlocs_per_type <-
        design_matrix %>%
        distinct(location, type) %>%
        count(type) %>%
        pull(n)
    ntimesteps <-
        design_matrix %>%
        distinct(time) %>%
        nrow
    set.seed(seed_main)
    clus <- parallel::makeCluster(nr_nodes)
    parallel::clusterApply(clus,
                           as.integer(runif(nr_nodes) * seed_node_base),
                           set.seed)
    # simulate selected modelparameters, i.e. at location level (1 location has 1 type)
    modelpars_sim <-
        parallel::clusterMap(
            clus,
            function(type, modelterm, nlocs, seed, model, latent_extra) {
                tibble::tibble(
                    type = type,
                    modelpar_simulation=
                        purrr::map(1:nlocs,
               # the below code is preferred over 'n=1000' because otherwise
               # the sampled hyperparameter values remain constant among posterior samples
               # and also with seed it repeats random numbers that it already used.
                                   ~INLA::inla.posterior.sample(
                                       n = 1,
                                       result = model,
                                       selection =
                                           magrittr::set_names(list(0),
                                                               c(modelterm, latent_extra)),
                                       seed = seed
                                   )[[1]]))
            },
            type = modelterms_type$type,
            modelterm = modelterms_type$modelterm,
            nlocs = nlocs_per_type,
            seed = as.integer(runif(nlocs_per_type) * seed_inla_base),
            MoreArgs = list(model = M1, latent_extra = latent_extra))
    parallel::stopCluster(clus)
    modelpars_sim <-
        bind_rows(modelpars_sim) %>%
        nest(modelpar_simulation = modelpar_simulation)
    # Simulate all modelterms (per location x timestep),
    # excluding long-term trend and with constraints on temporal noise.
    # This provides a baseline scenario (no long-term trend).
    set.seed(seed_main)
    modelterms_sim <-
        design_matrix %>%
        nest(temporal_design = c(time, temporal_increase)) %>%
        nest(location = location) %>%
        inner_join(modelpars_sim, by = "type") %>%
        unnest(c(location, modelpar_simulation)) %>%
        relocate(location) %>%
        mutate(fixef_type = map_dbl(modelpar_simulation, ~.$latent[1, 1]),
               ranef_loc = map_dbl(modelpar_simulation,
                                   ~rnorm(1, sd = invsqrt(.$hyperpar["Precision for loc_code"]))),
               ranef_clus = map_dbl(modelpar_simulation,
                                    ~rnorm(1, sd = invsqrt(.$hyperpar["Precision for cluster_id"]))),
               temporal_noise = # sadly takes much computing time
                   map(modelpar_simulation, function(simobs) {
                       tau_temporal <- simobs$hyperpar[modelcomponent_tempnoise]
                       continue <- TRUE
                       while(continue) {
                           sim_temporal <- simulate_rw(tau = tau_temporal,
                                                       length = ntimesteps,
                                                       order = 1,
                                                       n_sim = 1)
                           continue <-
                               sim_temporal %>%
                               mutate(change = y - lag(y)) %>%
                               summarise(max_change = max(abs(change), na.rm = TRUE),
                                         max_net_change = max(abs(y)),
                                         pval_trend =
                                             lm(y ~ x) %>%
                                             summary %>%
                                             {.$coefficients["x", "Pr(>|t|)"]}) %>%
                               {.$max_change < 1.5 * invsqrt(tau_temporal) &&
                                       .$max_net_change < 1.5 * invsqrt(tau_temporal) &&
                                       .$pval_trend > 0.2} %>%
                               !.
                       }
                       sim_temporal %>%
                           as_tibble %>%
                           mutate(y = y - mean(y)) %>%
                           select(ranef_time = y)
                   }),
               resid_noise =
                   map2(modelpar_simulation, modelterm_type, function(simobs, type) {
                       simobs$hyperpar[names(simobs$hyperpar) %>%
                                           str_detect(str_c("^Stratum.*",
                                                            str_remove(type, "type")))] %>%
                           invsqrt %>%
                           {tibble(resid = rnorm(ntimesteps, sd = .))}
                   })
        )
    # optionally: unnest, calculate response values - including temporal increase -
    # and drop simulated modelterms
    if (simulate_response) {
        response_sim <-
            modelterms_sim %>%
            select(-modelterm_type, -modelpar_simulation) %>%
            unnest(cols = c(temporal_design, temporal_noise, resid_noise)) %>%
            rowwise %>%
            mutate(response = sum(fixef_type,
                                  ranef_loc,
                                  ranef_clus,
                                  temporal_increase,
                                  ranef_time,
                                  resid),
                   .keep = "unused") %>%
            ungroup %>%
            arrange(location, type, time)
        return(list(modelterms_sim = modelterms_sim,
                    response_sim = response_sim))
    } else
        return(modelterms_sim)
}
