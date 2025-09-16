library(RDS)
library(RDStreeboot)
library(surveybootstrap)
library(Neighboot)


## FOR CHAIN RESAMPLING:

prepare_chain_bootstrap_inputs <- function(rds_data,
                                           id_col = "id",
                                           recruiter_col = "recruiter.id",
                                           degree_col = "known_network_size",
                                           traits = "trait_A",
                                           keep.vars = NULL) {
  # Step 1: Rename to expected structure for surveybootstrap
  rds_data <- rds_data %>%
    rename(uid = !!sym(id_col),
           recruiter_id = !!sym(recruiter_col),
           degree = !!sym(degree_col))
  
  # Step 2: Build recruitment chains
  seed_ids <- rds_data$uid[rds_data$recruiter_id == 0 | is.na(rds_data$recruiter_id)]
  
  chains <- lapply(seed_ids, function(seed_id) {
    surveybootstrap::make.chain(
      seed.id = seed_id,
      survey.data = rds_data,
      is.child.fn = function(id, parent_id) {
        rds_data$recruiter_id[rds_data$uid == id] == parent_id
      }
    )
  })
  
  # Step 3: Estimate mixing model
  mm <- surveybootstrap::estimate.mixing(
    survey.data = rds_data,
    parent.data = rds_data,
    traits = traits
  )
  
  # Step 4: Estimate degree distributions
  dd <- surveybootstrap::estimate.degree.distns(
    survey.data = rds_data,
    d.hat.vals = "degree",
    traits = traits,
    keep.vars = keep.vars
  )
  
  return(list(
    chains = chains,
    mm = mm,
    degree_dist = dd,
    processed_data = rds_data
  ))
}


# Suppose dd is your main data frame with known_network_size and exploitation traits
chain_inputs <- prepare_chain_bootstrap_inputs(
  rds_data = dd,
  traits = c("document_withholding_rds", "pay_issues_rds"),
  keep.vars = c("document_withholding_nsum", "pay_issues_nsum")
)

rds_sample$chains <- chain_inputs$chains
rds_sample$mm <- chain_inputs$mm
rds_sample$degree_dist <- chain_inputs$degree_dist




##############################################################################################
##############################################################################################
##############################################################################################



bootstrap_rds_sample <- function(rds_sample,
                                 method = c("tree", "neighboot", "chain", "simple", "ss"),
                                 B = 500,
                                 traits = NULL,
                                 degree_col = "q13",
                                 keep.vars = NULL,
                                 return_rds_df = TRUE,
                                 save_path = NULL,
                                 neighb_args = list(),
                                 verbose = TRUE) {
  
  method <- match.arg(method)
  
  # Load required packages
  if (method == "tree") requireNamespace("RDStreeboot")
  if (method == "neighboot") requireNamespace("Neighboot")
  if (method == "chain") requireNamespace("surveybootstrap")
  
  # If input is rds.data.frame, extract base data
  if ("rds.data.frame" %in% class(rds_sample)) {
    dd <- as.data.frame(rds_sample)
  } else if (is.data.frame(rds_sample)) {
    dd <- rds_sample
  } else {
    stop("Input must be data.frame or rds.data.frame")
  }
  
  # Setup common arguments
  id_col <- "id"
  recruiter_col <- "recruiter.id"
  
  # --- Method-specific bootstrap execution ---
  boot_list <- switch(method,
                      
                      # ------------------------------------------
                      "tree" = {
                        if (verbose) cat("Running Tree Bootstrap via RDStreeboot...\n")
                        net <- list(
                          nodes = dd[[id_col]],
                          edges = list(
                            node1 = dd[[recruiter_col]],
                            node2 = dd[[id_col]]
                          ),
                          traits = if (!is.null(traits)) dd[, traits, drop = FALSE] else NULL,
                          degree = dd[[degree_col]]
                        )
                        
                        resamples <- RDStreeboot::tree.boot(net, B = B)
                        if (return_rds_df) {
                          purrr::map(resamples, ~ RDS::as.rds.data.frame(.x, id = id_col, recruiter.id = recruiter_col))
                        } else resamples
                      },
                      
                      # ------------------------------------------
                      "neighboot" = {
                        if (verbose) cat("Running Neighboot Bootstrap via Neighboot::neighb...\n")
                        net <- list(
                          nodes = dd[[id_col]],
                          edges = list(
                            node1 = dd[[recruiter_col]],
                            node2 = dd[[id_col]]
                          ),
                          traits = if (!is.null(traits)) dd[, traits, drop = FALSE] else NULL,
                          degree = dd[[degree_col]]
                        )
                        
                        boot <- do.call(Neighboot::neighb, c(list(dat = net, reps = B, output = "list"), neighb_args))
                        if (return_rds_df) {
                          purrr::map(boot, ~ RDS::as.rds.data.frame(.x, id = id_col, recruiter.id = recruiter_col))
                        } else boot
                      },
                      
                      # ------------------------------------------
                      "chain" = {
                        if (verbose) cat("Running Chain Bootstrap via surveybootstrap...\n")
                        
                        # Build inputs
                        chain_inputs <- prepare_chain_bootstrap_inputs(
                          rds_data = dd,
                          id_col = id_col,
                          recruiter_col = recruiter_col,
                          degree_col = degree_col,
                          traits = traits,
                          keep.vars = keep.vars
                        )
                        
                        boot <- surveybootstrap::rds.boot.draw.chain(
                          chains = chain_inputs$chains,
                          mm = chain_inputs$mm,
                          dd = chain_inputs$degree_dist,
                          num.reps = B,
                          keep.vars = keep.vars
                        )
                        
                        if (return_rds_df) {
                          purrr::map(boot, ~ RDS::as.rds.data.frame(.x, id = id_col, recruiter.id = recruiter_col))
                        } else boot
                      },
                      
                      # ------------------------------------------
                      "simple" = {
                        if (verbose) cat("Running Simple IID Bootstrap...\n")
                        replicate(B, dd[sample(nrow(dd), replace = TRUE), ], simplify = FALSE)
                      },
                      
                      # ------------------------------------------
                      "ss" = {
                        stop("SS-bootstrap not yet implemented.")
                      }
  )
  
  if (!is.null(save_path)) {
    if (verbose) cat("Saving bootstrap samples to:", save_path, "\n")
    saveRDS(boot_list, file = save_path)
  }
  
  return(boot_list)
}





#### TESTING

B <- 500

boot_samples <- bootstrap_rds_sample(
  rds_sample = rd.dd,
  method = "tree",
  B = B,
  traits = c("q8_a", "q11", "q5"),
  keep.vars = c("document_withholding_nsum", "pay_issues_nsum"),
  save_path = here::here("output", paste0("bootstrap_chain_",B,".rds") )
)


