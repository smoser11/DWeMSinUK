# Global configuration for DWeMSinUK analyses

# Returns a named list of shared configuration values used by
# the main pipeline, RDS, and NSUM components.
get_global_config <- function() {
  list(
    # Methods
    preferred_rds_method = "RDS_SS",
    preferred_nsum_method = "weighted",

    # Population scenarios
    population_sizes = c(50000, 100000, 980000, 1740000),
    main_population_size = 980000,

    # Execution
    include_bootstrap = TRUE,
    n_bootstrap = 1000,
    parallel_cores = 4,

    # Output controls
    save_tables = TRUE,
    save_plots = TRUE
  )
}

