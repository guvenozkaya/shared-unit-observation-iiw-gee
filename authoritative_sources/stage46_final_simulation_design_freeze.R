# Stage 46: Final Simulation Design Freeze
# No new simulation is fitted in this stage.

stage46_find_file <- function(filename) {
  roots <- unique(c(getwd(), dirname(normalizePath(sys.frame(1)$ofile %||% getwd(), mustWork = FALSE))))
  roots <- roots[dir.exists(roots)]
  for (root in roots) {
    direct <- file.path(root, filename)
    if (file.exists(direct)) return(normalizePath(direct))
    hit <- list.files(root, pattern = paste0("^", gsub("\\.", "\\\\.", filename), "$"),
                      recursive = TRUE, full.names = TRUE)
    if (length(hit)) return(normalizePath(hit[[1]]))
  }
  NULL
}

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || !nzchar(x)) y else x

stage46_expected_manifest <- function() {
  data.frame(
    scenario_id = c("VD_B_N","VD_B_A","VD_SH_N","VD_SH_A","VS_B_N","VS_B_A","VS_SH_N","VS_SH_A"),
    mechanism = c("VD","VD","VD","VD","VS","VS","VS","VS"),
    structure = c("B","B","SH","SH","B","B","SH","SH"),
    true_beta3 = c(0,.2,0,.2,0,.2,0,.2),
    gamma_control = c(.3,.3,.3,.3,.6,.6,.6,.6),
    gamma_treatment = c(.75,.75,.75,.75,.6,.6,.6,.6),
    total_rate_reported = c(10.974,12.071,10.890,12.197,10.620,11.894,10.692,11.761),
    parameter_source = c(
      "Stage40_null_calibration","Stage42_alternative_calibration",
      "Stage40_null_calibration","Stage42_alternative_calibration",
      "Stage40_null_calibration","Stage42_alternative_calibration",
      "Stage40_null_calibration","Stage42_alternative_calibration"
    ),
    stringsAsFactors = FALSE
  )
}

stage46_validate_manifest <- function(path) {
  x <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  e <- stage46_expected_manifest()
  req <- c("scenario_id","mechanism","structure","true_beta3","gamma_control",
           "gamma_treatment","total_rate","parameter_source")
  stopifnot(all(req %in% names(x)), nrow(x) == 8, !anyDuplicated(x$scenario_id))
  x <- x[match(e$scenario_id, x$scenario_id), , drop = FALSE]

  audit <- data.frame(
    field = c("scenario_id","mechanism","structure","parameter_source",
              "true_beta3","gamma_control","gamma_treatment","total_rate"),
    pass = c(
      identical(as.character(x$scenario_id), e$scenario_id),
      identical(as.character(x$mechanism), e$mechanism),
      identical(as.character(x$structure), e$structure),
      identical(as.character(x$parameter_source), e$parameter_source),
      max(abs(x$true_beta3-e$true_beta3)) <= 1e-10,
      max(abs(x$gamma_control-e$gamma_control)) <= 1e-10,
      max(abs(x$gamma_treatment-e$gamma_treatment)) <= 1e-10,
      all(round(x$total_rate,3) == round(e$total_rate_reported,3))
    ),
    stringsAsFactors = FALSE
  )
  audit$maximum_absolute_difference <- c(
    NA,NA,NA,NA,
    max(abs(x$true_beta3-e$true_beta3)),
    max(abs(x$gamma_control-e$gamma_control)),
    max(abs(x$gamma_treatment-e$gamma_treatment)),
    max(abs(x$total_rate-e$total_rate_reported))
  )
  list(manifest=x, audit=audit, pass=all(audit$pass))
}

stage46_methods <- data.frame(
  method_order = 8:12,
  method = c(
    "M8_patient_clustered_GEE",
    "M9_single_process_IIW_GEE",
    "M10_separate_two_process_IIW_GEE",
    "M11_combined_two_process_IIW_GEE",
    "M12_oracle_combined_IIW_GEE"
  ),
  role = c("unweighted_reference","estimated_single_process_IIW",
           "estimated_separate_two_process_IIW","estimated_combined_two_process_IIW",
           "oracle_combined_IIW"),
  stringsAsFactors = FALSE
)

stage46_metrics <- data.frame(
  metric = c("mean_estimate","bias","relative_bias_percent","empirical_se",
             "mean_model_se","model_to_empirical_se_ratio","rmse","coverage_95",
             "rejection_rate_005","convergence_rate","mean_ci_width","mcse_bias",
             "mcse_coverage","mcse_rejection","minimum_ess_fraction","mean_ess_fraction",
             "maximum_raw_q99","mean_raw_q99","mean_fraction_truncated"),
  tier = c("primary","primary","primary_alt_only","primary","primary","primary",
           "primary","primary","primary","primary","supplementary","primary","primary",
           "primary","supplementary","supplementary","supplementary","supplementary",
           "supplementary"),
  stringsAsFactors = FALSE
)

run_stage46_final_design_freeze <- function(
  output_directory = "Stage46_FINAL_DESIGN_FREEZE",
  stage45_directory = "Stage45_VAR_intermediate_precision",
  manifest_path = NULL
) {
  if (is.null(manifest_path)) manifest_path <- stage46_find_file("stage42_frozen_var_manifest.csv")
  if (is.null(manifest_path)) stop("stage42_frozen_var_manifest.csv not found.")

  mcheck <- stage46_validate_manifest(manifest_path)
  if (!mcheck$pass) {
    print(mcheck$audit, row.names=FALSE)
    stop("Frozen Stage 42 manifest audit failed.")
  }

  if (!dir.exists(stage45_directory)) {
    f <- stage46_find_file("stage45_focus_summary.csv")
    if (!is.null(f)) stage45_directory <- dirname(f)
  }
  if (!dir.exists(stage45_directory)) stop("Stage45 output directory not found.")

  required45 <- c(
    "stage44_scenario_readiness.csv","stage44_seed_audit.csv",
    "stage45_m8_vs_iiw_bias_contrast.csv","stage45_estimated_vs_oracle_gap.csv",
    "stage45_se_inference_calibration.csv","stage45_mc_precision_summary.csv",
    "stage45_focus_summary.csv"
  )
  p45 <- file.path(stage45_directory, required45)
  if (!all(file.exists(p45))) stop("Stage45 evidence files are incomplete.")

  readiness <- read.csv(file.path(stage45_directory,"stage44_scenario_readiness.csv"))
  seed45 <- read.csv(file.path(stage45_directory,"stage44_seed_audit.csv"))
  stage45_pass <- nrow(readiness)==8 && all(as.logical(readiness$scenario_pass)) &&
    nrow(seed45)==1 && as.integer(seed45$expected_jobs[1])==400 &&
    isTRUE(as.logical(seed45$job_keys_unique[1])) &&
    isTRUE(as.logical(seed45$seeds_unique[1]))
  if (!stage45_pass) stop("Stage45 technical evidence did not pass.")

  design <- data.frame(
    parameter = c(
      "design_version","scenario_count","replicates_per_scenario","total_datasets",
      "n_patient","method_count","alpha","ci_level","seed_base","workers","batch_size",
      "checkpointing","failed_fit_policy","performance_denominator",
      "dgm_recalibration_after_freeze","weight_rule","primary_estimand",
      "mcse_probability_at_0.05_R1000","mcse_probability_at_0.50_R1000"
    ),
    value = c(
      "Stage46_FINAL_FREEZE_v1","8","1000","8000","500","5","0.05","0.95",
      "20460000","4","8","RDS checkpoint after every batch; exact-seed resume",
      "Record failed/non-converged fit; do not retune DGM or estimator",
      "Performance among successful fits; convergence against all attempted fits",
      "PROHIBITED","Frozen Stage41 IIW/truncation implementation unchanged",
      "group-by-time interaction beta3",
      sprintf("%.8f",sqrt(.05*.95/1000)),
      sprintf("%.8f",sqrt(.25/1000))
    ),
    stringsAsFactors = FALSE
  )

  seeds <- do.call(rbind, lapply(seq_len(nrow(mcheck$manifest)), function(s) {
    data.frame(
      scenario_index=s,
      scenario_id=mcheck$manifest$scenario_id[s],
      replicate=1:1000,
      seed=as.integer(20460000 + s*100000 + 1:1000)
    )
  }))
  rownames(seeds) <- NULL
  seed_audit <- data.frame(
    expected_jobs=8000L,
    actual_jobs=nrow(seeds),
    unique_job_keys=length(unique(paste(seeds$scenario_id,seeds$replicate))),
    unique_seeds=length(unique(seeds$seed)),
    pass=(nrow(seeds)==8000L &&
          length(unique(paste(seeds$scenario_id,seeds$replicate)))==8000L &&
          length(unique(seeds$seed))==8000L)
  )

  mcse <- data.frame(
    probability=c(.01,.025,.05,.10,.50,.90,.95,.975,.99),
    replicates=1000L
  )
  mcse$mcse <- sqrt(mcse$probability*(1-mcse$probability)/1000)

  files_to_hash <- c(
    "stage43_frozen_var_m8_m12_smoke_test_v1_1.R",
    "stage44_var_performance_pipeline_pilot.R",
    "stage45_var_intermediate_precision_pilot.R",
    "stage42_frozen_var_manifest.csv"
  )
  hash_paths <- vapply(files_to_hash, stage46_find_file, character(1))
  present <- nzchar(hash_paths) & file.exists(hash_paths)
  hashes <- rep(NA_character_, length(files_to_hash))
  hashes[present] <- unname(tools::md5sum(hash_paths[present]))
  hash_table <- data.frame(artifact=files_to_hash,path=hash_paths,md5=hashes,present=present)

  stage46_pass <- mcheck$pass && stage45_pass && isTRUE(seed_audit$pass[1]) && all(present)

  dir.create(output_directory, recursive=TRUE, showWarnings=FALSE)
  write.csv(design,file.path(output_directory,"stage46_final_simulation_design.csv"),row.names=FALSE)
  write.csv(mcheck$manifest,file.path(output_directory,"stage46_frozen_scenarios_full_precision.csv"),row.names=FALSE)
  write.csv(stage46_methods,file.path(output_directory,"stage46_frozen_methods.csv"),row.names=FALSE)
  write.csv(stage46_metrics,file.path(output_directory,"stage46_frozen_performance_metrics.csv"),row.names=FALSE)
  write.csv(seeds,file.path(output_directory,"stage46_seed_manifest_8000.csv"),row.names=FALSE)
  write.csv(seed_audit,file.path(output_directory,"stage46_seed_audit.csv"),row.names=FALSE)
  write.csv(mcse,file.path(output_directory,"stage46_mcse_targets.csv"),row.names=FALSE)
  write.csv(mcheck$audit,file.path(output_directory,"stage46_manifest_audit.csv"),row.names=FALSE)
  write.csv(hash_table,file.path(output_directory,"stage46_code_hashes.csv"),row.names=FALSE)

  freeze <- list(
    version="Stage46_FINAL_FREEZE_v1",
    frozen_at=format(Sys.time(),tz="Europe/Istanbul",usetz=TRUE),
    design=design, scenarios=mcheck$manifest, methods=stage46_methods,
    metrics=stage46_metrics, seeds=seeds, seed_audit=seed_audit,
    mcse_targets=mcse, manifest_audit=mcheck$audit, code_hashes=hash_table,
    stage45_pass=stage45_pass, stage46_pass=stage46_pass
  )
  saveRDS(freeze,file.path(output_directory,"stage46_final_design_freeze.rds"))

  cat("\nSTAGE 46 FINAL SIMULATION DESIGN FREEZE\n")
  cat("New simulation fitted: NO\n")
  cat("Outcome-driven DGM recalibration allowed: NO\n\n")
  cat("FINAL DESIGN\n")
  print(design,row.names=FALSE,right=FALSE)
  cat("\nMANIFEST AUDIT\n")
  print(mcheck$audit,row.names=FALSE,digits=6)
  cat("\nSEED AUDIT\n")
  print(seed_audit,row.names=FALSE)
  cat("\nMONTE CARLO PRECISION TARGETS\n")
  print(mcse,row.names=FALSE,digits=6)
  cat("\nCODE / ARTIFACT HASHES\n")
  print(hash_table,row.names=FALSE)
  cat("\nStage 46 decision:", if(stage46_pass) "PASS" else "REVIEW", "\n")

  invisible(freeze)
}

# Run:
# source("stage46_final_simulation_design_freeze.R")
# stage46 <- run_stage46_final_design_freeze(
#   output_directory = "Stage46_FINAL_DESIGN_FREEZE",
#   stage45_directory = "Stage45_VAR_intermediate_precision"
# )
