# Publication-facing method labels.
publication_method_labels <- c(
  M8_patient_clustered_GEE = "M1",
  M9_single_process_IIW_GEE = "M2",
  M10_separate_two_process_IIW_GEE = "M3",
  M11_combined_two_process_IIW_GEE = "M4",
  M12_oracle_combined_IIW_GEE = "M5"
)
publication_method_label <- function(x) {
  y <- unname(publication_method_labels[x]); y[is.na(y)] <- x[is.na(y)]; y
}
