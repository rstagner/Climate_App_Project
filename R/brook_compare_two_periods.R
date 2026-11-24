# ==============================================================================
# 1. SETUP & DATA LOADING
# ==============================================================================
library(tidyverse)

# Load joined daily climate dataset containing NOAA station and NASA POWER columns
climate_joined <- readRDS("data/climate_joined.rds")

# ==============================================================================
# 2. PLAIN-LANGUAGE EXPLANATION HELPER ("WHAT THIS MEANS")
# ==============================================================================
build_period_explanation <- function(city_label,
                                     var_label,
                                     source_label,
                                     a_span,
                                     b_span,
                                     a_mean,
                                     b_mean,
                                     diff_val,
                                     p_val_txt,
                                     p_num,
                                     test_label,
                                     n_a,
                                     n_b) {
  # Describe whether Period B is warmer, cooler, or unchanged relative to Period A
  direction_txt <- if (diff_val > 0) {
    paste0(sprintf("%+.2f", diff_val), " higher in Period B (", b_span, ") than Period A (", a_span, ")")
  } else if (diff_val < 0) {
    paste0(sprintf("%+.2f", diff_val), " lower in Period B (", b_span, ") than Period A (", a_span, ")")
  } else {
    "0.00 difference between Period A and Period B"
  }
  
  # Summarize statistical significance using the selected two-sample test
  sig_txt <- if (p_num < 0.05) {
    paste0(
      "Because p is ", p_val_txt, " (< 0.05), the difference between the two periods is statistically significant using a ",
      test_label, "."
    )
  } else {
    paste0(
      "Because p is ", p_val_txt, " (>= 0.05), the difference between the two periods is not statistically significant using a ",
      test_label, "."
    )
  }
  
  # Assemble a clear, student- and teacher-friendly takeaway paragraph
  paste0(
    city_label, " (", var_label, ", ", source_label, ") has a Period A (",
    a_span, ") mean of ", sprintf("%.2f", a_mean), " and a Period B (", b_span, ") mean of ",
    sprintf("%.2f", b_mean), "—a shift of ", direction_txt, ". ", sig_txt,
    " (Sample sizes: n = ", n_a, " daily records in Period A and n = ",
    n_b, " in Period B.)"
  )
}
