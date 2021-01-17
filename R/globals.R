# Set global variables
global_vars <- c(".",".data", ".id", ".loss",
                 ".original_data", ".outcome", ".placebo",
                 ".predictor_weights",
                 ".predictors", ".synthetic_control",
                 ".type", ".unit_weights", "Observed", "Synthetic",
                 "mspe", "mspe_ratio", "name", "period", "post_mspe",
                 "pre_mspe", "real_y", "sd", "synth_y",
                 "time_unit", "type", "type_text", "unit",
                 "unit_name", "value", "value_adjusted", "var",
                 "variable", "w.weight", "weight")
utils::globalVariables(global_vars)
