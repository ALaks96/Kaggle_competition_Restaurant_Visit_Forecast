# Creating history file:
usethis::use_build_ignore("devtools_history.R")
usethis::use_package("conflicted")
remotes::install_github("Thinkr-open/attachment")

# Creating vignette
devtools::document()

# Creating zzz.R script

# Creating one hot encoding functions (visit_fct.R, reserve_fct.R, features.R)
usethis::use_pipe()
attachment::att_to_description()
conflicted::conflict_scout()
devtools::check()

# Creating data formatting functions (data_prep.R)
attachment::att_to_description()
conflicted::conflict_scout()
devtools::check()

# Creating xgboost function (xgb_fcts.R)
attachment::att_to_description()
conflicted::conflict_scout()
devtools::check()

# Creating prediction evaluation functions (pred_eval.R)
attachment::att_to_description()
conflicted::conflict_scout()
devtools::check()

# Creating shinyapp functions (shiny_fcts.R)
attachment::att_to_description()
conflicted::conflict_scout()
devtools::check()

# Completing vignette, checking it runs etc.. and final check

devtools::check()
