# _targets.R file:
library(targets)
source("R/functions.R")

controller <- crew::crew_controller_local(
  name = "Controller",
  workers = 5,
  seconds_idle = 3
)
tar_option_set(controller = controller)
list(
  tar_target(x, seq_len(1000)),
  tar_target(y, Sys.sleep(1), pattern = map(x)) # Run for 1 second.
)