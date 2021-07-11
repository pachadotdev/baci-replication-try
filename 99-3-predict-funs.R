# flag
# 0 = no estimation 
# 2 = quantity estimation only 
# 4 = net weight estimation only 
# 6 = both quantity and net weight are estimated.

num_flag <- function(str) {
  case_when(
    str == 0 ~ "no estimation",
    str == 2 ~ "quantity estimation only",
    str == 4 ~ "net weight estimation only",
    str == 6 ~ "both quantity and net weight are estimated",
    TRUE ~ NA_character_
  )
}

fct_flag <- function(d) {
  d %>% 
    mutate(
      flag_exp = as_factor(num_flag(flag_exp)),
      flag_imp = as_factor(num_flag(flag_imp)),
    )
}

not_cif <- c(
  "dza", "geo",
  # sacu
  "zaf", "bwa", "lso", "nam", "swz"
)

