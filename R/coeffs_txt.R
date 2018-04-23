coeff_txt <- function(object, selection, se = FALSE){
  paste0("(\\textit{b} = ",
         filter(object, label == selection)$b,
         ", 95% CI [",
         filter(object, label == selection)$ll,
         ", ",
         filter(object, label == selection)$ul,
         "], \\textit{z} = ",
         filter(object, label == selection)$z,
         ", \\textit{p} ",
         filter(object, label == selection)$p_txt,
         ", $\\beta$ = ",
         filter(object, label == selection)$beta,
         ")"
  )
}