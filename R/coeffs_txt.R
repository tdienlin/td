coeffs_txt <- function(object, selection, se = FALSE){
  paste0("(\\textit{b} = ",
         round(filter(object, label == selection)$est, 2),
         ", 95% CI [",
         round(filter(object, label == selection)$ll, 2),
         ", ",
         round(filter(object, label == selection)$ul, 2),
         "], \\textit{z} = ",
         round(filter(object, label == selection)$z, 2),
         ", \\textit{p} ",
         my_round(filter(object, label == selection)$p, "p_txt"),
         ", $\\beta$ = ",
         my_round(filter(object, label == selection)$std, "std"),
         ")"
  )
}