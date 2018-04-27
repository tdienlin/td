coeffs_txt <- function(object, selection, se = FALSE){
  paste0("\\textit{b} ",
         my_round(filter(object, label == selection)$est, "b_txt"),
         ", 95% CI [",
         my_round(filter(object, label == selection)$ll, 2),
         ", ",
         my_round(filter(object, label == selection)$ul, 2),
         "], \\textit{z} ",
         my_round(filter(object, label == selection)$z, "2_txt"),
         ", \\textit{p} ",
         my_round(filter(object, label == selection)$p, "p_txt"),
         ", $\\beta$ ",
         my_round(filter(object, label == selection)$std, "std_txt")
  )
}