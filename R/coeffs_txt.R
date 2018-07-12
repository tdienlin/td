coeffs_txt <- function(object, selection, se = FALSE, 
                       indirect_effect = FALSE) {

  if(isTRUE(indirect_effect)) {
    paste0("\\textit{b} ",
           my_round(filter(object, label == selection)$est, "b_txt"),
           ", 95% CI [",
           my_round(filter(object, label == selection)$ll, 2),
           ", ",
           my_round(filter(object, label == selection)$ul, 2),
           "], $\\beta$ ",
           my_round(filter(object, label == selection)$std, "std_txt")
    )
  } else {
    paste0("$\\beta$ ",
           my_round(filter(object, label == selection)$std, "std_txt"),
           ", \\textit{b} ",
           my_round(filter(object, label == selection)$est, "b_txt"),
           ", 95% CI [",
           my_round(filter(object, label == selection)$ll, 2),
           ", ",
           my_round(filter(object, label == selection)$ul, 2),
           "], \\textit{z}(",
           filter(object, label == selection)$df,
           ") ",
           my_round(filter(object, label == selection)$z, "2_txt"),
           ", \\textit{p} ",
           my_round(filter(object, label == selection)$p, "p_txt")
    )
  }
}