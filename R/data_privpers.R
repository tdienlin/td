#' Dataset "Privacy and Personality"
#'
#' Data with several measures of privacy and personality variables. For more information (e.g., the codebook with a list of all items), see \href{https://osf.io/st7p6/}{https://osf.io/st7p6/}.
#'
#' @docType data
#'
#' @usage perspriv
#'
#' @format A data frame with 296 rows and 223 variables. Answer format was a 7-point Likert scale.
#' \describe{
#'   \item{pri_att_soc}{Privacy Attitude Societal}
#'   \item{pri_att_int}{Privacy Attitude Interpersonal}
#'   \item{pri_con_soc}{Privacy Concerns Societal}
#'   \item{pri_con_int}{Privacy Concerns Interpersonal}
#'   \item{pri_val_gen}{Privacy Values General}
#'   \item{pri_val_soc}{Privacy Values Societal}
#'   \item{pri_val_soc_sce}{Privacy Values Societal Scenario Approach}
#'   \item{pri_val_int}{Privacy Values Interpersonal}
#'   \item{pri_val_int_sce}{Privacy Values Interpersonal Scenario Approach}
#'   \item{pri_nee_gen}{Privacy Need General}
#'   \item{pri_nee_soc}{Privacy Need Societal}
#'   \item{pri_nee_int}{Privacy Need Interpersonal}
#'   \item{pri_beh_soc}{Privacy Behavior Societal}
#'   \item{pri_beh_int}{Privacy Behavior Interpersonal}
#'   \item{pri_beh_exp_neg_soc}{Privacy Experiences Negative Societal}
#'   \item{pri_beh_exp_neg_int}{Privacy Experiences Negative Interpersonal}
#'   \item{pri_beh_exp_pos_soc}{Privacy Experiences Positive Societal}
#'   \item{pri_beh_exp_pos_int}{Privacy Experiences Positive Interpersonal}
#'   \item{pri_nor_gen}{Privacy Subjective Norm General}
#'   \item{pri_nor_soc}{Privacy Subjective Norm Societal}
#'   \item{pri_nor_int}{Privacy Subjective Norm Interpersonal}
#'   \item{itg}{Integrity}
#'   \item{soc}{Sociability}
#'   \item{anx}{Anxiety}
#'   \item{tra}{Traditionalism}
#'   \item{ria}{Risk Avoidance}
#'   \item{bfi}{Big Five}
#'   \item{soc_des}{Social Desirability}
#'   \item{sex}{Sex}
#'   \item{age}{Age}
#'   \item{eth}{Ethnicity}
#'   \item{edu}{Education}
#'   \item{edu_par}{Education Parents}
#'   \item{job_par_1}{Job Parent 1}
#'   \item{job_par_2}{Job Parent 2}
#'   \item{inc}{Income}
#'   \item{inc_par}{Income Parents}
#' }
#'
#' @keywords datasets, privacy, personality.
#'
#' @references Dienlin, T., & Metzger, M. (2019). Measures of Privacy and Personality. Retrieved from osf.io/st7p6
#'
#' @source \href{https://osf.io/cqp46/}{Open Science Framework}
#'
#' @examples
#' d <- td::privpers
"privpers"