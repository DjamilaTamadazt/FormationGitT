# fonction de stat agregee
calcul_stat_agregee <- function(data, stat = "moyenne", ...) {
  if (stat == "moyenne") {
    resultat <- mean(data, na.rm = TRUE, ...)
  } else if (stat == "ecart-type" || stat == "sd") {
    resultat <- sd(data, na.rm = TRUE, ...)
  } else if (stat == "variance") {
    resultat <- var(data, na.rm = TRUE, ...)
  }
  return(resultat)
}

#' Title
#'
#' @param annee : nombre entier de taille 4
#'
#' @return Renvoie la décennie sur 4 positions de type numérique
#' @export
#'
#' @examples calcul décennie annee (2016) renvoi 2010
calcul_deccenie_annee <- function(annee) {
  return(annee - annee %% 10)
}