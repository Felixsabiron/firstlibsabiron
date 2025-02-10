#' Calculer la distribution des âges des élus
#'
#' Cette fonction calcule la distribution des âges à partir d'une colonne `Date.de.naissance` en format **Date**.
#'
#' @param data Un dataframe contenant au moins la colonne suivante :
#'   - `Date.de.naissance` (Date) : La date de naissance des élus, qui doit être déjà convertie en format `Date`.
#'
#' @return Un vecteur numérique représentant les âges des élus en années.
#'
#' @details
#' - La fonction vérifie la présence de la colonne `Date.de.naissance`.
#' - Elle vérifie que la colonne est bien de type **Date**, sinon elle renvoie une erreur.
#' - Elle calcule l'âge en années en utilisant la différence entre la date du jour (`Sys.Date()`) et la `Date.de.naissance`.
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#' library(lubridate)
#'
#' # Exemple de dataframe avec la colonne en format Date
#' data <- data.frame(
#'   Date.de.naissance = as.Date(c("1950-05-12", "1965-09-23", "1948-11-07"))
#' )
#'
#' # Calculer la distribution des âges
#' calcul_distribution_age(data)
#' }
#'
#'
calcul_distribution_age <- function(data) {
 if (!"Date.de.naissance" %in% colnames(data)) {
  stop("La colonne 'Date.de.naissance' doit être présente dans le dataframe.")
 }

 # Vérification que la colonne "Date.de.naissance" est de type Date
 if (!inherits(data$Date.de.naissance, "Date")) {
  stop("La colonne 'Date.de.naissance' doit être au format Date. Utilisez as.Date() pour la convertir.")
 }

 # Calcul des âges en années
 ages <- as.numeric(difftime(Sys.Date(), data$Date.de.naissance, units = "days")) %/% 365

 return(ages)
}
