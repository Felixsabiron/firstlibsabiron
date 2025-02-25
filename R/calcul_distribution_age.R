#' Calculer la distribution des ages des elus
#'
#' Cette fonction calcule la distribution des ages a partir d'une colonne `Date.de.naissance` en format **Date**.
#'
#' @param data Un dataframe contenant au moins la colonne suivante :
#'   - `Date.de.naissance` (Date) : La date de naissance des elus, qui doit etre deja convertie en format `Date`.
#'
#' @return Un vecteur numerique representant les ages des elus en annees.
#'
#' @details
#' - La fonction verifie la presence de la colonne `Date.de.naissance`.
#' - Elle verifie que la colonne est bien de type **Date**, sinon elle renvoie une erreur.
#' - Elle calcule l'age en annees en utilisant la difference entre la date du jour (`Sys.Date()`) et la `Date.de.naissance`.
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
#' # Calculer la distribution des ages
#' calcul_distribution_age(data)
#' }
#'
#'
calcul_distribution_age <- function(data) {
 if (!"Date.de.naissance" %in% colnames(data)) {
  stop("La colonne 'Date.de.naissance' doit etre presente dans le dataframe.")
 }

 # Verification que la colonne "Date.de.naissance" est de type Date
 if (!inherits(data$Date.de.naissance, "Date")) {
  stop("La colonne 'Date.de.naissance' doit etre au format Date. Utilisez as.Date() pour la convertir.")
 }

 # Calcul des ages en annees
 ages <- as.numeric(difftime(Sys.Date(), data$Date.de.naissance, units = "days")) %/% 365

 return(ages)
}
