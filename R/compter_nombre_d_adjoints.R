#' Compter le nombre d'élus uniques
#'
#' Cette fonction compte le nombre total d'élus présents dans un dataframe contenant leurs informations.
#'
#' @param data Un dataframe contenant au moins les colonnes `Nom.de.l.élu`, `Prénom.de.l.élu` et `Date.de.naissance`.
#'
#' @return Un entier représentant le nombre total d'élus dans le dataframe.
#'
#' @details
#' - La fonction vérifie la présence des colonnes obligatoires dans `data`.
#' - Elle compte le nombre total de lignes du dataframe, supposant que chaque ligne représente un élu unique.
#'
#' @examples
#' \dontrun{
#' # Exemple de dataframe
#' data <- data.frame(
#'   Nom.de.l.élu = c("Durand", "Martin", "Dupont", "Durand"),
#'   Prénom.de.l.élu = c("Jean", "Sophie", "Paul", "Jean"),
#'   Date.de.naissance = c("12/05/1950", "23/09/1965", "07/11/1948", "12/05/1950")
#' )
#'
#' # Compter le nombre total d'élus
#' compter_nombre_d_elus(data)
#' }
#'
#'
compter_nombre_d_elus <- function(data) {
 if (!all(c("Nom.de.l.élu", "Prénom.de.l.élu", "Date.de.naissance") %in% colnames(data))) {
  stop("Les colonnes de nom, prénom et date de naissance doivent être présentes dans le dataframe.")
 }

 nombre_unique <- nrow(data)

 return(nombre_unique)
}
