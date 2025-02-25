#' Compter le nombre d'elus uniques
#'
#' Cette fonction compte le nombre total d'elus presents dans un dataframe contenant leurs informations.
#'
#' @param data Un dataframe contenant au moins les colonnes `Nom.de.l.elu`, `Prenom.de.l.elu` et `Date.de.naissance`.
#'
#' @return Un entier representant le nombre total d'elus dans le dataframe.
#'
#' @details
#' - La fonction verifie la presence des colonnes obligatoires dans `data`.
#' - Elle compte le nombre total de lignes du dataframe, supposant que chaque ligne represente un elu unique.
#'
#' @examples
#' \dontrun{
#' # Exemple de dataframe
#' data <- data.frame(
#'   Nom.de.l.elu = c("Durand", "Martin", "Dupont", "Durand"),
#'   Prenom.de.l.elu = c("Jean", "Sophie", "Paul", "Jean"),
#'   Date.de.naissance = c("12/05/1950", "23/09/1965", "07/11/1948", "12/05/1950")
#' )
#'
#' # Compter le nombre total d'elus
#' compter_nombre_d_elus(data)
#' }
#'
#'
compter_nombre_d_elus <- function(data) {
 if (!all(c("Nom.de.l.elu", "Prenom.de.l.elu", "Date.de.naissance") %in% colnames(data))) {
  stop("Les colonnes de nom, prenom et date de naissance doivent etre presentes dans le dataframe.")
 }

 nombre_unique <- nrow(data)

 return(nombre_unique)
}
