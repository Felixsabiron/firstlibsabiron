#' Creer un objet de type "commune"
#'
#' Cette fonction transforme un dataframe representant une commune en un objet de classe "commune".
#'
#' @param df Un dataframe contenant au moins la colonne `Code.de.la.commune`.
#'
#' @return Un dataframe de classe `"commune"`, qui peut etre utilise avec des fonctions specifiques aux communes.
#'
#' @details
#' - La fonction verifie que la colonne `Code.de.la.commune` est presente dans `df`.
#' - Elle s'assure que le dataframe represente une seule commune, sinon elle genere une erreur.
#' - La classe `"commune"` est ajoutee à l'objet pour faciliter son utilisation ultérieure.
#'
#' @examples
#' \dontrun{
#' data <- data.frame(
#'   Code.de.la.commune = rep("75056", 5),
#'   Nom.de.la.commune = rep("Paris", 5),
#'   Population = c(100, 200, 150, 180, 220)
#' )
#' commune <- creer_commune(data)
#' class(commune)  # Vérifier la classe
#' }
#'
#' @export
creer_commune <- function(df) {
 # Vérifier la présence de la colonne clé
 if (!"Code.de.la.commune" %in% colnames(df)) {
  stop("Le dataframe doit contenir la colonne 'Code.de.la.commune'.")
 }

 # Vérifier s'il y a plusieurs communes
 unique_communes <- unique(df$Code.de.la.commune)
 if (length(unique_communes) > 1) {
  stop("Le dataframe contient plusieurs communes. Fournissez les données d'une seule commune.")
 }

 # Ajouter la classe "commune"
 if (!inherits(df, "commune")) {
  class(df) <- c("commune", class(df))
 }

 return(df)
}
