#' Créer un objet de type "departement"
#'
#' Cette fonction transforme un dataframe représentant un département en un objet de classe "departement".
#'
#' @param df Un dataframe contenant au moins la colonne `Code.du.département`.
#'
#' @return Un dataframe de classe `"departement"`, qui peut être utilisé avec des fonctions spécifiques aux départements.
#'
#' @details
#' - La fonction vérifie que la colonne `Code.du.département` est présente dans `df`.
#' - Elle s'assure que le dataframe représente un seul département, sinon elle génère une erreur.
#' - La classe `"departement"` est ajoutée à l'objet pour permettre un traitement spécifique.
#'
#' @examples
#' \dontrun{
#' data <- data.frame(
#'   Code.du.département = rep("75", 10),
#'   Nom.du.département = rep("Paris", 10),
#'   Population = c(100, 200, 150, 180, 220, 250, 300, 270, 260, 280)
#' )
#' departement <- creer_departement(data)
#' class(departement)  # Vérifier la classe
#' }
#'
#' @export
creer_departement <- function(df) {
 # Vérifier la présence de la colonne clé
 if (!"Code.du.département" %in% colnames(df)) {
  stop("Le dataframe doit contenir la colonne 'Code.du.département'.")
 }

 # Vérifier s'il y a plusieurs départements
 unique_departements <- unique(df$Code.du.département)
 if (length(unique_departements) > 1) {
  stop("Le dataframe contient plusieurs départements. Fournissez les données d'un seul département.")
 }

 # Ajouter la classe "departement"
 if (!inherits(df, "departement")) {
  class(df) <- c("departement", class(df))
 }

 return(df)
}
