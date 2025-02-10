#' Créer un objet de type "departement"
#'
#' Cette fonction transforme un dataframe représentant un seul département en un objet de classe "departement".
#'
#' @description
#' La fonction vérifie que le dataframe ne contient qu'un seul département distinct (basé sur `Code.du.département`),
#' et lui attribue la classe `"departement"` s'il ne l'a pas déjà.
#'
#' @param df Un dataframe contenant au moins la colonne suivante :
#'   - `Code.du.département` (character ou numeric) : Le code identifiant le département.
#'
#' @return Un dataframe de classe `"departement"`, prêt à être utilisé dans d'autres fonctions spécifiques aux départements.
#'
#' @details
#' - La fonction compte les valeurs uniques dans la colonne `Code.du.département` pour s'assurer qu'il n'y a **qu'un seul département**.
#' - Si plusieurs départements sont détectés, la fonction s'arrête avec un message d'erreur.
#' - Si la classe `"departement"` n'est pas déjà présente, elle est ajoutée à l'objet.
#'
#' @examples
#' \dontrun{
#' # Exemple de dataframe valide avec un seul département
#' data <- data.frame(
#'   Code.du.département = rep("75", 5),  # Paris
#'   Nom.du.département = rep("Paris", 5),
#'   Population = c(100, 200, 150, 180, 220)
#' )
#'
#' # Créer l'objet departement
#' departement <- creer_departement(data)
#' class(departement)  # Vérifier la classe
#' }
#'
#' @export
creer_departement <- function(df) {
 # Vérifier si le DataFrame est composé de plusieurs départements
 unique_departments <- df$Code.du.département |>
  unique() |>
  length()

 if (unique_departments > 1) {
  stop("Le DataFrame est composé de plusieurs départements. Veuillez fournir les données d'un seul département.")
 }

 # Ajouter la classe "departement" si elle est absente
 if (!inherits(df, "departement")) {
  class(df) <- c("departement", class(df))
 }

 return(df)
}
