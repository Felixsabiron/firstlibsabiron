#' Créer un objet de type "commune"
#'
#' Cette fonction permet de transformer un dataframe représentant une seule commune en un objet de classe "commune".
#'
#' @description
#' La fonction vérifie que le dataframe respecte la structure attendue via `validate_schema()`,
#' s'assure qu'il ne contient qu'une seule commune distincte (basée sur `Code.de.la.commune`),
#' et lui attribue la classe `"commune"` s'il ne l'a pas déjà.
#'
#' @param df Un dataframe contenant au moins la colonne suivante :
#'   - `Code.de.la.commune` (character ou numeric) : Le code identifiant la commune.
#'
#' @return Un dataframe de classe `"commune"`, prêt à être utilisé dans d'autres fonctions spécifiques aux communes.
#'
#' @details
#' - `validate_schema(df)` est appelé pour valider la structure du dataframe avant tout traitement.
#' - La fonction vérifie qu'il n'y a **qu'une seule commune distincte** en comptant les valeurs uniques de `Code.de.la.commune`.
#' - Si plusieurs communes sont détectées, la fonction s'arrête avec un message d'erreur.
#' - Si la classe `"commune"` n'est pas déjà présente, elle est ajoutée à l'objet.
#'
#' @examples
#' \dontrun{
#' # Exemple de dataframe valide avec une seule commune
#' data <- data.frame(
#'   Code.de.la.commune = rep("75056", 5),  # Paris
#'   Nom.de.la.commune = rep("Paris", 5),
#'   Population = c(100, 200, 150, 180, 220)
#' )
#'
#' # Créer l'objet commune
#' commune <- creer_commune(data)
#' class(commune)  # Vérifier la classe
#' }
#'
#' @export
creer_commune <- function(df) {
 # Vérifier que le DataFrame respecte la structure minimale
 validate_schema(df)

 # Vérifier si le DataFrame est composé de plusieurs communes
 unique_communes_code <- df$Code.de.la.commune |>
  unique() |>
  length()

 if (unique_communes_code > 1) {
  stop("Le DataFrame est composé de plusieurs communes. Veuillez fournir les données d'une seule commune.")
 }

 # Ajouter la classe "commune" au df si elle n'est pas présente
 if (!inherits(df, "commune")) {
  class(df) <- c("commune", class(df))
 }

 # Retourner le DataFrame avec la nouvelle classe
 return(df)
}
