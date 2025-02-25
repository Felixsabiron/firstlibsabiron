#' Creer un objet de type "departement"
#'
#' Cette fonction transforme un dataframe representant un departement en un objet de classe "departement".
#'
#' @param df Un dataframe contenant au moins la colonne `Code.du.departement`.
#'
#' @return Un dataframe de classe `"departement"`, qui peut etre utilise avec des fonctions specifiques aux departements.
#'
#' @details
#' - La fonction verifie que la colonne `Code.du.departement` est presente dans `df`.
#' - Elle s'assure que le dataframe represente un seul departement, sinon elle genere une erreur.
#' - La classe `"departement"` est ajoutee a l'objet pour permettre un traitement specifique.
#'
#' @examples
#' \dontrun{
#' data <- data.frame(
#'   Code.du.departement = rep("75", 10),
#'   Nom.du.departement = rep("Paris", 10),
#'   Population = c(100, 200, 150, 180, 220, 250, 300, 270, 260, 280)
#' )
#' departement <- creer_departement(data)
#' class(departement)  # Verifier la classe
#' }
#'
#' @export
creer_departement <- function(df) {
 # Verifier la presence de la colonne cle
 if (!"Code.du.departement" %in% colnames(df)) {
  stop("Le dataframe doit contenir la colonne 'Code.du.departement'.")
 }

 # Verifier s'il y a plusieurs departements
 unique_departements <- unique(df$Code.du.departement)
 if (length(unique_departements) > 1) {
  stop("Le dataframe contient plusieurs departements. Fournissez les donnees d'un seul departement.")
 }

 # Ajouter la classe "departement"
 if (!inherits(df, "departement")) {
  class(df) <- c("departement", class(df))
 }

 return(df)
}

