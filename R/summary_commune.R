#' Resume pour les objets de classe "commune"
#'
#' Cette fonction genere un resume pour les objets de classe `commune`.
#' Elle affiche le nom de la commune, le nombre total d'elus et la repartition professionnelle.
#'
#' @param obj Un objet de classe `commune`.
#' @param ... Arguments supplementaires (non utilises).
#'
#' @return Un resume des informations de la commune sous forme de liste.
#' @export
summary_commune <- function(obj, ...) {
 # Verification de la classe
 if (!inherits(obj, "commune")) {
  stop("L'objet n'est pas de classe 'commune'")
 }

 # Creation du resume
 result <- list(
  nom_commune = unique(obj$Libelle.de.la.commune),
  nombre_elus = nrow(obj),
  repartition_professionnelle = obj %>%
   dplyr::count(Code.de.la.categorie.socio.professionnelle, name = "n")
 )

 # Attribution de la classe S3 pour l'affichage
 class(result) <- "summary.commune"

 return(result)
}

#' Methode S3 pour summary.commune
#'
#' Cette fonction redirige automatiquement `summary()` vers `summary_commune()`
#' pour les objets de classe `commune`.
#'
#' @param object Un objet de classe `commune`.
#' @param ... Arguments supplementaires (non utilises).
#'
#' @export
summary.commune <- function(object, ...) {
 summary_commune(object, ...)
}

#' Affichage du resume pour summary.commune
#'
#' Cette fonction definit l'affichage personnalise pour les objets de classe `summary.commune`.
#'
#' @param x Un objet de classe `summary.commune`.
#' @param ... Arguments supplementaires (non utilises).
#'
#' @return NULL (resume affiche dans la console)
#' @export
print.summary.commune <- function(x, ...) {
 cat("Resume de la commune :\n")
 cat("Nom de la commune :", x$nom_commune, "\n")
 cat("Nombre total d'elus :", x$nombre_elus, "\n")
 cat("\nRepartition professionnelle :\n")
 print(x$repartition_professionnelle)
}
