#' Resume pour les objets de classe departement
#'
#' Cette fonction genere un resume pour les objets de classe `departement`.
#' Elle affiche le nom du departement, le nombre total de communes, et le nombre total d'elus.
#'
#' @param object Un objet de classe `departement`.
#' @param ... Arguments supplementaires (non utilises).
#'
#' @return Un resume des informations du departement sous forme de liste.
#' @export
#' @method summary departement
summary.departement <- function(object, ...) {

 # Verification de la classe de l'objet
 if (!inherits(object, "departement")) {
  stop("L'objet n'est pas de classe 'departement'")
 }

 # Calcul du nom du departement
 nom_departement <- unique(object$Libelle.du.departement)

 # Calcul du nombre total de communes
 nb_communes <- object |>
  dplyr::distinct(Code.de.la.commune) |>
  nrow()

 # Calcul du nombre total d'elus
 nb_elus <- nrow(object)

 # Creation du resume sous forme de liste
 result <- list(
  nom_departement = nom_departement,
  nombre_communes = nb_communes,
  nombre_elus = nb_elus
 )

 # Attribution de la classe summary.departement a la liste
 class(result) <- "summary.departement"

 # Retourner le resultat
 return(result)
}

#' Affichage du resume pour summary.departement
#'
#' Cette fonction definit l'affichage personnalise pour les objets de classe `summary.departement`.
#'
#' @param object Un objet de classe `summary.departement`.
#' @param ... Arguments supplementaires (non utilises).
#'
#' @return NULL (resume affiche dans la console)
#' @export
print.summary.departement <- function(object, ...) {
 cat("Resume du departement :\n")
 cat("Nom du departement :", object$nom_departement, "\n")
 cat("Nombre total de communes :", object$nombre_communes, "\n")
 cat("Nombre total d'elus :", object$nombre_elus, "\n")
}
