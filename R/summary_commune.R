#' Résumé pour les objets de classe commune
#'
#' Cette fonction génère un résumé pour les objets de classe `commune`.
#' Elle affiche le nom de la commune et le nombre total d'élus.
#'
#' @param obj Un objet de classe `commune`.
#' @param ... Arguments supplémentaires (non utilisés).
#'
#' @return Un résumé des informations de la commune sous forme de liste.
#' @export
#' @method summary commune
summary.commune <- function(obj, ...) {
 if (!inherits(obj, "commune")) {
  stop("L'objet n'est pas de classe 'commune'")
 }

 result <- list(
  nom_commune = unique(obj$Libelle.de.la.commune),
  nombre_elus = nrow(obj),
  repartition_professionnelle = obj %>%
   dplyr::count(Code.de.la.categorie.socio.professionnelle, name = "n")
 )

 class(result) <- "summary.commune"  # 🔥 Spécification explicite de la classe

 return(result)
}

