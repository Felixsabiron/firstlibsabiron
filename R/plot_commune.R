#' Visualiser la repartition des elus par code professionnel dans une commune
#'
#' Cette fonction genere un graphique en barres representant le nombre d'elus par code professionnel
#' pour une commune donnee.
#'
#' @description
#' La fonction verifie que l'objet fourni est bien de classe `"commune"`, valide sa structure avec `validate_schema()`,
#' puis produit un graphique en barres horizontales representant la repartition des elus par code professionnel.
#' Le titre du graphique est compose du nom de la commune et de son departement.
#'
#' @param x Un objet de classe "commune".
#' @param ... Arguments supplementaires passes a `ggplot2::ggplot()`.
#'
#' @return Un graphique `ggplot2` representant la repartition des elus par code professionnel dans la commune.
#'
#' @details
#' - La fonction utilise `validate_schema(x)` pour s'assurer que la structure des donnees est correcte.
#' - Elle verifie que `x` est bien un objet de classe `"commune"`, sinon elle renvoie une erreur.
#' - Elle compte le nombre d'elus par code professionnel et filtre ceux ayant `n = 0`.
#' - Un graphique en barres est genere, trie en ordre decroissant et affichant les valeurs sur les barres.
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' library(dplyr)
#'
#' # Exemple de dataframe representant une commune
#' data <- data.frame(
#'   Libelle.de.la.commune = rep("Paris", 6),
#'   Libelle.du.departement = rep("Paris", 6),
#'   Code.de.la.categorie.socio.professionnelle = c("A1", "B2", "A1", "C3", "B2", "A1")
#' )
#'
#' # Ajouter la classe "commune"
#' class(data) <- c("commune", class(data))
#'
#' # Generer le graphique
#' plot(data)
#' }
#'
#' @importFrom dplyr count filter arrange desc
#' @importFrom ggplot2 ggplot aes geom_bar geom_text labs theme_minimal
#' @importFrom stats reorder
#' @method plot commune
#' @export
plot.commune <- function(x, ...) {
 df <- x  # Assigne `x` a `df` pour garder le reste du code inchange

 # Verifier que le DataFrame respecte le schema attendu
 validate_schema(df)

 # Verifier que l'objet df est bien de la classe "commune"
 if (!inherits(df, "commune")) {
  stop("L'objet doit etre de classe 'commune'. Utilisez `creer_commune()` pour le transformer.")
 }

 # Extraire le nom de la commune et du departement
 nom_commune <- unique(df$Libelle.de.la.commune)
 nom_departement <- unique(df$Libelle.du.departement)

 # Compter le nombre d'elus par code professionnel et filtrer ceux a 0
 count_professions <- df |>
  dplyr::count(Code.de.la.categorie.socio.professionnelle) |>
  dplyr::filter(n > 0) |>
  dplyr::arrange(dplyr::desc(n))

 # Construire le titre et l'axe des abscisses
 titre_graphique <- paste(nom_commune, "-", nom_departement)
 axe_x <- paste("Libelles des codes professionnels pour les", sum(count_professions$n), "elus")

 # Generer le graphique en barres horizontal
 ggplot2::ggplot(count_professions, ggplot2::aes(x = n, y = stats::reorder(Code.de.la.categorie.socio.professionnelle, n))) +
  ggplot2::geom_bar(stat = "identity", fill = "darkblue") +
  ggplot2::geom_text(ggplot2::aes(label = n), hjust = -0.2, color = "black", size = 2) +  # Ajoute les etiquettes des valeurs
  ggplot2::labs(
   title = titre_graphique,
   x = axe_x,
   y = "Code professionnel"
  ) +
  ggplot2::theme_minimal()
}
