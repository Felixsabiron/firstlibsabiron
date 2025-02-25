#' Visualiser la repartition des elus par code professionnel
#'
#' Cette fonction genere un graphique en barres horizontales representant le nombre d'elus par code professionnel.
#'
#' @description
#' La fonction compte le nombre d'elus pour chaque code professionnel (`Code.de.la.categorie.socio.professionnelle`),
#' filtre les codes n'ayant aucun elu, puis produit un graphique en barres classe par ordre decroissant.
#'
#' @param data Un dataframe contenant au moins la colonne suivante :
#'   - `Code.de.la.categorie.socio.professionnelle` (character) : Code professionnel des elus.
#'
#' @return Un graphique `ggplot2` representant la repartition des elus par code professionnel.
#'
#' @details
#' - Les valeurs a `n = 0` sont filtrees avant l'affichage du graphique.
#' - Le graphique est trie par nombre d'elus de maniere decroissante.
#' - Les barres sont colorees en bleu fonce pour une meilleure lisibilite.
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' library(dplyr)
#'
#' # Exemple de dataframe
#' data <- data.frame(
#'   Code.de.la.categorie.socio.professionnelle = c("A1", "B2", "A1", "C3", "B2", "A1")
#' )
#'
#' # Generer le graphique
#' plot_code_professions(data)
#' }
#'
#' @importFrom dplyr count filter arrange
#' @importFrom ggplot2 ggplot aes geom_bar labs theme_minimal
#'
plot_code_professions <- function(data) {
 if (!"Code.de.la.categorie.socio.professionnelle" %in% colnames(data)) {
  stop("La colonne 'Code.de.la.categorie.socio.professionnelle' doit etre presente dans le dataframe.")
 }

 # Compter le nombre d'elus par code professionnel et filtrer ceux a 0
 count_professions <- data |>
  dplyr::count(Code.de.la.categorie.socio.professionnelle) |>
  dplyr::filter(n > 0) |>
  dplyr::arrange(desc(n))

 # Generer le bar chart horizontal
 ggplot2::ggplot(count_professions, ggplot2::aes(x = n, y = reorder(Code.de.la.categorie.socio.professionnelle, n))) +
  ggplot2::geom_bar(stat = "identity", fill = "darkblue") +
  ggplot2::labs(title = "Nombre d'elus par code professionnel",
                x = "Nombre d'elus",
                y = "Code professionnel") +
  ggplot2::theme_minimal()
}
