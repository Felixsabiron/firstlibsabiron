#' Visualiser la répartition des élus par code professionnel
#'
#' Cette fonction génère un graphique en barres horizontales représentant le nombre d'élus par code professionnel.
#'
#' @description
#' La fonction compte le nombre d'élus pour chaque code professionnel (`Code.de.la.catégorie.socio.professionnelle`),
#' filtre les codes n'ayant aucun élu, puis produit un graphique en barres classé par ordre décroissant.
#'
#' @param data Un dataframe contenant au moins la colonne suivante :
#'   - `Code.de.la.catégorie.socio.professionnelle` (character) : Code professionnel des élus.
#'
#' @return Un graphique `ggplot2` représentant la répartition des élus par code professionnel.
#'
#' @details
#' - Les valeurs à `n = 0` sont filtrées avant l'affichage du graphique.
#' - Le graphique est trié par nombre d'élus de manière décroissante.
#' - Les barres sont colorées en bleu foncé pour une meilleure lisibilité.
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' library(dplyr)
#'
#' # Exemple de dataframe
#' data <- data.frame(
#'   Code.de.la.catégorie.socio.professionnelle = c("A1", "B2", "A1", "C3", "B2", "A1")
#' )
#'
#' # Générer le graphique
#' plot_code_professions(data)
#' }
#'
#' @importFrom dplyr count filter arrange
#' @importFrom ggplot2 ggplot aes geom_bar labs theme_minimal
#'
plot_code_professions <- function(data) {
 if (!"Code.de.la.catégorie.socio.professionnelle" %in% colnames(data)) {
  stop("La colonne 'Code.de.la.catégorie.socio.professionnelle' doit être présente dans le dataframe.")
 }

 # Compter le nombre d'élus par code professionnel et filtrer ceux à 0
 count_professions <- data |>
  dplyr::count(Code.de.la.catégorie.socio.professionnelle) |>
  dplyr::filter(n > 0) |>
  dplyr::arrange(desc(n))

 # Générer le bar chart horizontal
 ggplot2::ggplot(count_professions, ggplot2::aes(x = n, y = reorder(Code.de.la.catégorie.socio.professionnelle, n))) +
  ggplot2::geom_bar(stat = "identity", fill = "darkblue") +
  ggplot2::labs(title = "Nombre d'élus par code professionnel",
                x = "Nombre d'élus",
                y = "Code professionnel") +
  ggplot2::theme_minimal()
}
