#' Visualiser la repartition des elus par code professionnel dans un departement
#'
#' Cette fonction genere un graphique en barres representant le nombre d'elus par code professionnel
#' pour un departement donne.
#'
#' @description
#' La fonction verifie que l'objet fourni est bien de classe `"departement"`, valide sa structure avec `validate_schema()`,
#' puis produit un graphique en barres horizontales representant la repartition des elus par code professionnel.
#' Le graphique affiche les **10 codes professionnels les plus representes** dans le departement.
#' Le titre du graphique inclut le nom du departement et le nombre total de communes qu'il contient.
#'
#' @param x Un objet de classe "departement".
#' @param ... Arguments supplementaires passes a `ggplot2::ggplot()`.
#'
#' @return Un graphique `ggplot2` representant la repartition des elus par code professionnel dans le departement.
#'
#' @details
#' - La fonction utilise `validate_schema(x)` pour s'assurer que la structure des donnees est correcte.
#' - Elle verifie que `x` est bien un objet de classe `"departement"`, sinon elle renvoie une erreur.
#' - Elle compte le nombre total de communes distinctes presentes dans le departement.
#' - Elle filtre et selectionne les **10 codes professionnels les plus representes**.
#' - Un graphique en barres est genere, trie en ordre decroissant et affichant les valeurs sur les barres.
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' library(dplyr)
#'
#' # Exemple de dataframe representant un departement
#' data <- data.frame(
#'   Libelle.du.departement = rep("Paris", 15),
#'   Libelle.de.la.commune = c(rep("Paris", 10), rep("Boulogne-Billancourt", 5)),
#'   Code.de.la.categorie.socio.professionnelle = sample(LETTERS[1:5], 15, replace = TRUE)
#' )
#'
#' # Ajouter la classe "departement"
#' class(data) <- c("departement", class(data))
#'
#' # Generer le graphique
#' plot(data)
#' }
#'
#' @importFrom dplyr count filter arrange distinct slice_head desc
#' @importFrom ggplot2 ggplot aes geom_bar geom_text labs theme_minimal
#' @importFrom stats reorder
#' @method plot departement
#' @export
plot.departement <- function(x, ...) {
 df <- x  # Assigne `x` a `df` pour garder le reste du code inchange

 # Verifier que le DataFrame respecte la structure minimale
 validate_schema(df)

 # Verifier que l'objet df est bien de la classe "departement"
 if (!inherits(df, "departement")) {
  stop("L'objet doit etre de classe 'departement'. Utilisez `creer_departement()` pour le transformer.")
 }

 # Extraire le nom du departement
 nom_departement <- unique(df$Libelle.du.departement)

 # Calculer le nombre de communes distinctes dans le departement
 nb_communes <- df |>
  dplyr::distinct(Libelle.de.la.commune) |>
  nrow()

 # Compter le nombre d'elus par code professionnel et filtrer les 10 plus frequents
 count_professions <- df |>
  dplyr::count(Code.de.la.categorie.socio.professionnelle) |>
  dplyr::filter(n > 0) |>
  dplyr::arrange(dplyr::desc(n)) |>
  dplyr::slice_head(n = 10) # Selectionner les 10 codes professionnels les plus representes

 # Construire le titre et l'axe des abscisses
 titre_graphique <- paste(nom_departement, "-", nb_communes, "communes")
 axe_x <- paste("Libelles des 10 codes professionnels les plus representes pour", nom_departement)

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
