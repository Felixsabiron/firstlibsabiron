#' Trouver l'élu(e) le plus âgé(e)
#'
#' Cette fonction identifie l'élu(e) le plus âgé(e) dans un dataframe contenant des informations sur les élus.
#'
#' @param data Un dataframe contenant au moins les colonnes suivantes :
#'   - `Nom.de.l.élu` (character) : Le nom de l'élu(e).
#'   - `Prénom.de.l.élu` (character) : Le prénom de l'élu(e).
#'   - `Date.de.naissance` (character) : La date de naissance sous format `jour/mois/année` (ex: "12/05/1950").
#'
#' @return Un dataframe avec les informations de l'élu(e) le plus âgé(e), contenant les colonnes :
#'   - `Nom.de.l.élu`
#'   - `Prénom.de.l.élu`
#'   - `Date.de.naissance` (au format Date).
#'
#' @details
#' - La fonction vérifie la présence des colonnes requises.
#' - Elle convertit la colonne `Date.de.naissance` en format Date avec `lubridate::dmy()`.
#' - Elle sélectionne l'élu(e) avec la date de naissance la plus ancienne.
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#' library(lubridate)
#'
#' # Exemple de dataframe
#' data <- data.frame(
#'   Nom.de.l.élu = c("Durand", "Martin", "Dupont"),
#'   Prénom.de.l.élu = c("Jean", "Sophie", "Paul"),
#'   Date.de.naissance = c("12/05/1950", "23/09/1965", "07/11/1948")
#' )
#'
#' # Trouver l'élu le plus âgé
#' trouver_l_elus_le_plus_age(data)
#' }
#'
#' @importFrom dplyr mutate slice select
#' @importFrom lubridate dmy
#'
trouver_l_elus_le_plus_age <- function(data) {
 if (!all(c("Nom.de.l.élu", "Prénom.de.l.élu", "Date.de.naissance") %in% colnames(data))) {
  stop("Les colonnes 'Nom.de.l.élu', 'Prénom.de.l.élu' et 'Date.de.naissance' doivent être présentes dans le dataframe.")
 }

 data |>
  dplyr::mutate(Date.de.naissance = lubridate::dmy(Date.de.naissance)) |>
  dplyr::slice(which.min(Date.de.naissance)) |>
  dplyr::select(Nom.de.l.élu, Prénom.de.l.élu, Date.de.naissance)
}
