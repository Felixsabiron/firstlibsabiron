#' Generer un rapport personnalise
#'
#' Cette fonction genere un rapport Quarto au format PDF incluant :
#' - Une visualisation des elus par code professionnel pour la commune et le departement.
#' - Un resume des informations pour la commune et le departement.
#'
#' @param commune La commune pour laquelle generer le rapport (code INSEE).
#' @param departement Le departement concerne (code numerique).
#' @param output Le nom du fichier de sortie (sans chemin).
#'
#' @return Le chemin du fichier genere.
#' @import ggplot2
#' @export
generer_rapport <- function(commune, departement, output) {

 # Localisation du fichier rapport.qmd dans inst
 # Utilisation de system.file() pour trouver le fichier après installation
 qmd_file <- system.file("rapport.qmd", package = "firstlibsabiron")

 # Si le package n'est pas installé (en développement), utiliser le chemin local
 if (qmd_file == "") {
  qmd_file <- file.path("inst", "rapport.qmd")
 }

 # Vérification de l'existence du fichier
 if (!file.exists(qmd_file)) {
  stop("Le fichier rapport.qmd n'a pas été trouvé.")
 }

 # Dossier de sortie
 output_dir <- "output"

 # Vérifier que le dossier de sortie existe, sinon le créer
 if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
 }

 # Nom du fichier sans le chemin
 output_filename <- basename(output)

 # Compilation du rapport avec Quarto
 quarto::quarto_render(
  input = qmd_file,
  output_file = output_filename, # Seulement le nom du fichier !
  execute_params = list(
   code_commune = commune,
   code_departement = departement
  )
 )

 # Chercher le fichier généré dans plusieurs dossiers potentiels
 possible_dirs <- c("..", "../..", ".", "inst")
 generated_file <- NULL

 for (dir in possible_dirs) {
  found_files <- list.files(path = dir, pattern = output_filename, full.names = TRUE, recursive = TRUE)
  if (length(found_files) > 0) {
   generated_file <- found_files[1]
   break
  }
 }

 # Si le fichier n'a toujours pas été trouvé, lever une erreur
 if (is.null(generated_file) || !file.exists(generated_file)) {
  stop("Le fichier généré n'a pas été trouvé.")
 }

 # Déplacement du fichier généré dans le dossier "output"
 destination_file <- file.path(output_dir, output_filename)
 file.rename(generated_file, destination_file)

 # Message de confirmation
 message("Rapport généré avec succès : ", destination_file)
 return(destination_file)
}



