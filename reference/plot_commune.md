# Visualiser la repartition des elus par code professionnel dans une commune

La fonction verifie que l'objet fourni est bien de classe `"commune"`,
valide sa structure avec
[`validate_schema()`](https://felixsabiron.github.io/firstlibsabiron/reference/validate_schema.md),
puis produit un graphique en barres horizontales representant la
repartition des elus par code professionnel. Le titre du graphique est
compose du nom de la commune et de son departement.

## Usage

``` r
plot_commune(x, ...)
```

## Arguments

- x:

  Un objet de classe "commune".

- ...:

  Arguments supplementaires passes a
  [`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html).

## Value

Un graphique `ggplot2` representant la repartition des elus par code
professionnel dans la commune.

## Details

Cette fonction genere un graphique en barres representant le nombre
d'elus par code professionnel pour une commune donnee.

- La fonction utilise `validate_schema(x)` pour s'assurer que la
  structure des donnees est correcte.

- Elle verifie que `x` est bien un objet de classe `"commune"`, sinon
  elle renvoie une erreur.

- Elle compte le nombre d'elus par code professionnel et filtre ceux
  ayant `n = 0`.

- Un graphique en barres est genere, trie en ordre decroissant et
  affichant les valeurs sur les barres.

## Examples

``` r
if (FALSE) { # \dontrun{
library(ggplot2)
library(dplyr)

# Exemple de dataframe representant une commune
data <- data.frame(
  Libelle.de.la.commune = rep("Paris", 6),
  Libelle.du.departement = rep("Paris", 6),
  Code.de.la.categorie.socio.professionnelle = c("A1", "B2", "A1", "C3", "B2", "A1")
)

# Ajouter la classe "commune"
class(data) <- c("commune", class(data))

# Generer le graphique
plot_commune(data)
} # }
```
