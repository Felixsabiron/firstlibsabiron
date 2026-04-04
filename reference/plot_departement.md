# Visualiser la repartition des elus par code professionnel dans un departement

La fonction verifie que l'objet fourni est bien de classe
`"departement"`, valide sa structure avec
[`validate_schema()`](https://felixsabiron.github.io/firstlibsabiron/reference/validate_schema.md),
puis produit un graphique en barres horizontales representant la
repartition des elus par code professionnel. Le graphique affiche les
**10 codes professionnels les plus representes** dans le departement. Le
titre du graphique inclut le nom du departement et le nombre total de
communes qu'il contient.

## Usage

``` r
plot_departement(x, ...)
```

## Arguments

- x:

  Un objet de classe "departement".

- ...:

  Arguments supplementaires passes a
  [`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html).

## Value

Un graphique `ggplot2` representant la repartition des elus par code
professionnel dans le departement.

## Details

Cette fonction genere un graphique en barres representant le nombre
d'elus par code professionnel pour un departement donne.

- La fonction utilise `validate_schema(x)` pour s'assurer que la
  structure des donnees est correcte.

- Elle verifie que `x` est bien un objet de classe `"departement"`,
  sinon elle renvoie une erreur.

- Elle compte le nombre total de communes distinctes presentes dans le
  departement.

- Elle filtre et selectionne les **10 codes professionnels les plus
  representes**.

- Un graphique en barres est genere, trie en ordre decroissant et
  affichant les valeurs sur les barres.

## Examples

``` r
if (FALSE) { # \dontrun{
library(ggplot2)
library(dplyr)

# Exemple de dataframe representant un departement
data <- data.frame(
  Libelle.du.departement = rep("Paris", 15),
  Libelle.de.la.commune = c(rep("Paris", 10), rep("Boulogne-Billancourt", 5)),
  Code.de.la.categorie.socio.professionnelle = sample(LETTERS[1:5], 15, replace = TRUE)
)

# Ajouter la classe "departement"
class(data) <- c("departement", class(data))

# Generer le graphique
plot_departement(data)
} # }
```
