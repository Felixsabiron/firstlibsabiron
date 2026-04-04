# Trouver l'elu(e) le plus age(e)

Cette fonction identifie l'elu(e) le plus age(e) dans un dataframe
contenant des informations sur les elus.

## Usage

``` r
trouver_l_elus_le_plus_age(data)
```

## Arguments

- data:

  Un dataframe contenant au moins les colonnes suivantes :

  - `Nom.de.l.elu` (character) : Le nom de l'elu(e).

  - `Prenom.de.l.elu` (character) : Le prenom de l'elu(e).

  - `Date.de.naissance` (character) : La date de naissance sous format
    `jour/mois/annee` (ex: "12/05/1950").

## Value

Un dataframe avec les informations de l'elu(e) le plus age(e), contenant
les colonnes :

- `Nom.de.l.elu`

- `Prenom.de.l.elu`

- `Date.de.naissance` (au format Date).

## Details

- La fonction verifie la presence des colonnes requises.

- Elle convertit la colonne `Date.de.naissance` en format Date avec
  [`lubridate::dmy()`](https://lubridate.tidyverse.org/reference/ymd.html).

- Elle selectionne l'elu(e) avec la date de naissance la plus ancienne.

## Examples

``` r
if (FALSE) { # \dontrun{
library(dplyr)
library(lubridate)

# Exemple de dataframe
data <- data.frame(
  Nom.de.l.elu = c("Durand", "Martin", "Dupont"),
  Prenom.de.l.elu = c("Jean", "Sophie", "Paul"),
  Date.de.naissance = c("12/05/1950", "23/09/1965", "07/11/1948")
)

# Trouver l'elu le plus age
trouver_l_elus_le_plus_age(data)
} # }
```
