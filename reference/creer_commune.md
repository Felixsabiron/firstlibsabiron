# Creer un objet de type "commune"

La fonction s'assure que le dataframe contient une seule commune
distincte, et lui attribue la classe `"commune"` s'il ne l'a pas deja.

## Usage

``` r
creer_commune(df)
```

## Arguments

- df:

  Un dataframe contenant au moins la colonne suivante :

  - `Code.de.la.commune` (character ou numeric) : Le code identifiant la
    commune.

## Value

Un dataframe de classe `"commune"`, pret a etre utilise dans d'autres
fonctions specifiques aux communes.

## Details

Cette fonction transforme un dataframe representant une seule commune en
un objet de classe "commune".

- La fonction verifie que la colonne `Code.de.la.commune` est presente
  dans `df`.

- Elle s'assure qu'il n'y a **qu'une seule commune distincte**, sinon
  elle genere une erreur.

- Si la classe `"commune"` n'est pas deja presente, elle est ajoutee a
  l'objet.

## Examples

``` r
if (FALSE) { # \dontrun{
# Exemple de dataframe valide avec une seule commune
data <- data.frame(
  Code.de.la.commune = rep("75056", 5),  # Paris
  Nom.de.la.commune = rep("Paris", 5),
  Population = c(100, 200, 150, 180, 220)
)

# Creer l'objet commune
commune <- creer_commune(data)
class(commune)  # Verifier la classe
} # }
```
