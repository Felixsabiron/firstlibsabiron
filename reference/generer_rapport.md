# Generer un rapport personnalise

Cette fonction genere un rapport Quarto au format PDF ou HTML incluant :

- Une visualisation des elus par code professionnel pour la commune et
  le departement.

- Un resume des informations pour la commune et le departement.

## Usage

``` r
generer_rapport(commune, departement, output)
```

## Arguments

- commune:

  La commune pour laquelle generer le rapport (code INSEE).

- departement:

  Le departement concerne (code numerique).

- output:

  Le nom du fichier de sortie (ex: "rapport.pdf" ou "rapport.html").

## Value

Le chemin du fichier genere.
