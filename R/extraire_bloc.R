#' Extraire l'un des blocs (tableaux de données) d'une des stations
#'
#' @param synthese Synthèse pour une station, au format texte.
#' @param pattern_debut Chaîne de caractère qui ve permettre d'identifier le début
#'     du tableau.
#' @param nb_lignes Numérique. Nombre de lignes du tableau (dont les noms des colonnes).
#'
#' @return Un dataframe contenant les données.
#' @export
#'
#' @importFrom stringi stri_detect_fixed
#' @importClassesFrom stringr str_split
#'
#' @examples
#' \dontrun{
#' liste <- scinder_syntheses(syntheses = readLines("raw_data/export.csv"))
#' }
extraire_bloc <- function(synthese, pattern_debut, nb_lignes) {

  ligne_debut <- synthese %>%
    stringi::stri_detect_fixed(pattern = pattern_debut) %>%
    which()

  data <- synthese[(1 + ligne_debut):(nb_lignes + ligne_debut)] %>%
    str_split(pattern = ";") %>%
    as.data.frame() %>%
    t() %>%
    as.data.frame()

}
