#' Scinder les synthèses par station
#' @param syntheses Objet texte simple (lecture du csv d'export).
#'
#' @return Une liste avec ubn objet par station.
#' @export
#'
#' @importFrom stringi stri_detect_fixed
#' @importClassesFrom purrr map
#'
#' @examples
#' \dontrun{
#' liste <- scinder_syntheses(syntheses = readLines("raw_data/export.csv"))
#' }
scinder_syntheses <- function(syntheses) {
  indexes_debut <- syntheses %>%
    stringi::stri_detect_fixed(pattern = "Code station") %>%
    which()

  nb_stations <- length(indexes_debut)

  indexes_fin <- (indexes_debut[2:nb_stations] - 1) %>%
    c(length(syntheses))

  map(
    .x = 1:nb_stations,
    .f = extraire_1_sta,
    syntheses = syntheses,
    indexes_debut = indexes_debut,
    indexes_fin = indexes_fin
  )

}
