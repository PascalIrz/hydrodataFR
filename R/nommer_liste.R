#' Nommage des synthèses par les codes stations
#'
#' @param liste Liste issue de la fonction scinder_syntheses()
#'
#' @return La même liste mais dont les objets sont nommés d'après les codes stations
#' @export
#'
#' @importFrom purrr map
#'
#' @examples
#' \dontrun{
#' liste <- nommer_liste(liste)
#' }
nommer_liste <- function(liste) {

  noms_liste <- map(.x = liste,
                    .f = function(synthese) synthese[[2]]) %>%
    unlist()

  names(liste) <- noms_liste

  map(.x = liste,
      .f = function(synthese) synthese[1])

}
