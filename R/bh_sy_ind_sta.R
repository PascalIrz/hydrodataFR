#' Extraire un type d'indicateur statistique de la liste des synthèses
#'
#' @param syntheses Liste des synthèses par station.
#' @param sta_id Caractère. Code station.
#' @param indicateur Caractère. Type d'indicateur statistique à extraire.
#'
#' @return Un dataframe avec la statistique pour la station. Par défaut ce sont les
#'     écoulements mensuels.
#' @export
#'
#' @examples
#' \dontrun{
#' bh_sy_ind_sta(syntheses, sta_id = "J1813010", stat = "em")
#' }
bh_sy_ind_sta <- function(syntheses,
                          sta_id,
                          indicateur = "em")

{
  synthese <- syntheses[sta_id] %>%
    .[[1]] %>%
    .[[indicateur]]

  return(synthese)

}
