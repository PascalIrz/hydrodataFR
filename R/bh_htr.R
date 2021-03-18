#' Extension de la fonction menv_bh_htr_tod()
#'
#' Permet de télécharger plus de données à la fois, mais admet moins d'arguments.
#'
#' @param stations_id Vecteur texte. Identifiants des stations.
#' @param grandeur_hydro Texte. Grandeur hydrométrique observée pour la série. Prend la valeur
#'     "Q" pour le débit et "H" pour la hauteur d'eau.
#' @param timestep Pas de temps entre mesures consécutives.
#'
#' @return Le dataframe contenant les données.
#' @export
#'
#' @importFrom purrr map reduce
#'
#' @examples
#' \dontrun{
#' mes_stations <- c("J7010610", "J4125710")
#' debits_plusieurs_stations <- bh_htr(stations_id = mes_stations)
#' }
bh_htr <-
  function(stations_id,
           grandeur_hydro = "Q",
           timestep = 60) {
    map(
      .x = stations_id,
      bbox = NULL,
      .f = bh_htr_base,
      grandeur_hydro = grandeur_hydro,
      timestep = timestep
    ) %>%
      reduce(rbind)


  }

