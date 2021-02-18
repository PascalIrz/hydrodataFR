#' Parser n stations à partir des données texte.
#'
#' @param liste Liste contenant un autant d'objets qu'il y a de stations.
#' @param sta_ids Caractère. Vacteur des identifiants des stations.
#'
#' @return Une liste de liste structurée.
#' @noRd
#' @importFrom purrr map
#'
#' @examples
#' \dontrun{
#' dfs_sta <- parser_nsta(liste = liste,
#' sta_ids = c("J0621610", "J3403010"))
#' }
parser_nsta <- function(liste, sta_ids)

{
  sorties <- map(.x = sta_ids,
                 .f = parser_1sta,
                 liste = liste)


}



