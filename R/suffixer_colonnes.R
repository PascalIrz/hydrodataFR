#' Ajouter des suffixes aux noms de colonnes (cas des intervalles de confiance)
#'
#' @param noms_colonnes Vecteur contenanc les noms des colonnes
#' @param suffixes Vecteur contenant les suffixes (par défaut "est", "min", "max").
#'
#' @return Un nouveau vecteur contenant des noms de colonnes. Par défaut il est 3
#'     fois plus long quye le premier.
#' @export
#'
#' @importFrom purrr map
#'
#' @examples
#' \dontrun{
#' new_names <- suffixer_colonnes(old_names)
#' }
suffixer_colonnes <-
  function(noms_colonnes,
           suffixes = c("est", "min", "max"))

  {
    map(.x = noms_colonnes,
        .f = paste,
        suffixes) %>%
      unlist()


  }
