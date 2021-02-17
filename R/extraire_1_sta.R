#' Extraire les données d'une seule station en texte à partir de l'export.
#'
#' @param syntheses Le fichier csv d'export de la banque Hydro
#' @param indexes_debut Numérique. Vecteur indiquant les indexes de début de
#'     chaque station.
#' @param indexes_fin Numérique. Vecteur indiquant les indexes de fin de
#'     chaque station.
#' @param i Numérique. Numéro de la station (depuis 1 pour la première)
#' @param nom_liste Nom de la liste de sortie.
#'
#' @return Une liste.
#' @export
#'
#' @examples
#' \dontrun{
#' data_sta <- extraire_1_sta(syntheses = syntheses,
#                 indexes_debut = indexes_debut,
#                 indexes_fin = indexes_fin,
#                 i = 1)
#' }

extraire_1_sta <-
  function(syntheses,
           indexes_debut,
           indexes_fin,
           i,
           nom_liste = "liste") {
    sta <- list()

    sta_data <- syntheses[indexes_debut[i]:indexes_fin[i]]

    sta[[1]] <- sta_data

    sta_id <- sta_data[[2]] %>%
      strsplit(";") %>%
      .[[1]] %>%
      .[[1]]

    sta[[2]] <- sta_id

    sta

  }
