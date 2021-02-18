#' Parser l'export csv des synthèses de la banque HYDRO
#'
#' L'export est un empilement non structuré de tableaux. La fonction permet d'interpréter
#'     les données par station et de constituer pour chacune un tableau par thème (crues,
#'     étiages, etc.).
#'
#' @param fichier Texte. Chemin vers le fichier d'export au format csv.
#'
#' @return Une liste de listes structurée. Elle contient autant d'éléments qu'il y a de
#'     stations dans les données. Pour chaque station, la liste comprend 8 tableaux
#'     correspondant aux débits mensuels, crues, modules, étiages, etc.
#' @export
#'
#' @examples
#' \dontrun{
#' syntheses <- bh_sy_parser(fichier = "raw_data/5590_4_synthese.csv")
#' }
bh_sy_parser <- function(fichier) {

  # Lecture du fichier texte d'export des syntheses
  syntheses <- readLines(con = fichier)

  # Création d'une liste contenant un élément par station et nommé d'après le code station
  syntheses_liste <- scinder_syntheses(syntheses = syntheses) %>%
    nommer_liste()

  # Création d'un vecteur contenant les identifiants des stations
  identifiants <- names(syntheses_liste)

  sorties <- parser_nsta(sta_ids = identifiants,
                         liste = syntheses_liste)

  names(sorties) <- identifiants

  sorties

}

