#' Parser une station à partir des données texte.
#'
#' @param liste Liste contenant un autant d'objets qu'il y a de stations.
#' @param sta_id Caractère. Identifiant de la station, par exemple "J0621610".
#'
#' @return Une liste avec un dataframe par tableau de l'export.
#' @noRd
#'
#' @importFrom dplyr pull
#'
#' @examples
#' \dontrun{
#' dfs_sta <- parser_1sta(liste = liste,
#' sta_id = "J0621610")
#' }
parser_1sta <- function(liste, sta_id) {
  ma_synthese <- liste[sta_id] %>%
    unlist()

  # -------------------------------------------------------------
  # ecoulements mensuels
  # -------------------------------------------------------------
  nb_lignes <- 4

  em <- extraire_bloc(synthese = ma_synthese,
                      pattern_debut = "Ecoulements mensuels",
                      nb_lignes = nb_lignes)

  noms_colonnes <- em[1, ]

  noms_lignes <- em %>%
    pull(V1) %>%
    .[. != ""]

  em <- em %>%
    mef_df(noms_colonnes = noms_colonnes,
           noms_lignes = noms_lignes)

  # -------------------------------------------------------------
  # modules interannuels
  # -------------------------------------------------------------
  nb_lignes <- 2
  mi <- extraire_bloc(synthese = ma_synthese,
                      pattern_debut = "Modules interannuels",
                      nb_lignes = nb_lignes)

  noms_colonnes <- mi[1, ]

  noms_lignes <- mi %>%
    pull(V1) %>%
    .[. != ""]

  mi <- mi %>%
    mef_df(
      noms_colonnes = noms_colonnes,
      noms_lignes = noms_lignes,
      suffixer = TRUE
    )

  # données module (moyenne)
  mm <- extraire_bloc(synthese = ma_synthese,
                      pattern_debut = "Module (moyenne)",
                      nb_lignes = 0)

  # bidouillage noms col pour que le nb de noms corresponde au nb de colonnes
  noms_colonnes <- mm[2, 1] %>%
    paste(c("est", "min", "max"))

  noms_colonnes <- c("V1", noms_colonnes, "V2")

  noms_lignes <- "Débits (m3/s)"

  mm <- mm %>%
    mef_df(
      noms_colonnes = noms_colonnes,
      noms_lignes = noms_lignes,
      suffixer = FALSE
    )

  # -------------------------------------------------------------
  # Basses eaux
  # -------------------------------------------------------------
  # Comme les 2 premières ligens ne contiennent pas le même nb d'éléments que
  # les suivantes, besoin de procéder en deux temps

  ### Lignes du haut
  nb_lignes <- 3
  be <- extraire_bloc(synthese = ma_synthese,
                      pattern_debut = "Basses eaux",
                      nb_lignes = nb_lignes)

  noms_colonnes <- be[1, ]

  noms_lignes <- be %>%
    pull(V1) %>%
    .[. != ""]

  be <- be %>%
    mef_df(
      noms_colonnes = noms_colonnes,
      noms_lignes = noms_lignes,
      suffixer = TRUE
    )

  ### Lignes du bas
  nb_lignes <- 5
  be2 <- extraire_bloc(synthese = ma_synthese,
                       pattern_debut = "Basses eaux",
                       nb_lignes = nb_lignes) %>%
    .[c(1, 4, 5), ]

  noms_colonnes <-  be2[1, ]

  noms_lignes <- be2 %>%
    pull(V1) %>%
    .[. != ""]

  be2 <- be2 %>%
    mef_df(
      noms_colonnes = noms_colonnes,
      noms_lignes = noms_lignes,
      suffixer = FALSE
    )
  # -------------------------------------------------------------
  # Crues
  # -------------------------------------------------------------
  # Comme les 2 premières ligens ne contiennent pas le même nb d'éléments que
  # les suivantes, besoin de procéder en deux temps

  ### Lignes du haut
  nb_lignes <- 3
  cr <- extraire_bloc(synthese = ma_synthese,
                      pattern_debut = "Crues",
                      nb_lignes = nb_lignes)

  noms_colonnes <- cr[1, ]

  noms_lignes <- cr %>%
    pull(V1) %>%
    .[. != ""]

  cr <- cr %>%
    mef_df(
      noms_colonnes = noms_colonnes,
      noms_lignes = noms_lignes,
      suffixer = FALSE
    )


  ### Lignes du bas
  # On retient 8 lignes car la centennale n'est jamais remplie
  nb_lignes <- 8
  cr2 <- extraire_bloc(synthese = ma_synthese,
                       pattern_debut = "Crues",
                       nb_lignes = nb_lignes) %>%
    .[c(1, 4:8), ]

  noms_colonnes <- cr2[1, ]

  noms_lignes <- cr2 %>%
    pull(V1) %>%
    .[. != ""] %>%
    .[. != "NA"]

  cr2 <- cr2 %>%
    mef_df(
      noms_colonnes = noms_colonnes,
      noms_lignes = noms_lignes,
      suffixer = TRUE
    )

  # -------------------------------------------------------------
  # Maximums connus
  # -------------------------------------------------------------
  nb_lignes <- 2
  dc <- extraire_bloc(synthese = ma_synthese,
                      pattern_debut = "Débits classés",
                      nb_lignes = nb_lignes)

  noms_colonnes <- paste0("f_", dc[1, ])

  noms_lignes <- 'Débit (m3/s)'

  dc <- dc %>%
    mef_df(
      noms_colonnes = noms_colonnes,
      noms_lignes = noms_lignes,
      suffixer = FALSE
    )

  # -------------------------------------------------------------
  # assemblage par station
  # -------------------------------------------------------------
  sortie <- list(em, mi, mm, be, be2, cr, cr2, dc)

  names(sortie) <- c("em", "mi", "mm", "be", "be2", "cr", "cr2", "dc")

  sortie

}
