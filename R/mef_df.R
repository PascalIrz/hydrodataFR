#' Mettre en forme un des dataframes thématiques d'une station.
#'
#' @param df Le dataframe contenant les donnéers
#' @param noms_colonnes Noms des colonnes.
#' @param noms_lignes Vecteur contenant le nom des fignes
#' @param suffixer TRUE/FALSE. Indique si les noms des colonnes doivent être suffixés
#'     (cas où les intervalles de confiance sont indiqués => suffixes est, min et max).
#'
#' @return Le dataframe mis en forme.
#' @noRd
#'
#' @importFrom dplyr slice mutate_all n select starts_with
#' @importFrom purrr set_names discard
#' @importFrom magrittr set_rownames
#'
#' @examples
#' \dontrun{
#' df <- mef_df(df = mon_df, noms_colonnes = mes_noms_c,
#' noms_lignes = mes_noms_l, suffixer = TRUE)
#' }
mef_df <- function(df, noms_colonnes, noms_lignes, suffixer = FALSE)

{
  df <- df %>%
    slice(2:n()) %>%
    mutate_all(as.numeric) %>%
    purrr::set_names(noms_colonnes) %>%
    purrr::discard( ~ all(is.na(.)))

  if(length(df) == 0)  {df <- NA  # si tableau vide

  }else{

    nb_cols <- length(df)

    if(suffixer == TRUE)

    {

      noms_colonnes_candidats <- noms_colonnes %>%
        .[.!=''] %>%
        suffixer_colonnes()

      df <- df %>%
        purrr::set_names(noms_colonnes_candidats[1:length(df)])

    }

 #   if('Année' %in% names(df))
    if(stringr::str_detect(names(df), "^Ann") %>% sum() > 0)

    {

     # df <- df %>% select(-`Année`)
      df <- df %>% select(-(starts_with("Ann")))

    }

    df <- df %>%
      magrittr::set_rownames(noms_lignes)

  }

  df

}
