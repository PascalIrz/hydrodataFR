#' Produire le graphique, pour une station, du débit en fonction du temps en représentant
#'     QMNA5, module et normale mansuelle
#'
#' @param debit_sh Dataframe contenant les données de débit temps réel pour la station,
#'     produit par la fonction bh_htr().
#' @param synthese_sh Dataframe contenant les données de débit synthèse pour la station,
#'     produit par la fonction bh_sy_ind_sta().
#' @param donnees_sh Dataframe contenant les catactéristiques de la station,
#'     produit par la fonction bh_sta_data()
#' @param coul_em Caractère. Code de la couleur associée aux écoulements mensuels.
#' @param coul_QMNA5 Caractère. Code de la couleur associée au QMNA5.
#' @param coul_module Caractère. Code de la couleur associée au module.
#' @param coul_actuel Caractère. Code de la couleur associée aux débits temps réel eu aux
#'     données les plus récentes.
#'
#' @return Un graphique de classe ggplot2.
#' @export
#'
#' @importFrom purrr set_names
#' @importFrom tibble rownames_to_column
#' @importFrom dplyr filter select mutate left_join pull
#' @importFrom tidyr pivot_longer
#' @importFrom ggplot2 ggplot aes geom_col geom_line scale_x_datetime scale_y_continuous
#' @importFrom ggplot2 labs geom_hline scale_colour_manual scale_linetype_manual
#' @importFrom lubridate month
#'
#' @examples
#' \dontrun{
#' mon_graphique <- bh_htr_grapher(debit_sh = debit_tr,
#' synthese_sh = synthese_ma_sh,
#' donnees_sh = sh_data)
#' }
bh_htr_grapher <- function(debit_sh, synthese_sh, donnees_sh,
                           coul_em = "#54b5c5", coul_QMNA5 = "red",
                           coul_module = "brown", coul_actuel = "darkgreen")

{

  em_long <- synthese_sh %>%
    .[["em"]] %>%
    purrr::set_names(1:12) %>%
    rownames_to_column(var = "variable") %>%
    pivot_longer(cols = -variable, names_to = "mois", values_to = "em") %>%
    filter(variable == "Debits (m3/s)") %>%
    select(-variable)

  # Ajout des em au tableau des débits temps réel
  data <- debit_sh %>%
    mutate(mois = month(date_obs) %>% as.character) %>%
    left_join(y = em_long)

  # Tableau qui va servir à indiquer module et QMNA5
  hline_data <- data.frame(
    y = c(synthese_sh$mm$`Module (moyenne) est`,
          synthese_sh$be %>% .["Quinquennale seche",] %>% pull(`QMNA (m3/s) est`)),
    type = c("dashed", "solid"),
    couleur = c(coul_module, coul_QMNA5)
  )

  libelle_ma_sh <- donnees_sh %>%
    filter(code_site == ma_sh) %>%
    pull(libelle_site)

  # Constitution du graphique
  g_htr <- ggplot(data = data,
                  aes(x = date_obs, y = debit)) +
    geom_col(aes(y = em), stat = "identity", width = 20000, alpha = 0.050, fill = coul_em) +
    geom_line(col = coul_actuel) +
    scale_x_datetime(date_labels = "%d/%m") + # étiquette axe des dates
    scale_y_continuous(limits = c(0, NA)) +
    labs(x = "", title = libelle_ma_sh) + # légendes
    geom_hline(data = hline_data, aes(yintercept = y, col = couleur, linetype = type)) +
    scale_colour_manual(values = c(coul_module, coul_QMNA5),
                        labels = c("Module", "QMNA5"),
                        name = "Statistique") +
    scale_linetype_manual(values = c("solid", "dashed"),
                          labels = c("Module", "QMNA5"),
                          name = "Statistique")

  # Affichage du graphique
  g_htr
}
