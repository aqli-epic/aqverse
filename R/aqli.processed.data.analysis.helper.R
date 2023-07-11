##> AQLI processed data helper functions==================================================

#' Collapse AQLIs most granular processed data to less granular levels
#'
#' AQLI (in 2021) reports annual average PM2.5 and life years lost at 3 levels:
#' gadm0, 1, 2 (\href{https://gadm.org/}{gadm}). These levels define the administrative region boundaries of a
#' particular "country". gadm0 corresponds to a country. gadm1 corresponds to
#' "state" and gadm2 corresponds to "district/prefecture/county" etc. This function
#' takes in a gadm2 dataset and has the functionality to collapsing it gadm1 and
#' gadm0 (and gadm2, which is equivalent to the original data).
#' Furthermore, one can specify the \code{years} to specify the years for which
#' the collapsed data is needed and \code{perc_red_by} (percentage reduction by) to specify if
#' additional columns showing pollution levels and life years lost after a custom reduction are to be also c
#' computed
#'
#'
#' @importFrom stringr str_c
#' @import dplyr
#'
#'
#'
#' @param df AQLI processed gadm level 2 dataset, corresponding to the data-dictionary at this link (to be added).
#'
#' @param level_col_name_vec a vector specifying the highest resolution at which the \code{df} is to be collapsed. This
#'                           vector is to be specified starting from lowest to highest resolution (for a non-continent summary).
#'                           The four available resolutions are: \code{continent}, \code{country}, \code{name_1}, \code{name_2}.
#'                           For a \code{continent} level summary, the vector would look like: c("continent"). But, for all
#'                           "non-continent summary" the vector would be specified from lowest to highest resolution. For example -
#'                           for a state (\code{gadm1}) level summary, the vector would look like: c("country", "name_1"). See examples
#'                           below for more use cases.
#'
#' @param years  a vector of years for which the summary data is needed, e.g. \code{c(1998, 2000, 2021)}.
#'               The 2023 AQLI dataset provides data on the following years 1998 to 2021.
#'
#' @param perc_red_by custom reduction percentage (numeric), i.e. if columns for custom reduction are needed. For example, if we want to see
#'                    pollution (and corresponding lyl) columns for a 10% reduction relative to the years specified, then this
#'                    parameter would be 10.
#'
#'
#' @examples
#' gadm_level_summary(gadm2_aqli_2021, c("country"), c(2021), 10) # country (gadm0) level summary for the year 2021, with 10% custom reduction column
#' gadm_level_summary(gadm2_aqli_2021, c("country", "name_1"), c(2021), 10) # state (gadm1) level summary for the year 2021, with 10% custom reduction column
#' gadm_level_summary(gadm2_aqli_2021, c("continent"), c(2021), 10) # continent level summary for the year 2021, with 10% custom reduction column
#' gadm_level_summary(gadm2_aqli_2021, c("country", "name_1", "name_2"), c(2021), 10) # district (gadm0) level summary for the year 2021, with 10% custom reduction column
#'
#'
#' @return returns a collapsed dataset with
#'
#'
#'
#' @export

gadm_level_summary <- function(df, level_col_name_vec, years, perc_red_by){

  le_constant <- 0.098
  pol_col_names <- stringr::str_c("pm", years)
  pol_col_names_red_to <- stringr::str_c("pm", years, "_reduced_to")
  llpp_who_col_names <- stringr::str_c("llpp_who_", years)
  llpp_nat_col_names <- stringr::str_c("llpp_nat_", years)
  llpp_pol_red_names <- stringr::str_c("llpp_pol_red_", years)
  total_lyl_who_columns <- stringr::str_c("total_lyl_who_", years, "_millions")
  total_lyl_nat_columns <- stringr::str_c("total_lyl_nat_", years, "_millions")
  total_lyl_pol_red_to_columns <- stringr::str_c("total_lyl_pol_red_", years, "_millions")

  # when level  just equal to continent, deal separately, as in this case natstandard column should not be included
  if((level_col_name_vec[1] == "continent") & (length(level_col_name_vec) == 1)){
    aqli_wide <- df %>%
      dplyr::group_by_at(level_col_name_vec) %>%
      dplyr::mutate(pop_weights = population/sum(population, na.rm = TRUE)) %>%
      dplyr::mutate(across(dplyr::starts_with("pm"), ~(.x*pop_weights), .names = "{col}_pop_weighted")) %>%
      dplyr::summarise(across(dplyr::contains("pop_weighted"), ~(round(sum(.x, na.rm = TRUE), 1)), .names = "avg_{col}"),
                       total_population = sum(population, na.rm = TRUE), objectid_gadm2 = objectid_gadm2[1], iso_alpha3 = iso_alpha3[1], whostandard = whostandard[1], .groups = "keep") %>%
      select(objectid_gadm2, iso_alpha3, all_of(level_col_name_vec), total_population, whostandard, dplyr::everything()) %>%
      dplyr::mutate(objectid_gadm2 = dplyr::row_number()) %>%
      dplyr::rename(population = total_population,
                    objectid_level = objectid_gadm2) %>%
      dplyr::rename_with(~str_replace_all(.x, "(_pop_weighted)|(avg_)", ""), dplyr::contains("_pop_weighted")) %>%
      dplyr::mutate(across(starts_with("pm"), (~round((.x - whostandard)*le_constant, 1)), .names = "llpp_who_{col}")) %>%
      dplyr::mutate(across(starts_with("pm"), (~round((.x*((perc_red_by)/100)), 1)*le_constant), .names = "llpp_pol_red_{col}")) %>%
      dplyr::mutate(across(starts_with("llpp"), ~ifelse(.x < 0, 0, .x))) %>%
      dplyr::select(objectid_level, iso_alpha3, all_of(level_col_name_vec), population, whostandard, dplyr::everything()) %>%
      dplyr::rename_with(~str_replace(.x, "pm", ""), dplyr::contains("llpp")) %>%
      dplyr::mutate(across(dplyr::matches("pm|llpp"), ~(round(.x, 1)), .names = "{col}")) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(across(dplyr::matches("llpp_who"), ~round((population*.x)/1000000, 1), .names = "total_{col}_millions")) %>%
      dplyr::mutate(across(dplyr::matches("llpp_pol_red"), ~round((population*.x)/1000000, 1), .names = "total_{col}_millions")) %>%
      dplyr::rename_with(~str_replace_all(.x, "llpp", "lyl"), dplyr::contains("total_llpp")) %>%
      dplyr::mutate(across(dplyr::matches("pm"), ~.x - (round((.x)*(perc_red_by/100), 1)), .names = "{col}_reduced_to")) %>%
      dplyr::select(objectid_level:whostandard, all_of(c(pol_col_names, pol_col_names_red_to, llpp_who_col_names, llpp_pol_red_names, total_lyl_who_columns, total_lyl_pol_red_to_columns))) %>%
      dplyr::mutate(across(matches("pm|llpp|total"), ~ifelse(population == 0, NA, .x))) %>%
      dplyr::mutate(population = ifelse(population == 0, NA, population))


    return(aqli_wide)

  } else if(("name_2" %in% level_col_name_vec)){
    if((which(level_col_name_vec == "name_2") > 2)){
      aqli_wide <- df %>%
        dplyr::mutate(across(starts_with("pm"), (~round((.x*((perc_red_by)/100)), 1)*le_constant), .names = "llpp_pol_red_{col}")) %>%
        dplyr::mutate(across(starts_with("llpp"), ~ifelse(.x < 0, 0, .x))) %>%
        dplyr::rename_with(~str_replace(.x, "pm", ""), dplyr::contains("llpp")) %>%
        dplyr::mutate(across(dplyr::matches("pm|llpp"), ~(round(.x, 1)), .names = "{col}")) %>%
        dplyr::mutate(across(dplyr::matches("llpp_who"), ~round((population*.x)/1000000, 1), .names = "total_{col}_millions")) %>%
        dplyr::mutate(across(dplyr::matches("llpp_nat"), ~round((population*.x)/1000000, 1), .names = "total_{col}_millions")) %>%
        dplyr::mutate(across(dplyr::matches("llpp_pol_red"), ~round((population*.x)/1000000, 1), .names = "total_{col}_millions")) %>%
        dplyr::rename_with(~str_replace_all(.x, "llpp", "lyl"), dplyr::contains("total_llpp")) %>%
        dplyr::mutate(across(dplyr::matches("pm"), ~.x - (round((.x)*(perc_red_by/100), 1)), .names = "{col}_reduced_to")) %>%
        dplyr::select(objectid_gadm2:natstandard, continent, all_of(c(pol_col_names, pol_col_names_red_to, llpp_who_col_names, llpp_nat_col_names, llpp_pol_red_names, total_lyl_who_columns,
                                                                      total_lyl_nat_columns, total_lyl_pol_red_to_columns))) %>%
        dplyr::mutate(across(matches("pm|llpp|total"), ~ifelse(population == 0, NA, .x))) %>%
        dplyr::mutate(population = ifelse(population == 0, NA, population))

    }

    return(aqli_wide)

  } else {
    aqli_wide <- df %>%
      dplyr::group_by_at(level_col_name_vec) %>%
      dplyr::mutate(pop_weights = population/sum(population, na.rm = TRUE)) %>%
      dplyr::mutate(across(dplyr::starts_with("pm"), ~(.x*pop_weights), .names = "{col}_pop_weighted")) %>%
      dplyr::summarise(across(dplyr::contains("pop_weighted"), ~(round(sum(.x, na.rm = TRUE), 1)), .names = "avg_{col}"),
                       total_population = sum(population, na.rm = TRUE), objectid_gadm2 = objectid_gadm2[1], iso_alpha3 = iso_alpha3[1], whostandard = whostandard[1], natstandard = natstandard[1], .groups = "keep") %>%
      select(objectid_gadm2, iso_alpha3, all_of(level_col_name_vec), total_population, whostandard, natstandard, dplyr::everything()) %>%
      dplyr::mutate(objectid_gadm2 = dplyr::row_number()) %>%
      dplyr::rename(population = total_population,
                    objectid_level = objectid_gadm2) %>%
      dplyr::rename_with(~str_replace_all(.x, "(_pop_weighted)|(avg_)", ""), dplyr::contains("_pop_weighted")) %>%
      dplyr::mutate(across(starts_with("pm"), (~round((.x - whostandard)*le_constant, 1)), .names = "llpp_who_{col}")) %>%
      dplyr::mutate(across(starts_with("pm"), (~round((.x*((perc_red_by)/100)), 1)*le_constant), .names = "llpp_pol_red_{col}")) %>%
      dplyr::mutate(across(starts_with("pm"), (~round((.x - natstandard)*le_constant, 1)), .names = "llpp_nat_{col}")) %>%
      dplyr::mutate(across(starts_with("llpp"), ~ifelse(.x < 0, 0, .x))) %>%
      dplyr::mutate(across(starts_with("llpp_nat"), ~ifelse(natstandard == 0, NA, .x))) %>%
      dplyr::select(objectid_level, iso_alpha3, all_of(level_col_name_vec), population, whostandard, natstandard, dplyr::everything()) %>%
      dplyr::rename_with(~str_replace(.x, "pm", ""), dplyr::contains("llpp")) %>%
      dplyr::mutate(across(dplyr::matches("pm|llpp"), ~(round(.x, 1)), .names = "{col}")) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(across(dplyr::matches("llpp_who"), ~round((population*.x)/1000000, 1), .names = "total_{col}_millions")) %>%
      dplyr::mutate(across(dplyr::matches("llpp_nat"), ~round((population*.x)/1000000, 1), .names = "total_{col}_millions")) %>%
      dplyr::mutate(across(dplyr::matches("llpp_pol_red"), ~round((population*.x)/1000000, 1), .names = "total_{col}_millions")) %>%
      dplyr::rename_with(~str_replace_all(.x, "llpp", "lyl"), dplyr::contains("total_llpp")) %>%
      dplyr::mutate(across(dplyr::matches("pm"), ~.x - (round((.x)*(perc_red_by/100), 1)), .names = "{col}_reduced_to")) %>%
      dplyr::select(objectid_level:natstandard, all_of(c(pol_col_names, pol_col_names_red_to, llpp_who_col_names, llpp_nat_col_names, llpp_pol_red_names, total_lyl_who_columns,
                                                         total_lyl_nat_columns, total_lyl_pol_red_to_columns))) %>%
      dplyr::mutate(across(matches("pm|llpp|total"), ~ifelse(population == 0, NA, .x))) %>%
      dplyr::mutate(population = ifelse(population == 0, NA, population))


    return(aqli_wide)

  }
}

#-----------------------------------------------------


#' Add AQLI base custom theme to plots
#'
#' Adds a custom AQLI theme to plots in a single command
#'
#' @import ggplot2
#' @importFrom ggthemes theme_tufte
#'
#' @param "" no parameters needed. See examples for usage.
#'
#' @examples
#' plot1 + themes_aqli_base
#' mtcars %>% ggplot2::ggplot() + ggplot2::geom_histogram(mapping = ggplot2::aes(mpg)) + themes_aqli_base
#'
#' @return when added to a plot, returns a plot with the AQLI plots theme.
#'
#'
#' @export


themes_aqli_base <- ggthemes::theme_tufte() +
  ggplot2::theme(plot.title = element_text(size = 18, hjust = 0.5, margin = margin(b = 0.2, unit = "cm")),
        plot.subtitle = element_text(size = 14, hjust = 0.5, margin = margin(b = 0.7, unit = "cm")),
        axis.title.x = element_text(size = 14, margin = margin(t = 0.3, b = 0.5, unit = "cm")),
        axis.title.y = element_text(size = 14, margin = margin(r = 0.3, unit = "cm")),
        axis.text = element_text(size = 13),
        plot.caption = element_text(size = 10, hjust = 0, margin = margin(t = 0.7, unit = "cm"), face = "italic"),
        legend.box.background = element_rect(color = "black"),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 13),
        legend.position = "bottom")


#-----------------------------------------------------


#' Add AQLI color scale buckets for all AQLI scales to a dataset
#'
#' Adds the buckets (mappping AQLI colors to the corresponding pollution or life years lost
#' value range buckets) as a column to the underlying dataset (depending on the underlying scale in use).
#' The dataset should have a column that will be used for mapping colors to values.
#' This function comes in handy while plotting graphs that need to be colored according to AQLI color scales.
#'
#' @importFrom dplyr mutate
#'
#' @param df The dataframe to which the AQLI color scales will be added as a character column
#' @param scale_type This is a parameter of type \code{character} and can take one of 4 values: (a) \code{lyl}: AQLI life years lost (single year);
#'                   (b) \code{pollution}: AQLI pollution (in µg/m³, single year); (c) \code{lyldiff}:
#'                   Life years lost scale (for difference between 2 years); (d) \code{poldiff}: pollution
#'                   difference between 2 years scale (in %).
#'
#'                   Note that, while plotting the actual map
#'                   that will use this function, make sure to add in the \code{fill} aesthetic of the mapping
#'                   function the following (defined here for each scale): \code{lyl} scale:
#'                    \code{forcats::fct_reorder(lyl_bucket, order_lyl_bucket)}; \code{pollution} scale:
#'                    \code{forcats::fct_reorder(pol_bucket, order_pol_bucket)}; \code{lyldiff} scale:
#'                    \code{forcats::fct_reorder(lyldiff_bucket, order_lyldiff_bucket)}; \code{poldiff} scale:
#'                    \code{forcats::fct_reorder(poldiff_bucket, order_poldiff_bucket)}
#'
#' @param col_name The column name (type:\code{character}) that will be used to map pollution/life years lost (whether
#'                 single year or difference between 2 years) values to the AQLI colors. Note, that
#'                 this column should be present in \code{df}.
#'
#' @examples
#' df %>% add_aqli_color_scale_buckets(scale_type = "pollution", col_name = "pm2021")
#' df %>% add_aqli_color_scale_buckets(scale_type = "lyl", col_name = "llpp_who_2021")
#' df %>% add_aqli_color_scale_buckets(scale_type = "poldiff", col_name = "pm_example_pol_diff_col")
#' df %>% add_aqli_color_scale_buckets(scale_type = "lyldiff", col_name = "llpp_example_lyl_diff_col")
#'
#' @return returns a data frame with an additional column (of class "character") that has the buckets
#'         corresponding to the \code{scale_type} specified.
#'
#'
#' @export
#'


add_aqli_color_scale_buckets <- function(df, scale_type = "pollution", col_name){

  # life years lost (single year) scale
  if(scale_type == "lyl"){
    df %>%
      dplyr::mutate(lyl_bucket = ifelse((!!as.symbol(col_name) >= 0) & (!!as.symbol(col_name) < 0.1), "0 to < 0.1", NA),
             lyl_bucket = ifelse((!!as.symbol(col_name) >= 0.1) & (!!as.symbol(col_name) < 0.5), "0.1 to < 0.5", lyl_bucket),
             lyl_bucket = ifelse((!!as.symbol(col_name) >= 0.5) & (!!as.symbol(col_name) < 1), "0.5 to < 1", lyl_bucket),
             lyl_bucket = ifelse((!!as.symbol(col_name) >= 1) & (!!as.symbol(col_name) < 2), "1 to < 2", lyl_bucket),
             lyl_bucket = ifelse((!!as.symbol(col_name) >= 2) & (!!as.symbol(col_name) < 3), "2 to < 3", lyl_bucket),
             lyl_bucket = ifelse((!!as.symbol(col_name) >= 3) & (!!as.symbol(col_name) < 4), "3 to < 4", lyl_bucket),
             lyl_bucket = ifelse((!!as.symbol(col_name) >= 4) & (!!as.symbol(col_name) < 5), "4 to < 5", lyl_bucket),
             lyl_bucket = ifelse((!!as.symbol(col_name) >= 5) & (!!as.symbol(col_name) < 6), "5 to < 6", lyl_bucket),
             lyl_bucket = ifelse((!!as.symbol(col_name) >= 6), ">= 6", lyl_bucket)) %>%
      dplyr::mutate(order_lyl_bucket = ifelse(lyl_bucket == "0 to < 0.1", 1, NA),
             order_lyl_bucket = ifelse(lyl_bucket == "0.1 to < 0.5", 2, order_lyl_bucket),
             order_lyl_bucket = ifelse(lyl_bucket == "0.5 to < 1", 3, order_lyl_bucket),
             order_lyl_bucket = ifelse(lyl_bucket == "1 to < 2", 4, order_lyl_bucket),
             order_lyl_bucket = ifelse(lyl_bucket == "2 to < 3", 5, order_lyl_bucket),
             order_lyl_bucket = ifelse(lyl_bucket == "3 to < 4", 6, order_lyl_bucket),
             order_lyl_bucket = ifelse(lyl_bucket == "4 to < 5", 7, order_lyl_bucket),
             order_lyl_bucket = ifelse(lyl_bucket == "5 to < 6", 8, order_lyl_bucket),
             order_lyl_bucket = ifelse(lyl_bucket == ">= 6", 9, order_lyl_bucket))

    # pollution (single year) scale
  } else if (scale_type == "pollution") {
    df %>%
      dplyr::mutate(pol_bucket = ifelse((!!as.symbol(col_name) >= 0) & (!!as.symbol(col_name) < 5), "0 to < 5", NA),
             pol_bucket = ifelse((!!as.symbol(col_name) >= 5) & (!!as.symbol(col_name) < 10), "5 to < 10", pol_bucket),
             pol_bucket = ifelse((!!as.symbol(col_name) >= 10) & (!!as.symbol(col_name) < 20), "10 to < 20", pol_bucket),
             pol_bucket = ifelse((!!as.symbol(col_name) >= 20) & (!!as.symbol(col_name) < 30), "20 to < 30", pol_bucket),
             pol_bucket = ifelse((!!as.symbol(col_name) >= 30) & (!!as.symbol(col_name) < 40), "30 to < 40", pol_bucket),
             pol_bucket = ifelse((!!as.symbol(col_name) >= 40) & (!!as.symbol(col_name) < 50), "40 to < 50", pol_bucket),
             pol_bucket = ifelse((!!as.symbol(col_name) >= 50) & (!!as.symbol(col_name) < 60), "50 to < 60", pol_bucket),
             pol_bucket = ifelse((!!as.symbol(col_name) >= 60) & (!!as.symbol(col_name) < 70), "60 to < 70", pol_bucket),
             pol_bucket = ifelse((!!as.symbol(col_name) >= 70), ">= 70", pol_bucket)) %>%
      dplyr::mutate(order_pol_bucket = ifelse(pol_bucket == "0 to < 5", 1, NA),
             order_pol_bucket = ifelse(pol_bucket == "5 to < 10", 2, order_pol_bucket),
             order_pol_bucket = ifelse(pol_bucket == "10 to < 20", 3, order_pol_bucket),
             order_pol_bucket = ifelse(pol_bucket == "20 to < 30", 4, order_pol_bucket),
             order_pol_bucket = ifelse(pol_bucket == "30 to < 40", 5, order_pol_bucket),
             order_pol_bucket = ifelse(pol_bucket == "40 to < 50", 6, order_pol_bucket),
             order_pol_bucket = ifelse(pol_bucket == "50 to < 60", 7, order_pol_bucket),
             order_pol_bucket = ifelse(pol_bucket == "60 to < 70", 8, order_pol_bucket),
             order_pol_bucket = ifelse(pol_bucket == ">= 70", 9, order_pol_bucket))

    # life years lost (difference between 2 years) scale
  } else if (scale_type == "lyldiff"){

    df %>%
      dplyr::mutate(lyldiff_bucket = ifelse((!!as.symbol(col_name) < -2), "< -2", NA),
             lyldiff_bucket = ifelse((!!as.symbol(col_name) >= -2) & (!!as.symbol(col_name) < -0.5), "-2 to (< -0.5)", lyldiff_bucket),
             lyldiff_bucket = ifelse((!!as.symbol(col_name) >= -0.5) & (!!as.symbol(col_name) < -0.1), "-0.5 to (< -0.1)", lyldiff_bucket),
             lyldiff_bucket = ifelse((!!as.symbol(col_name) >= -0.1) & (!!as.symbol(col_name) < 0), "-0.1 to (< 0)", lyldiff_bucket),
             lyldiff_bucket = ifelse((!!as.symbol(col_name) >= 0) & (!!as.symbol(col_name) < 0.1), "0 to (< 0.1)", lyldiff_bucket),
             lyldiff_bucket = ifelse((!!as.symbol(col_name) >= 0.1) & (!!as.symbol(col_name) < 0.5), "0.1 to (< 0.5)", lyldiff_bucket),
             lyldiff_bucket = ifelse((!!as.symbol(col_name) >= 0.5) & (!!as.symbol(col_name) < 2), "0.5 to (< 2)", lyldiff_bucket),
             lyldiff_bucket = ifelse((!!as.symbol(col_name) >= 2), ">= 2", lyldiff_bucket)) %>%
      dplyr::mutate(order_lyldiff_bucket = ifelse(lyldiff_bucket == "< -2", 1, NA),
             order_lyldiff_bucket = ifelse(lyldiff_bucket == "-2 to (< -0.5)", 2, order_lyldiff_bucket),
             order_lyldiff_bucket = ifelse(lyldiff_bucket == "-0.5 to (< -0.1)", 3, order_lyldiff_bucket),
             order_lyldiff_bucket = ifelse(lyldiff_bucket == "-0.1 to (< 0)", 4, order_lyldiff_bucket),
             order_lyldiff_bucket = ifelse(lyldiff_bucket == "0 to (< 0.1)", 5, order_lyldiff_bucket),
             order_lyldiff_bucket = ifelse(lyldiff_bucket == "0.1 to (< 0.5)", 6, order_lyldiff_bucket),
             order_lyldiff_bucket = ifelse(lyldiff_bucket == "0.5 to (< 2)", 7, order_lyldiff_bucket),
             order_lyldiff_bucket = ifelse(lyldiff_bucket == ">= 2", 8, order_lyldiff_bucket))

    # pollutuon (difference in % between 2 years) scale
  } else if (scale_type == "poldiff"){

    df %>%
      dplyr::mutate(poldiff_bucket = ifelse((!!as.symbol(col_name) < -10), "< -10", NA),
             poldiff_bucket = ifelse((!!as.symbol(col_name) >= -10) & (!!as.symbol(col_name) < -5), "-10 to (< -5)", poldiff_bucket),
             poldiff_bucket = ifelse((!!as.symbol(col_name) >= -5) & (!!as.symbol(col_name) < -1), "-5 to (< -1)", poldiff_bucket),
             poldiff_bucket = ifelse((!!as.symbol(col_name) >= -1) & (!!as.symbol(col_name) < 0), "-1 to (< 0)", poldiff_bucket),
             poldiff_bucket = ifelse((!!as.symbol(col_name) >= 0) & (!!as.symbol(col_name) < 1), "0 to (< 1)", poldiff_bucket),
             poldiff_bucket = ifelse((!!as.symbol(col_name) >= 1) & (!!as.symbol(col_name) < 5), "1 to (< 5)", poldiff_bucket),
             poldiff_bucket = ifelse((!!as.symbol(col_name) >= 5) & (!!as.symbol(col_name) < 10), "5 to (< 10)", poldiff_bucket),
             poldiff_bucket = ifelse((!!as.symbol(col_name) >= 10), ">= 10", poldiff_bucket)) %>%
      dplyr::mutate(order_poldiff_bucket = ifelse(poldiff_bucket == "< -10", 1, NA),
             order_poldiff_bucket = ifelse(poldiff_bucket == "-10 to (< -5)", 2, order_poldiff_bucket),
             order_poldiff_bucket = ifelse(poldiff_bucket == "-5 to (< -1)", 3, order_poldiff_bucket),
             order_poldiff_bucket = ifelse(poldiff_bucket == "-1 to (< 0)", 4, order_poldiff_bucket),
             order_poldiff_bucket = ifelse(poldiff_bucket == "0 to (< 1)", 5, order_poldiff_bucket),
             order_poldiff_bucket = ifelse(poldiff_bucket == "1 to (< 5)", 6, order_poldiff_bucket),
             order_poldiff_bucket = ifelse(poldiff_bucket == "5 to (< 10)", 7, order_poldiff_bucket),
             order_poldiff_bucket = ifelse(poldiff_bucket == ">= 10", 8, order_poldiff_bucket))

  }

}


#' Plot a histogram of pollution/life years lost columns in the AQLI dataset
#'
#' Plots a histogram of either pollution or life years lost values in AQLI color scale
#'
#' @importFrom stringr str_c
#' @import ggplot2
#'
#' @param df An AQLI dataframe that contains the columns that will be used to plot the histogram.
#' @param scale_type This can take one of 2 values \code{"pollution"} or \code{"lyl"}.
#' @param col_name The column name (in quotes) from \code{df} for which the histogram is to be plotted.
#' @param region_name Plot subtitle (in quotes)
#'
#'
#' @examples
#' df %>% aqli_hist(scale_type = "pollution", col_name = "pm2021", region_name = "")
#' df %>% aqli_hist(scale_type == "lyl", col_name = "llpp_who_2021", region_name = "Delhi")
#'
#' @return returns a histogram plot based on the specifications provided.
#'
#'
#'
#' @export


aqli_hist <- function(df, scale_type = "pollution", col_name = "pm2021", region_name = "enter region name"){
  if(scale_type == "pollution"){
    pol_year <- as.numeric(str_remove(col_name, "pm"))
    x_axis_title <- str_c("Annual average PM2.5 in ", pol_year, "(µg/m³)")
    y_axis_title <- "Number of people"
    plot_title <- str_c("Distribution of Annual Average PM2.5 pollution in", pol_year)
    plot_subtitle <- region_name
    plot_caption <- "*AQLI only reports satellite derived annual average PM2.5 data"
    legend_title <- "PM2.5 (in µg/m³)"
    plt <- df %>%
      add_aqli_color_scale_buckets(scale_type = "pollution", col_name = col_name) %>%
      ggplot() +
      geom_histogram(mapping = aes(x = !!as.symbol(col_name), weight = population, fill = !!as.symbol(col_name), group = !!as.symbol(col_name))) +
      scale_fill_gradient(low = "#a1f5ff",
                          high = "#1a1638") +
      labs(x = x_axis_title, y = y_axis_title, title = plot_title, subtitle = plot_subtitle, caption = plot_caption,
           fill = legend_title) +
      themes_aqli_base
    return(plt)

  } else if(scale_type == "lyl"){
    lyl_year <- as.numeric(str_remove(col_name, "llpp_who_"))
    x_axis_title <- str_c("Life years lost to PM2.5 pollution in ", lyl_year)
    y_axis_title <- "Number of people"
    plot_title <- str_c("Distribution of life years lost to PM2.5 pollution in ", lyl_year)
    plot_subtitle <- region_name
    plot_caption <- "*AQLI only reports satellite derived annual average PM2.5 data."
    legend_title <- "Life years lost"
    plt <- df %>%
      add_aqli_color_scale_buckets(scale_type = "lyl", col_name = col_name) %>%
      ggplot() +
      geom_histogram(mapping = aes(x = !!as.symbol(col_name), weight = population, fill = !!as.symbol(col_name), group = !!as.symbol(col_name))) +
      labs(x = x_axis_title, y = y_axis_title, title = plot_title, subtitle = plot_subtitle, caption = plot_caption,
           fill = legend_title) +
      scale_fill_gradient(low = "#ffeda0",
                          high = "#800026") +
      themes_aqli_base




    return(plt)

  }
}






