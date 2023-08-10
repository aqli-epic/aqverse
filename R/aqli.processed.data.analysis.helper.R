##> AQLI processed data helper functions==================================================

#-------------------------------------------------

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
#' @return when added to a plot, returns a plot with the AQLI plots base theme.
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

#----------------------------------------------------------

#' Plot a histogram of pollution/life years lost columns in the AQLI dataset
#'
#' Plots a histogram of either pollution or life years lost values in AQLI color scale
#'
#' @importFrom stringr str_c
#' @import ggplot2
#' @import ggthemes
#' @importFrom aqverse add_aqli_color_scale_buckets themes_aqli_base
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

#-----------------------------------------------------------


#' Plot aqli pollution and life years lost bar graphs
#'
#' Plots pollution and life years lost regional bar graphs in AQLI colors.
#'
#' @import ggplot2
#' @importFrom aqverse add_aqli_color_scale_buckets themes_aqli_base
#'
#' @param df AQLI data that will be used to plot the bar graph.
#' @param scale_type One of \code{pollution} or \code{lyl}. Depends on the underlying underlying value of \code{y_var} column  (from \code{df}) that is plotted.
#' @param x_var A column of type \code{character}, e.g. depending on the bar graph, this could be country, state, district. See examples below. Note
#'              that this will end up being plotted on y axis as the function by default flips the axis using \code{coord_flip}. You can flip it back
#'              by piping the output of this function again to \code{coord_flip}.
#' @param y_var A column of type \code{numeric}, e.g. life years lost or pollution in a given year. Note that this will end up being plotted on y axis as the function by default flips the axis using \code{coord_flip}. You can flip it back
#'              by piping the output of this function again to \code{coord_flip}.
#'
#' @param title The title of the bar graph.
#' @param subtitle  The subtitle of the bar graph.
#' @param x_label x-axis label of the graph.
#' @param y_label  y-axis label of the graph.
#' @param legend_title Title for the legend of the bar graph.
#' @param caption Caption for the bar graph.
#'
#' @examples
#'
#' # Plotting the graph of life years lost relative to WHO guideline (llpp_who_2021) for all prefectures (name_2) of a aqli gadm2 dataset
#' # that is filtered for China.
#'
#' df %>%
#' aqli_bar(scale_type = "lyl", x_var = "name_2", y_var = "llpp_who_2021", title = "", subtitle = "", x_label = "Prefecture", y_label = "Potential Gain in Life Expectancy (Years)", legend_title = "Potential gain in life expectancy (Years)", caption = "") +
#'  theme(plot.background = element_rect(fill = "white", color = "white"))
#'
#' @note
#' Please note that the aqli color buckets should already be added to \code{df} using \code{add_aqli_color_scale_buckets} function before it gets
#' feeded to \code{aqli_bar}
#'
#' @return returns a bar plot given the above specifications.
#'
#' @export


aqli_bar <- function(df, scale_type = "pollution", x_var, y_var, title, subtitle, x_label, y_label, legend_title, caption){
  if(scale_type == "pollution"){
    plt <- df %>%
      add_aqli_color_scale_buckets(scale_type = "pollution", col_name = y_var) %>%
      ggplot() +
      geom_col(mapping = aes(x = forcats::fct_reorder(!!as.symbol(x_var), !!as.symbol(y_var)), y = !!as.symbol(y_var), fill = forcats::fct_reorder(!!as.symbol("pol_bucket"), !!as.symbol("order_pol_bucket")))) +
      scale_fill_manual(values = c("0 to < 5" = "#a1f5ff",
                                   "5 to < 10" = "#92d4eb",
                                   "10 to < 20" = "#82b5d5",
                                   "20 to < 30" = "#7197be",
                                   "30 to < 40" = "#5f7aa5",
                                   "40 to < 50" = "#4e5e8b",
                                   "50 to < 60" = "#3c456f",
                                   "60 to < 70" = "#2b2d54",
                                   ">= 70" = "#1a1638")) +
      labs(x = x_label, y = y_label, title = title, subtitle = subtitle, caption = caption, fill = legend_title) +
      themes_aqli_base +
      coord_flip()
    return(plt)

  } else if(scale_type == "lyl"){
    plt <- df %>%
      add_aqli_color_scale_buckets(scale_type = "lyl", col_name = y_var) %>%
      ggplot() +
      geom_col(mapping = aes(x = forcats::fct_reorder(!!as.symbol(x_var), !!as.symbol(y_var)), y = !!as.symbol(y_var), fill = forcats::fct_reorder(!!as.symbol("lyl_bucket"), !!as.symbol("order_lyl_bucket")))) +
      scale_fill_manual(values = c("0 to < 0.1" = "#ffffff",
                                   "0.1 to < 0.5" = "#ffeda0",
                                   "0.5 to < 1" = "#fed976",
                                   "1 to < 2" = "#feb24c",
                                   "2 to < 3" = "#fd8d3c",
                                   "3 to < 4" = "#fc4e2a",
                                   "4 to < 5" = "#e31a1c",
                                   "5 to < 6" = "#bd0026",
                                   ">= 6" = "#800026")) +
      labs(x = x_label, y = y_label, title = title, subtitle = subtitle, caption = caption, fill = legend_title) +
      themes_aqli_base +
      coord_flip()
    return(plt)

  }
}


#------------------------------------------------------------

#' Plot AQLI pollution time series plots (trendlines)
#'
#' Plots pollution trends at country, state and district levels
#' from a given start year to a given end year
#'
#' @importFrom stringr str_c
#' @import dplyr
#' @import ggplot2
#' @import ggthemes
#' @importFrom tidyr pivot_longer
#' @importFrom aqverse themes_aqli_base

#'
#' @param gadm2_file aqli gadm2 (district/county/prefecture level) master file.
#' @param level one of \code{country}, \code{state} or \code{district}.
#' @param country_name country for which (the country itself, or a region within that country) the trendlines are to be plotted. Defaults to "India".
#' @param state_name state within the \code{country} specified for which (the state itself, or a region within that state) the
#'                   trendlines are to be plotted. Defaults to "NCT of Delhi"
#' @param district_name district within, the \code{state}, which is in \code{country} specified for which the
#'                      trendlines are to be plotted. Defaults to "NCT of Delhi".
#' @param start_year year from which the trendline will start (defaults to 1998).
#' @param end_year year at which the trendline will end (defaults to 2021).
#'
#'
#' @examples
#'
#' # this will produce a default trendlines plot for India from 1998 to 2021
#' trendlines_aqli(gadm2_file)
#'
#' # this will produce a trendline for the state of Uttar Pradesh from the year 2003 to 2021
#' trendlines_aqli(gadm2_file, level = "state", country_name = "India", state_name = "Uttar Pradesh", start_year = 2003, end_year = 2021)
#'
#' # this will produce a trendline for the district of Ghaziabad in the state of Uttar Pradesh in India from the year 2000 to 2021
#' trendlines_aqli(gadm2_file, level = "state", country_name = "India", state_name = "Uttar Pradesh", district_name = "Ghaziabad", start_year = 2000, end_year = 2021)
#'
#' @return a trend lines plot from \code{start_year} to \code{end_year} for the region specificed at the level specified in \code{level}
#'
#' @export


trendlines_aqli <- function(gadm2_file, level = "country", country_name = "India", state_name = "NCT of Delhi", district_name = "NCT of Delhi", start_year = 1998, end_year = 2021){

  pm_weighted_col_start <- stringr::str_c("pm", start_year, "_weighted")
  pm_weighted_col_end <- stringr::str_c("pm", end_year, "_weighted")

  if(level == "country"){
    trendlines_aqli_data <- gadm2_file %>%
      dplyr::filter(!is.na(population)) %>%
      dplyr::filter(country == country_name) %>%
      dplyr::group_by(country) %>%
      dplyr::mutate(pop_weights = population/sum(population, na.rm = TRUE),
                    mutate(across(starts_with("pm"), ~.x*pop_weights, .names = "{col}_weighted"))) %>%
      dplyr::summarise(across(ends_with("weighted"), sum)) %>%
      tidyr::pivot_longer(cols = !!as.symbol(pm_weighted_col_start):!!as.symbol(pm_weighted_col_end) , names_to = "years",
                          values_to = "pop_weighted_avg_pm2.5") %>%
      dplyr::mutate(years = as.integer(unlist(str_extract(years, "\\d+"))),
                    region = "National Average") %>%
      dplyr::select(years, region, pop_weighted_avg_pm2.5)

    trendlines_aqli_plt <- trendlines_aqli_data %>%
      ggplot2::ggplot() +
      ggplot2::geom_line(mapping = ggplot2::aes(x = years, y = pop_weighted_avg_pm2.5), lwd = 1.1,
                         color = "darkred") +
      ggplot2::scale_x_continuous(breaks = seq(start_year, end_year, 2)) +
      # scale_y_continuous(breaks = seq(0, max(trendlines_aqli_data$pop_weighted_avg_pm2.5), 10)) +
      ggthemes::theme_hc() +
      ggplot2::labs(x = "Years",
                    y = "Annual Average PM2.5 concentration (in µg/m3)") +
      ggplot2::theme(legend.position = "bottom", legend.title = element_blank(),
                     legend.text = element_text(size = 7),
                     axis.title.y = element_text(size = 9),
                     axis.title.x = element_text(size = 9)) +
      themes_aqli_base


  } else if (level == "state") {
    trendlines_aqli_data <- gadm2_file %>%
      dplyr::filter(!is.na(population)) %>%
      dplyr::filter(country == country_name, name_1 == state_name) %>%
      dplyr::group_by(country, name_1) %>%
      dplyr::mutate(pop_weights = population/sum(population, na.rm = TRUE),
                    mutate(across(starts_with("pm"), ~.x*pop_weights, .names = "{col}_weighted"))) %>%
      dplyr::summarise(across(ends_with("weighted"), sum)) %>%
      tidyr::pivot_longer(cols = !!as.symbol(pm_weighted_col_start):!!as.symbol(pm_weighted_col_end) , names_to = "years",
                          values_to = "pop_weighted_avg_pm2.5") %>%
      dplyr::mutate(years = as.integer(unlist(str_extract(years, "\\d+"))),
                    region = "State Average") %>%
      dplyr::select(years, region, pop_weighted_avg_pm2.5)

    trendlines_aqli_plt <- trendlines_aqli_data %>%
      ggplot2::ggplot() +
      ggplot2::geom_line(mapping = ggplot2::aes(x = years, y = pop_weighted_avg_pm2.5), lwd = 1.1,
                         color = "darkred") +
      ggplot2::scale_x_continuous(breaks = seq(start_year, end_year, 2)) +
      # scale_y_continuous(breaks = seq(0, max(trendlines_aqli_data$pop_weighted_avg_pm2.5), 10)) +
      ggthemes::theme_hc() +
      ggplot2::labs(x = "Years",
                    y = "Annual Average PM2.5 concentration (in µg/m3)") +
      ggplot2::theme(legend.position = "bottom", legend.title = element_blank(),
                     legend.text = element_text(size = 7),
                     axis.title.y = element_text(size = 9),
                     axis.title.x = element_text(size = 9)) +
      themes_aqli_base

  } else if (level == "district") {
    trendlines_aqli_data <- gadm2_file %>%
      dplyr::filter(!is.na(population)) %>%
      dplyr::filter(country == country_name, name_1 == state_name, name_2 == district_name) %>%
      dplyr::group_by(country, name_1, name_2) %>%
      dplyr::mutate(pop_weights = population/sum(population, na.rm = TRUE),
                    mutate(across(starts_with("pm"), ~.x*pop_weights, .names = "{col}_weighted"))) %>%
      dplyr::summarise(across(ends_with("weighted"), sum)) %>%
      tidyr::pivot_longer(cols = !!as.symbol(pm_weighted_col_start):!!as.symbol(pm_weighted_col_end) , names_to = "years",
                          values_to = "pop_weighted_avg_pm2.5") %>%
      dplyr::mutate(years = as.integer(unlist(str_extract(years, "\\d+"))),
                    region = "District Average") %>%
      dplyr::select(years, region, pop_weighted_avg_pm2.5)

    trendlines_aqli_plt <- trendlines_aqli_data %>%
      ggplot2::ggplot() +
      ggplot2::geom_line(mapping = ggplot2::aes(x = years, y = pop_weighted_avg_pm2.5), lwd = 1.1,
                         color = "darkred") +
      ggplot2::scale_x_continuous(breaks = seq(start_year, end_year, 2)) +
      # scale_y_continuous(breaks = seq(0, max(trendlines_aqli_data$pop_weighted_avg_pm2.5), 10)) +
      ggthemes::theme_hc() +
      ggplot2::labs(x = "Years",
                    y = "Annual Average PM2.5 concentration (in µg/m3)") +
      ggplot2::theme(legend.position = "bottom", legend.title = element_blank(),
                     legend.text = element_text(size = 7),
                     axis.title.y = element_text(size = 9),
                     axis.title.x = element_text(size = 9)) +
      themes_aqli_base

  }

  return(trendlines_aqli_plt)

}



#------------------------------------------------------------


#' Plot Country Level GBD Data
#'
#' Plots the GBD data for a specific country, filtering based on the number of most/least deadly threats to life expectancy.
#'
#' @param gbd_master_data The GBD master dataset for all countries.
#' @param country_name The name of the country to plot the GBD data for. Default is "India".
#' @param top_or_bottom Specify whether to consider the most or least deadly threats. Default is "most".
#' @param n_threats The number of most or least deadly threats to consider. Default is 10.
#'
#' @import dplyr
#' @import ggplot2
#' @importFrom ggthemes theme_tufte
#' @importFrom forcats fct_reorder
#' @importFrom aqverse add_aqli_color_scale_buckets themes_aqli_base
#'
#' @examples
#' plot_country_level_gbd(gbd_master_data, country_name = "India", degree = "most", n_threats = 10)
#'
#' @return A plot representing the GBD data for the specified country and threats.
#'
#' @export

plot_country_level_gbd <- function(gbd_master_data, country_name = "India", degree = "most", n_threats = 10){

  # filter GBD data to a particular country and to a given number of threats top or bottom

  if(top_or_bottom == "most"){
    gbd_data_region <- gbd_results_master_2021 %>%
      filter(country == country_name) %>%
      slice_max(lyl, n = n_threats)
  } else if (top_or_bottom == "least"){
    gbd_data_region <- gbd_results_master_2021 %>%
      filter(country == country_name) %>%
      slice_min(lyl, n = n_threats)
  }

  colnames(gbd_data_region)[3] <- c("llpp_who_2021")

  gbd_data_region <- gbd_data_region %>%
    add_aqli_color_scale_buckets("lyl", "llpp_who_2021")

  gbd_plot <- gbd_data_region %>%
    ggplot() +
    geom_col(mapping = aes(x = forcats::fct_reorder(cause_of_death, llpp_who_2021), y = llpp_who_2021, fill = forcats::fct_reorder(lyl_bucket, order_lyl_bucket)), width = 0.4, color = "black") +
    labs(x = "Threats to Life Expectancy", y = "Life Years Lost", fill = "Life years lost",
         title = "") +
    coord_flip() +
    ggthemes::theme_tufte() +
    themes_aqli_base +
    theme(legend.position = "bottom",
          axis.text = element_text(size = 14),
          axis.title.y = element_text(size = 16, margin = margin(r = 0.6, unit = "cm")),
          axis.title.x = element_text(size = 16, margin = margin(t = 0.6, b = 0.6, unit = "cm")),
          plot.caption = element_text(hjust = 0, size = 8, margin = margin(t = 0.8, unit = "cm")),
          plot.title = element_text(hjust = 0.5, size = 20, margin = margin(b = 0.8, unit = "cm")),
          plot.subtitle = element_text(hjust = 0.5, size = 10, margin = margin(b = 0.8, unit = "cm")),
          legend.box.background = element_rect(color = "black"),
          plot.background = element_rect(fill = "white", color = "white"),
          axis.line = element_line(),
          legend.text = element_text(size = 11),
          legend.title = element_text(size = 14),
          panel.grid.major.y = element_blank()) +
    scale_y_continuous(breaks = seq(0, 7, 1)) +
    # scale_x_discrete(limits = cause_of_death_ordered[seq(1, length(cause_of_death_ordered), by = 2)]) +
    scale_fill_manual(values = c("0 to < 0.1" = "#ffffff",
                                 "0.1 to < 0.5" = "#ffeda0",
                                 "0.5 to < 1" = "#fed976",
                                 "1 to < 2" = "#feb24c",
                                 "2 to < 3" = "#fd8d3c",
                                 "3 to < 4" = "#fc4e2a",
                                 "4 to < 5" = "#e31a1c",
                                 "5 to < 6" = "#bd0026",
                                 ">= 6" = "#800026")) +
    guides(fill = guide_legend(nrow = 1))

  return(gbd_plot)

}






#--------------------------------------------------------------

#' Plot AQLI and life expectancy gains single year maps
#'
#' This function takes in AQLI (Air Quality Life Index) and life expectancy data in CSV format, a
#' and geographic shapefiles representing different administrative levels. It generates a map visualizing
#' either the potential gain in life expectancy or air pollution levels (PM2.5) for a specific region
#' (country, state, or district) based on the specified parameters.
#'
#' @param gadm2_aqli_csv A CSV file containing AQLI and life expectancy data at the GADM2 (district/county/prefecture) level.
#' @param gadm2_aqli_shapefile A geographic shapefile for GADM2 (district/county etc) administrative level polygons.
#' @param gadm1_aqli_shapefile A geographic shapefile for GADM1 (state/provinces) administrative level polygons.
#' @param gadm0_aqli_shapefile A geographic shapefile for GADM0 (country) administrative level polygons.
#' @param region_level The administrative level of the region to plot (country, state, or district).
#' @param col_name_plt The name of the column in the CSV containing pollution or potential life expectancy gains data.
#' @param plot_type The type of plot: "lyl" for life expectancy or "pollution" for AQI.
#' @param country_name The name of the country for which to generate the map.
#' @param state_name The name of the state (if applicable) for which to generate the map.
#' @param district_name The name of the district (if applicable) for which to generate the map.
#'
#' @return A ggplot object displaying the map.
#'
#' @import ggplot2
#' @import dplyr
#' @import sf
#' @import ggthemes
#' @import forcats
#'
#' @examples
#' plot_aqli_pol_lyl_map(gadm2_aqli_csv, gadm2_aqli_shapefile, gadm1_aqli_shapefile, gadm0_aqli_shapefile, "state", "llpp_who_2021", "lyl", "India", "Karnataka", NULL)
#'
#' @export
plot_aqli_pol_lyl_map <- function(gadm2_aqli_csv, gadm2_aqli_shapefile, gadm1_aqli_shapefile, gadm0_aqli_shapefile, region_level, col_name_plt, plot_type, country_name, state_name, district_name){

  if(region_level == "country"){
    map_data <- gadm2_aqli_csv %>%
      filter(country == country_name) %>%
      left_join(gadm2_aqli_shapefile, by = c("objectid_gadm2" = "obidgadm2"))
  } else if (region_level == "state"){
    map_data <- gadm2_aqli_csv %>%
      filter(country == country_name, name_1 == state_name) %>%
      left_join(gadm2_aqli_shapefile, by = c("objectid_gadm2" = "obidgadm2"))

  } else if (region_level == "district"){
    map_data <- gadm2_aqli_csv %>%
      filter(country == country_name, name_1 == state_name, name_2 == district_name) %>%
      left_join(gadm2_aqli_shapefile, by = c("objectid_gadm2" = "obidgadm2"))
  }

  tmp_breaker <- 999

  #-----------------------------------lyl-----------------------------------#
  if(plot_type == "lyl"){
   map_data <- map_data %>%
      add_aqli_color_scale_buckets("lyl", col_name_plt) %>%
      select(-geometry, geometry) %>%
      st_as_sf()
   if(region_level == "country"){
     plt <- map_data %>%
     ggplot() +
       geom_sf(mapping = aes(fill = forcats::fct_reorder(lyl_bucket, order_lyl_bucket)), color = "lightgrey", lwd = 0.05) +
       geom_sf(data = gadm1_aqli_shapefile %>% filter(name0 == country_name), color = "black", fill = "transparent", lwd = 0.5) +
       ggthemes::theme_map() +
       scale_fill_manual(values = c("0 to < 0.1" = "#ffffff",
                                    "0.1 to < 0.5" = "#ffeda0",
                                    "0.5 to < 1" = "#fed976",
                                    "1 to < 2" = "#feb24c",
                                    "2 to < 3" = "#fd8d3c",
                                    "3 to < 4" = "#fc4e2a",
                                    "4 to < 5" = "#e31a1c",
                                    "5 to < 6" = "#bd0026",
                                    ">= 6" = "#800026")) +
       ggthemes::theme_map() +
       labs(fill = "Potential gain in life expectancy (Years) ", title = "") +
       theme(legend.position = "bottom",
             legend.justification = c(0.5, 3),
             legend.background = element_rect(color = "black"),
             legend.text = element_text(size = 14),
             legend.title = element_text(size = 15),
             plot.title = element_text(hjust = 0.5, size = 15),
             # legend.key = element_rect(color = "black"),
             legend.box.margin = margin(b = 1, unit = "cm"),
             plot.subtitle = element_text(hjust = 0.5, size = 7),
             plot.caption = element_text(hjust = 0.7, size = 9, face = "italic"),
             legend.key = element_rect(color = "black"),
             legend.box.spacing = unit(0, "cm"),
             legend.direction = "horizontal",
             plot.background = element_rect(fill = "white", color = "white")) +
       guides(fill = guide_legend(nrow = 1))

     return(plt)
   } else if (region_level == "state"){
    plt <-  map_data %>%
       ggplot() +
       geom_sf(mapping = aes(fill = forcats::fct_reorder(lyl_bucket, order_lyl_bucket)), color = "lightgrey", lwd = 0.05) +
       geom_sf(data = gadm1_aqli_shapefile %>% filter(name0 == country_name, name1  == state_name), color = "black", fill = "transparent", lwd = 0.5) +
       ggthemes::theme_map() +
       scale_fill_manual(values = c("0 to < 0.1" = "#ffffff",
                                    "0.1 to < 0.5" = "#ffeda0",
                                    "0.5 to < 1" = "#fed976",
                                    "1 to < 2" = "#feb24c",
                                    "2 to < 3" = "#fd8d3c",
                                    "3 to < 4" = "#fc4e2a",
                                    "4 to < 5" = "#e31a1c",
                                    "5 to < 6" = "#bd0026",
                                    ">= 6" = "#800026")) +
       ggthemes::theme_map() +
       labs(fill = "Potential gain in life expectancy (Years) ", title = "") +
       theme(legend.position = "bottom",
             legend.justification = c(0.5, 3),
             legend.background = element_rect(color = "black"),
             legend.text = element_text(size = 14),
             legend.title = element_text(size = 15),
             plot.title = element_text(hjust = 0.5, size = 15),
             # legend.key = element_rect(color = "black"),
             legend.box.margin = margin(b = 1, unit = "cm"),
             plot.subtitle = element_text(hjust = 0.5, size = 7),
             plot.caption = element_text(hjust = 0.7, size = 9, face = "italic"),
             legend.key = element_rect(color = "black"),
             legend.box.spacing = unit(0, "cm"),
             legend.direction = "horizontal",
             plot.background = element_rect(fill = "white", color = "white")) +
       guides(fill = guide_legend(nrow = 1))

    return(plt)

   } else if (region_level == "district"){
    plt <- map_data %>%
       ggplot() +
       geom_sf(mapping = aes(fill = forcats::fct_reorder(lyl_bucket, order_lyl_bucket)), color = "lightgrey", lwd = 0.05) +
       ggthemes::theme_map() +
       scale_fill_manual(values = c("0 to < 0.1" = "#ffffff",
                                    "0.1 to < 0.5" = "#ffeda0",
                                    "0.5 to < 1" = "#fed976",
                                    "1 to < 2" = "#feb24c",
                                    "2 to < 3" = "#fd8d3c",
                                    "3 to < 4" = "#fc4e2a",
                                    "4 to < 5" = "#e31a1c",
                                    "5 to < 6" = "#bd0026",
                                    ">= 6" = "#800026")) +
       ggthemes::theme_map() +
       labs(fill = "Potential gain in life expectancy (Years) ", title = "") +
       theme(legend.position = "bottom",
             legend.justification = c(0.5, 3),
             legend.background = element_rect(color = "black"),
             legend.text = element_text(size = 14),
             legend.title = element_text(size = 15),
             plot.title = element_text(hjust = 0.5, size = 15),
             # legend.key = element_rect(color = "black"),
             legend.box.margin = margin(b = 1, unit = "cm"),
             plot.subtitle = element_text(hjust = 0.5, size = 7),
             plot.caption = element_text(hjust = 0.7, size = 9, face = "italic"),
             legend.key = element_rect(color = "black"),
             legend.box.spacing = unit(0, "cm"),
             legend.direction = "horizontal",
             plot.background = element_rect(fill = "white", color = "white")) +
       guides(fill = guide_legend(nrow = 1))

     return(plt)

   }

   #--------------------pollution----------------------------------#

  } else if (plot_type == "pollution"){
   map_data <- map_data %>%
      add_aqli_color_scale_buckets("pollution", col_name_plt) %>%
      select(-geometry, geometry) %>%
      st_as_sf()

   if(region_level == "country"){
     plt <- map_data %>%
       ggplot() +
       geom_sf(mapping = aes(fill = forcats::fct_reorder(pol_bucket, order_pol_bucket)), color = "lightgrey", lwd = 0.05) +
       geom_sf(data = gadm1_aqli_shapefile %>% filter(name0 == country_name), color = "black", fill = "transparent", lwd = 0.5) +
       ggthemes::theme_map() +
       scale_fill_manual(values = c("0 to < 5" = "#a1f5ff",
                                    "5 to < 10" = "#92d4eb",
                                    "10 to < 20" = "#82b5d5",
                                    "20 to < 30" = "#7197be",
                                    "30 to < 40" = "#5f7aa5",
                                    "40 to < 50" = "#4e5e8b",
                                    "50 to < 60" = "#3c456f",
                                    "60 to < 70" = "#2b2d54",
                                    ">= 70" = "#1a1638")) +
       ggthemes::theme_map() +
       labs(fill = expression("Annual Average" ~ PM[2.5] ~ " (in µg/m³)"), title = "") +
       theme(legend.position = "bottom",
             legend.justification = c(0.5, 3),
             legend.background = element_rect(color = "black"),
             legend.text = element_text(size = 14),
             legend.title = element_text(size = 15),
             plot.title = element_text(hjust = 0.5, size = 15),
             # legend.key = element_rect(color = "black"),
             legend.box.margin = margin(b = 1, unit = "cm"),
             plot.subtitle = element_text(hjust = 0.5, size = 7),
             plot.caption = element_text(hjust = 0.7, size = 9, face = "italic"),
             legend.key = element_rect(color = "black"),
             legend.box.spacing = unit(0, "cm"),
             legend.direction = "horizontal",
             plot.background = element_rect(fill = "white", color = "white")) +
       guides(fill = guide_legend(nrow = 1))

     return(plt)
   } else if (region_level == "state"){
     plt <- map_data %>%
       ggplot() +
       geom_sf(mapping = aes(fill = forcats::fct_reorder(pol_bucket, order_pol_bucket)), color = "lightgrey", lwd = 0.05) +
       geom_sf(data = gadm1_aqli_shapefile %>% filter(name0 == country_name, name1 == state_name), color = "black", fill = "transparent", lwd = 0.5) +
       ggthemes::theme_map() +
       scale_fill_manual(values = c("0 to < 5" = "#a1f5ff",
                                    "5 to < 10" = "#92d4eb",
                                    "10 to < 20" = "#82b5d5",
                                    "20 to < 30" = "#7197be",
                                    "30 to < 40" = "#5f7aa5",
                                    "40 to < 50" = "#4e5e8b",
                                    "50 to < 60" = "#3c456f",
                                    "60 to < 70" = "#2b2d54",
                                    ">= 70" = "#1a1638")) +
       ggthemes::theme_map() +
       labs(fill = expression("Annual Average" ~ PM[2.5] ~ " (in µg/m³)"), title = "") +
       theme(legend.position = "bottom",
             legend.justification = c(0.5, 3),
             legend.background = element_rect(color = "black"),
             legend.text = element_text(size = 14),
             legend.title = element_text(size = 15),
             plot.title = element_text(hjust = 0.5, size = 15),
             # legend.key = element_rect(color = "black"),
             legend.box.margin = margin(b = 1, unit = "cm"),
             plot.subtitle = element_text(hjust = 0.5, size = 7),
             plot.caption = element_text(hjust = 0.7, size = 9, face = "italic"),
             legend.key = element_rect(color = "black"),
             legend.box.spacing = unit(0, "cm"),
             legend.direction = "horizontal",
             plot.background = element_rect(fill = "white", color = "white")) +
       guides(fill = guide_legend(nrow = 1))

     return(plt)

   } else if (region_level == "district"){
     plt <- map_data %>%
       ggplot() +
       geom_sf(mapping = aes(fill = forcats::fct_reorder(pol_bucket, order_pol_bucket)), color = "lightgrey", lwd = 0.05) +
       ggthemes::theme_map() +
       scale_fill_manual(values = c("0 to < 5" = "#a1f5ff",
                                    "5 to < 10" = "#92d4eb",
                                    "10 to < 20" = "#82b5d5",
                                    "20 to < 30" = "#7197be",
                                    "30 to < 40" = "#5f7aa5",
                                    "40 to < 50" = "#4e5e8b",
                                    "50 to < 60" = "#3c456f",
                                    "60 to < 70" = "#2b2d54",
                                    ">= 70" = "#1a1638")) +
       ggthemes::theme_map() +
       labs(fill = expression("Annual Average" ~ PM[2.5] ~ " (in µg/m³)"), title = "") +
       theme(legend.position = "bottom",
             legend.justification = c(0.5, 3),
             legend.background = element_rect(color = "black"),
             legend.text = element_text(size = 14),
             legend.title = element_text(size = 15),
             plot.title = element_text(hjust = 0.5, size = 15),
             # legend.key = element_rect(color = "black"),
             legend.box.margin = margin(b = 1, unit = "cm"),
             plot.subtitle = element_text(hjust = 0.5, size = 7),
             plot.caption = element_text(hjust = 0.7, size = 9, face = "italic"),
             legend.key = element_rect(color = "black"),
             legend.box.spacing = unit(0, "cm"),
             legend.direction = "horizontal",
             plot.background = element_rect(fill = "white", color = "white")) +
       guides(fill = guide_legend(nrow = 1))

   }

  }

}


#------------------------------------------------------------

#' AQLI website map summary sentence cross-check function




aqli_map_sentence_stats <- function(df, level = "global", data_type = "lyl", year1_col = "llpp_who_2021",
                                    year2_col = "llpp_who_2014", country_name = "India",
                                    state_name = "Uttar Pradesh", district_name = "Ghaziabad"){

  # global----------
  if(level == "global"){

    if(data_type == "lyl"){
      df %>%
        mutate(pop_weights = population/sum(population, na.rm = TRUE),
               lyl_year1_weighted = pop_weights*(!!as.symbol(year1_col)),
               lyl_year2_weighted = pop_weights*(!!as.symbol(year2_col))) %>%
        summarise(avg_lyl_year1 = round(sum(lyl_year1_weighted, na.rm = TRUE), 2),
                  avg_lyl_year2 = round(sum(lyl_year2_weighted, na.rm = TRUE), 2)) %>%
        mutate(lyl_diff = avg_lyl_year1 - avg_lyl_year2)
    } else if(data_type == "pol"){
      df %>%
        mutate(pop_weights = population/sum(population, na.rm = TRUE),
               pm_year1_weighted = pop_weights*(!!as.symbol(year1_col)),
               pm_year2_weighted = pop_weights*(!!as.symbol(year2_col))) %>%
        summarise(avg_pm_year1 = round(sum(pm_year1_weighted, na.rm = TRUE), 2),
                  avg_pm_year2 = round(sum(pm_year2_weighted, na.rm = TRUE), 2)) %>%
        mutate(perc_change = ((avg_pm_year1 - avg_pm_year2)/avg_pm_year2)*100)

    }
  } else if(level == "country"){

    if(data_type == "lyl"){
      df %>%
        filter(country == country_name) %>%
        mutate(pop_weights = population/sum(population, na.rm = TRUE),
               lyl_year1_weighted = pop_weights*(!!as.symbol(year1_col)),
               lyl_year2_weighted = pop_weights*(!!as.symbol(year2_col))) %>%
        summarise(avg_lyl_year1 = round(sum(lyl_year1_weighted, na.rm = TRUE), 2),
                  avg_lyl_year2 = round(sum(lyl_year2_weighted, na.rm = TRUE), 2)) %>%
        mutate(lyl_diff = avg_lyl_year1 - avg_lyl_year2)
    } else if(data_type == "pol"){
      df %>%
        filter(country == country_name) %>%
        mutate(pop_weights = population/sum(population, na.rm = TRUE),
               pm_year1_weighted = pop_weights*(!!as.symbol(year1_col)),
               pm_year2_weighted = pop_weights*(!!as.symbol(year2_col))) %>%
        summarise(avg_pm_year1 = round(sum(pm_year1_weighted, na.rm = TRUE), 2),
                  avg_pm_year2 = round(sum(pm_year2_weighted, na.rm = TRUE), 2)) %>%
        mutate(perc_change = ((avg_pm_year1 - avg_pm_year2)/avg_pm_year2)*100)

    }

  } else if(level == "state"){

  if(data_type == "lyl"){
    df %>%
      filter(country == country_name, name_1 == state_name) %>%
      mutate(pop_weights = population/sum(population, na.rm = TRUE),
             lyl_year1_weighted = pop_weights*(!!as.symbol(year1_col)),
             lyl_year2_weighted = pop_weights*(!!as.symbol(year2_col))) %>%
      summarise(avg_lyl_year1 = round(sum(lyl_year1_weighted, na.rm = TRUE), 2),
                avg_lyl_year2 = round(sum(lyl_year2_weighted, na.rm = TRUE), 2)) %>%
      mutate(lyl_diff = avg_lyl_year1 - avg_lyl_year2)
} else if(data_type == "pol"){
    df %>%
      filter(country == country_name, name_1 == state_name) %>%
      mutate(pop_weights = population/sum(population, na.rm = TRUE),
             pm_year1_weighted = pop_weights*(!!as.symbol(year1_col)),
             pm_year2_weighted = pop_weights*(!!as.symbol(year2_col))) %>%
    summarise(avg_pm_year1 = round(sum(pm_year1_weighted, na.rm = TRUE), 2),
              avg_pm_year2 = round(sum(pm_year2_weighted, na.rm = TRUE), 2)) %>%
    mutate(perc_change = ((avg_pm_year1 - avg_pm_year2)/avg_pm_year2)*100)


}



} else if(level == "district"){

  if(data_type == "lyl"){
    df %>%
      filter(country == country_name, name_1 == state_name, name_2 == district_name) %>%
      mutate(pop_weights = population/sum(population, na.rm = TRUE),
             lyl_year1_weighted = pop_weights*(!!as.symbol(year1_col)),
             lyl_year2_weighted = pop_weights*(!!as.symbol(year2_col))) %>%
      summarise(avg_lyl_year1 = round(sum(lyl_year1_weighted, na.rm = TRUE), 2),
                avg_lyl_year2 = round(sum(lyl_year2_weighted, na.rm = TRUE), 2)) %>%
      mutate(lyl_diff = avg_lyl_year1 - avg_lyl_year2)
  } else if(data_type == "pol"){
    df %>%
      filter(country == country_name, name_1 == state_name, name_2 == district_name) %>%
      mutate(pop_weights = population/sum(population, na.rm = TRUE),
             pm_year1_weighted = pop_weights*(!!as.symbol(year1_col)),
             pm_year2_weighted = pop_weights*(!!as.symbol(year2_col))) %>%
      summarise(avg_pm_year1 = round(sum(pm_year1_weighted, na.rm = TRUE), 2),
                avg_pm_year2 = round(sum(pm_year2_weighted, na.rm = TRUE), 2)) %>%
      mutate(perc_change = ((avg_pm_year1 - avg_pm_year2)/avg_pm_year2)*100)

  }


}

}





#-------------------------------------------------------------

#' percent of population in a given region, between x and y micrograms per cubic meter (upcoming)
#'
#'

#-------------------------------------------------------------

#'
