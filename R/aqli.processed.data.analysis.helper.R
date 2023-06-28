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
#' @param perc_red_by custom reduction percentage, i.e. if columns for custom reduction are needed. For example, if we want to see
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
      dplyr::summarise(across(dplyr::contains("pop_weighted"), ~(round(sum(.x, na.rm = TRUE), 2)), .names = "avg_{col}"),
                       total_population = sum(population, na.rm = TRUE), objectid_gadm2 = objectid_gadm2[1], iso_alpha3 = iso_alpha3[1], whostandard = whostandard[1], .groups = "keep") %>%
      select(objectid_gadm2, iso_alpha3, all_of(level_col_name_vec), total_population, whostandard, dplyr::everything()) %>%
      dplyr::mutate(objectid_gadm2 = dplyr::row_number()) %>%
      dplyr::rename(population = total_population,
                    objectid_level = objectid_gadm2) %>%
      dplyr::rename_with(~str_replace_all(.x, "(_pop_weighted)|(avg_)", ""), dplyr::contains("_pop_weighted")) %>%
      dplyr::mutate(across(starts_with("pm"), (~(.x - whostandard)*le_constant), .names = "llpp_who_{col}")) %>%
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
      dplyr::select(objectid_level:whostandard, all_of(c(pol_col_names, pol_col_names_red_to, llpp_who_col_names, llpp_pol_red_names, total_lyl_who_columns, total_lyl_pol_red_to_columns)))

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
                                                                      total_lyl_nat_columns, total_lyl_pol_red_to_columns)))
    }

    return(aqli_wide)

  } else {
    aqli_wide <- df %>%
      dplyr::group_by_at(level_col_name_vec) %>%
      dplyr::mutate(pop_weights = population/sum(population, na.rm = TRUE)) %>%
      dplyr::mutate(across(dplyr::starts_with("pm"), ~(.x*pop_weights), .names = "{col}_pop_weighted")) %>%
      dplyr::summarise(across(dplyr::contains("pop_weighted"), ~(round(sum(.x, na.rm = TRUE), 2)), .names = "avg_{col}"),
                       total_population = sum(population, na.rm = TRUE), objectid_gadm2 = objectid_gadm2[1], iso_alpha3 = iso_alpha3[1], whostandard = whostandard[1], natstandard = natstandard[1], .groups = "keep") %>%
      select(objectid_gadm2, iso_alpha3, all_of(level_col_name_vec), total_population, whostandard, natstandard, dplyr::everything()) %>%
      dplyr::mutate(objectid_gadm2 = dplyr::row_number()) %>%
      dplyr::rename(population = total_population,
                    objectid_level = objectid_gadm2) %>%
      dplyr::rename_with(~str_replace_all(.x, "(_pop_weighted)|(avg_)", ""), dplyr::contains("_pop_weighted")) %>%
      dplyr::mutate(across(starts_with("pm"), (~(.x - whostandard)*le_constant), .names = "llpp_who_{col}")) %>%
      dplyr::mutate(across(starts_with("pm"), (~round((.x*((perc_red_by)/100)), 1)*le_constant), .names = "llpp_pol_red_{col}")) %>%
      dplyr::mutate(across(starts_with("pm"), (~(.x - natstandard)*le_constant), .names = "llpp_nat_{col}")) %>%
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
                                                         total_lyl_nat_columns, total_lyl_pol_red_to_columns)))

    return(aqli_wide)

  }
}
