
# Pull Target Data from DATIM ---------------------------------------------

#' DATIM API Call for Targets
#'
#' @param url supply url forAPI call, recommend using`gen_url()`
#' @param username DATIM username
#' @param password DATIM password, recommend using `mypwd()`
#'
#' @export
#'
#' @examples
#' \dontrun{
#'  myurl <- paste0(baseurl, "api/29/analytics.json?
#'                  dimension=LxhLO68FcXm:udCop657yzi&
#'                  dimension=ou:LEVEL-4;HfVjCurKxh2&
#'                  filter=pe:2018Oct&
#'                  displayProperty=SHORTNAME&outputIdScheme=CODE")
#'  myuser <- "UserX"
#'  df_targets <- get_datim_targets(myurl, myuser, mypwd(myuser)) }

get_datim_targets <- function(url, username, password) {

  .Deprecated("get_datim_data")

  get_datim_data(url, username, password)

}

# Pull Target Data from DATIM ---------------------------------------------

#' DATIM API Call for Targets
#'
#' @param url supply url forAPI call, recommend using `gen_url()`
#' @param username DATIM username
#' @param password DATIM password, recommend using `mypwd()`
#'
#' @export
#'
#' @examples
#' \dontrun{
#'  myurl <- paste0(baseurl, "api/29/analytics.json?
#'                  dimension=LxhLO68FcXm:udCop657yzi&
#'                  dimension=ou:LEVEL-4;HfVjCurKxh2&
#'                  filter=pe:2018Oct&
#'                  displayProperty=SHORTNAME&outputIdScheme=CODE")
#'  myuser <- "UserX"
#'  df_datim <- get_datim_data(myurl, myuser, mypwd(myuser)) }

get_datim_data <- function(url, username, password) {

  glamr::package_check("httr")
  glamr::package_check("jsonlite")

  json <- url %>%
    httr::GET(httr::authenticate(username,password)) %>%
    httr::content("text") %>%
    jsonlite::fromJSON()

  if ( NROW(json$rows) > 0 ) {
    metadata <- purrr::map_dfr(json$metaData$items, dplyr::bind_rows, .id = "from")

    df <- tibble::as_tibble(json$rows, .name_repair = ~ json$headers$column)

    orguids <- df$`Organisation unit`

    if(stringr::str_detect(url, "hierarchyMeta=true")){

      orgpath <- dplyr::bind_rows(json$metaData$ouHierarchy) %>%
        tidyr::gather()

      levels <- orgpath$value %>%
        stringr::str_count("/") %>%
        max() + 1

      headers <- paste0("orglvl_", seq(1:levels))

      df <- dplyr::left_join(df, orgpath, by = c("Organisation unit" = "key")) %>%
        tidyr::separate(value, headers, sep = "/")
    }

    df <- df %>%
      dplyr::mutate_all(~plyr::mapvalues(., metadata$from, metadata$name, warn_missing = FALSE)) %>%
      dplyr::mutate(Value = as.numeric(Value)) %>%
      dplyr::bind_cols(orgunituid = orguids)

    return(df)

  } else {

    return(NULL)

  }
}
# Compile URL -------------------------------------------------------------

#' Generate a API URL
#'
#' @param ou_uid UID for the country, recommend using `identify_ouuids()`
#' @param org_lvl org hierarchy level, eg facility is level 7 in country X, recommend using `identify_levels()`
#' @param org_type organization type, either facility (default) or community
#' @param value_type results (default) or targets
#' @param fy_pd fiscal year(s) to cover, default will be current FY if not provided
#' @param is_hts is the API for HTS indicators (HTS_TST or HTS_TST_POS), default = FALSE
#' @param baseurl API base url, default = https://final.datim.org/
#'
#' @examples
#' \dontrun{
#'  #get OU UID
#'   ouuid <- identify_ouuids() %>% dplyr::filter(ou == "Ghana")
#'  #get facility level
#'   faclvl <- identify_levels("facility",
#'                             username = myuser, password = mypwd()) %>%
#'                             dplyr::filter(ou == "Ghana")
#'  #gen url
#'   myurl <- gen_url(ouuid, faclvl, org_type = facility) }

gen_url <- function(ou_uid, org_lvl,
                    org_type = "facility",
                    value_type = "results",
                    is_hts = FALSE,
                    fy_pd = NULL,
                    baseurl = "https://final.datim.org/"){

  if(is.null(fy_pd))
    fy_pd <- curr_fy

  if(is.integer(fy_pd)){
    cy_pd <- paste0(fy_pd-1, "Oct", collapse = ";")
  } else {
    cy_pd <- fy_pd
  }

  core_url <-
    paste0(baseurl,"api/29/analytics?",
           "dimension=pe:", cy_pd, "&", #period
           "dimension=ou:LEVEL-", org_lvl, ";", ou_uid, "&", #level and ou
           "dimension=bw8KHXzxd9i:NLV6dy7BE2O&", #Funding Agency -> USAID
           "dimension=SH885jaRe0o&", #Funding Mechanism
           "dimension=xRo1uG2KJHk&", #Age: <15/15+ (Coarse)
           "dimension=jyUTj5YC3OK&", #Cascade sex
           "dimension=IeMmjHyBUpi:",
             ifelse(value_type == "results", "Jh0jDM5yQ2E", "W8imnja2Owd"), "&")  #Targets / Results -># targets = W8imnja2Owd, results = Jh0jDM5yQ2E

  if(is_hts == TRUE){
    tech_url <-
      paste0(core_url,
             "dimension=LxhLO68FcXm:f5IPTM7mieH;wdoUps1qb3V;BTIqHnjeG7l;rI3JlpiuwEK;CUblPgOMGaT&", #technical area
             "dimension=ra9ZqrTtSQn&", #HTS Modality (USE ONLY for FY20,21 Results/FY21,22 Targets)
             "dimension=bDWsPYyXgWP:awSDzziN3Dn;EvyNJHbQ7ZE;mSBg9AZx1lV;viYXyEy7wKi&") #HIV Test Status (Specific)) - Pos/Neg + New Pos/Neg
  } else {
    tech_url <-
      paste0(core_url,
             # "dimension=LxhLO68FcXm:", ifelse(org_type == "community", "gma5vVZgK49","udCop657yzi;MvszPTQrUhy;gma5vVZgK49;wdoUps1qb3V"), "&", #technical areas, prep targets at community
             "dimension=LxhLO68FcXm:udCop657yzi;MvszPTQrUhy;gma5vVZgK49;wdoUps1qb3V&", #technical areas
             "dimension=HWPJnUTMjEq:Qbz6SrpmJ1y;h0pvSVe1TYf;pxz2gGSIQhG&") #Disaggregation Type -> Age/Sex, Age/Sex/HIVStatus, Age Aggregated/Sex/HIVStatus
  }

  if(org_type == "community")
    tech_url <-
    paste0(tech_url,
           "dimension=mINJi7rR1a6:PvuaP6YALSA;AookYR4ECPH&") #Type of organisational unit -> Community & Other organisation unit type

  final_url <-
    paste0(tech_url,
           "displayProperty=SHORTNAME&skipMeta=false&hierarchyMeta=true")

  return(final_url)

}


# Extract All Targets -----------------------------------------------------

#' Extract DATIM Results and Targets (DATIM API Call)
#'
#' @param ou_name Operating Unit name, if mechanism is not specified
#' @param username DATIM username
#' @param password DATIM password, recommend using `mypwd()`
#' @param baseurl API base url, default = https://final.datim.org/
#' @param fy_pd fiscal year(s) to cover, default will be current FY if not provided
#' @param quarters_complete no. of quarters completed through FY to determine weeks left in year
#' @param folderpath_output folder path to store DATIM output, default = NULL
#'
#' @export
#'
#' @examples
#' \dontrun{
#'  #ou mer data
#'  myuser <- "UserX"
#'  mech_x_dta <- pull_mer(ou_name = "Namibia", username = myuser, password = mypwd(myuser))
#'  }

pull_mer <- function(ou_name = NULL,
                     username, password,
                     baseurl = "https://final.datim.org/",
                     fy_pd = NULL,
                     quarters_complete = NULL,
                     folderpath_output = NULL){

  .Deprecated("datim_query")

  print(paste("Extracting data for", ou_name, format(Sys.time(), "%H:%M:%S")))

  #identify reporting levels
  ou_info <- identify_levels(username = username, password = password, baseurl = baseurl) %>%
    dplyr::left_join(identify_ouuids(username = username, password = password, baseurl = baseurl),
                     by = c("country_name" = "displayName")) %>%
    dplyr::filter(operatingunit == ou_name)

  ou_fac <- ou_info$facility
  ou_comm <- ou_info$community
  ou_psnu <- ou_info$prioritization
  if(ou_name %in% c("Burkina Faso", "Jamaica", "Liberia", "Mali", "Senegal"))
    ou_psnu <- 4
  ou_uid <- ou_info$id

  #period
  if(is.null(fy_pd))
    fy_pd <- curr_fy

  #pull non-HTS data results (vars only facility)
  df_nonhts_results <-
    gen_url(ou_uid, ou_fac, fy_pd = fy_pd, baseurl = baseurl) %>%
    get_datim_data(username, password)

  #remove VMMC_CIRC Age/Sex/HIVStatus results since targets and results reported under Age/Sex in FY21
  if(!is.null(df_nonhts_results))
    df_nonhts_results <- dplyr::filter(df_nonhts_results, !(`Technical Area` == "VMMC_CIRC" & `Disaggregation Type` == "Age/Sex/HIVStatus"))

  #pull non-HTS data results (vars only facility)
  df_nonhts_targets <-
    gen_url(ou_uid, ou_psnu, value_type = "targets", fy_pd = fy_pd, baseurl = baseurl) %>%
    get_datim_data(username, password)

  #add in country name for select countries with wrong DATIM target hierarchy
  if(!is.null(df_nonhts_targets) && ou_name %in% c("Burkina Faso", "Jamaica", "Liberia", "Mali", "Senegal"))
    df_nonhts_targets <- tibble::add_column(df_nonhts_targets, orglvl_4 := {ou_name}, .after = "orglvl_3")

  #remove VMMC_CIRC Age/Sex results since targets and results reported under Age/Sex/HIVStatus in FY21
  if(!is.null(df_nonhts_targets))
    df_nonhts_targets <- dplyr::filter(df_nonhts_targets, !(`Technical Area` == "VMMC_CIRC" & `Disaggregation Type` == "Age/Sex"))

  #pull HTS data (facility) results
  df_hts_fac_results <-
    gen_url(ou_uid, ou_fac, is_hts = TRUE, fy_pd = fy_pd, baseurl = baseurl) %>%
    get_datim_data(username, password)

  #pull HTS data (community) results
  df_hts_comm_results <-
    gen_url(ou_uid, ou_comm, org_type = "community", is_hts = TRUE, fy_pd = fy_pd, baseurl = baseurl) %>%
    get_datim_data(username, password)

  #add community level if same as psnu, otherwise will be missing
  if(!is.null(df_hts_comm_results) && ou_psnu == ou_comm)
    df_hts_comm_results <- dplyr::mutate(df_hts_comm_results, !!paste0("orglvl_", ou_psnu) := `Organisation unit`)

  #pull HTS data targets
  df_hts_targets <-
    gen_url(ou_uid, ou_psnu, value_type = "targets", is_hts = TRUE, fy_pd = fy_pd, baseurl = baseurl) %>%
    get_datim_data(username, password)

  #add in country name for select countries with wrong DATIM target hierarchy
  if(!is.null(df_hts_targets) && ou_name %in% c("Burkina Faso", "Jamaica", "Liberia", "Mali", "Senegal"))
    df_hts_targets <- tibble::add_column(df_hts_targets, orglvl_4 := {ou_name}, .after = "orglvl_3")

  #ensure data exists before continuing
  data_exists <- (max(nrow(df_nonhts_results), nrow(df_nonhts_targets),
                      nrow(df_hts_fac_results), nrow(df_hts_comm_results),
                      nrow(df_hts_targets), 1, na.rm = TRUE) - 1) > 0

  data_exists_hts <- (max(nrow(df_hts_fac_results), nrow(df_hts_comm_results),
                          nrow(df_hts_targets), 1, na.rm = TRUE) - 1) > 0

  if(data_exists){

    if(data_exists_hts){
    #combine all HTS data
    df_combo_hts <- dplyr::bind_rows(df_hts_fac_results, df_hts_comm_results, df_hts_targets)

    #remove extra status (known pos, recent negatives, unknown status) & unify technical area
      df_combo_hts <- df_combo_hts %>%
        dplyr::filter(!`HIV Test Status (Specific)` %in%
                        c("Known at Entry Positive (Specific)",
                          "Recent Negatives (Specific)",
                          "HIV Status Unknown (Specific)")) %>%
        dplyr::mutate(`Technical Area` = "HTS_TST")

    #create HTS_TST_POS
      df_hts_pos <- df_combo_hts %>%
        dplyr::filter(`HIV Test Status (Specific)` %in% c("HIV Positive (Specific)",
                                                          "Newly Tested Positives (Specific)")) %>%
        dplyr::mutate(`Technical Area` = "HTS_TST_POS")

    #bind and aggregate HTS and HTS_POS
      grp_keep <- names(df_combo_hts) %>%
        dplyr::setdiff(c("HTS Modality (USE ONLY for FY20,21 Results/FY21,22 Targets)",
                         "HIV Test Status (Specific)",
                         "Type of organisational unit",
                         "Value"))

      df_combo_hts <- df_combo_hts %>%
        dplyr::bind_rows(df_hts_pos) %>%
        dplyr::group_by_at(grp_keep) %>%
        dplyr::summarise(Value = sum(Value, na.rm = TRUE)) %>%
        dplyr::ungroup()
    } else {
      df_combo_hts <- NULL
    }

    #combine non HTS and HTS dfs
      df_combo <- dplyr::bind_rows(df_nonhts_results, df_nonhts_targets, df_combo_hts)

    #clean up orgunits, keeping just OU, PSNU, Community and Facility
      if(!"orglvl_4" %in% names(df_combo))
        df_combo <- dplyr::mutate(df_combo, orglvl_4 = `Organisation unit`)
      country_name <- unique(df_combo$orglvl_3)
      if(stringr::str_detect(country_name, "Region"))
        country_name <- unique(df_combo$orglvl_4) %>% setdiff(NA)

      df_combo <- purrr::map_dfr(.x = country_name,
                                 .f = ~ hierarchy_rename(df_combo, .x, username, password, baseurl))

    #clean variables and variable names
      df_combo <- df_combo %>%
        dplyr::rename(fy = Period, mech_name = `Funding Mechanism`, fundingagency = `Funding Agency`,
                      #primepartner = `Implementing Partner`,
                      agecoarse = `Age: <15/15+  (Coarse)`,
                      sex = `Cascade sex`, indicator = `Technical Area`, type = `Targets / Results`) %>%
        dplyr::select(-dplyr::matches("Disaggregation Type", "Type of organisational unit")) %>%
        tibble::add_column(mech_code = as.character(NA), .before = "mech_name") %>%
        tidyr::separate(mech_name, c(NA, "mech_code", "mech_name"), sep = " - ", extra = "merge") %>%
        dplyr::mutate(agecoarse = stringr::str_remove(agecoarse, " \\(Inclusive\\)"),
                      sex = stringr::str_remove(sex, "s$"),
                      psnu = stringr::str_trim(psnu),
                      type = stringr::str_replace(type, " ", "_") %>% tolower) %>%
        dplyr::group_by_if(is.character) %>%
        dplyr::summarise(Value = sum(Value, na.rm = TRUE)) %>%
        dplyr::ungroup() %>%
        tidyr::pivot_wider(names_from = type,
                           values_from = Value)

      if(any(stringr::str_detect(unique(df_combo$fy), "Oct [:digit:]{4}"))){
        df_combo <- dplyr::mutate(df_combo, fy = fy %>% stringr::str_sub(-4) %>% as.integer)
      } else {
        df_combo <- df_combo %>%
          dplyr::rename(Period = fy) %>%
          glamr::convert_datim_pd_to_qtr() %>%
          dplyr::rename(period = Period) %>%
          dplyr::mutate(fy = glue::glue("20{stringr::str_sub(period, 3, 4)}") %>% as.integer(),
                        .before = period)
      }

      if(var_exists(df_combo, "mer_targets"))
        df_combo <- dplyr::mutate(df_combo, psnu = ifelse(is.na(psnu) & mer_targets > 0, orgunit, psnu))

    # Export
    if(!is.null(folderpath_output) & fs::dir_exists(folderpath_output)){

      cat("\nExporting ...\n")

      #compile file name  and export data
      filename <- paste(
        fy_pd,
        ou_name,
        "MER Data",
        glamr::curr_date(),
        sep = "_"
      ) %>%
        paste0(".csv") %>%
        stringr::str_replace_all("_{2,}", "_")

      readr::write_csv(x = df,
                       file = file.path(folderpath_output, filename),
                       na = "")

      cat(crayon::blue("\n", file.path(folderpath_output, filename), "\n"))

    }

    invisible(df_combo)

    } else {
      invisible(NULL)
    }
  }
