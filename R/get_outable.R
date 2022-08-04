#' Pull Table of OUs/Countries, UIDs, ISO codes and levels
#'
#' `get_outable` pulls from DATIM to return a dataframe with all PEPFAR
#' Operating Units and countries along with useful information for merging,
#' eg ISO codes, and use in DATIM APIs, eg UIDs and hierarchy levels.
#'
#' `get_outtable` is a wrapper around `identify_ouuids` and `identify_levels`
#' that pulls this information directly from DATIM. The user will need to have
#' a DATIM account to access this data. You can take advantage of storing you
#' credentials locally in a secure way using `set_datim`.
#'
#' @param username DATIM Username, defaults to using `datim_user()` if blank
#' @param password DATIM password, defaults to using `datim_pwd()` if blank
#' @param baseurl base url for the API, default = https://final.datim.org/
#'
#' @export
#' @return data frame with all PEPFAR OUs, countries, their UIDs, ISO codes
#'   and different levels in the DATIM hierarchy
#' @seealso [set_datim()] to store DATIM authentication;
#'   [load_secrets()] to load credentials into session
#' @examples
#' \dontrun{
#'  load_secrets()
#'  ou_table <- datim_outable() }

get_outable <- function(username, password, baseurl = "https://final.datim.org/"){

  df_uid <- identify_ouuids(username, password, baseurl)

  df_levels <- identify_levels(username, password, baseurl)

  df_outable <- df_uid %>%
    dplyr::rename(operatingunit_uid = uid) %>%
    dplyr::select(-type) %>%
    dplyr::left_join(df_levels, ., by = c("operatingunit" = "country"))

  df_outable <- df_uid %>%
    dplyr::rename(country_uid = uid) %>%
    dplyr::select(-type) %>%
    dplyr::left_join(df_outable, ., by = c("country"))

  df_outable <- df_outable %>%
    dplyr::select(operatingunit, operatingunit_uid, operatingunit_iso,
                  country, country_uid, country_iso,
                  dplyr::everything()) %>%
    dplyr::arrange(operatingunit, country)

  return(df_outable)
}

#' Pull OU UIDS
#'
#' `identify_ouuids` pulls from DATIM to return a dataframe with all PEPFAR
#' Operating Units and countries and their UIDs. This is one of two components
#' that feeds into `get_outable`.
#'
#' To access the UIDs, the user will need to have a DATIM account. You can
#' take advantage of storing you credentials locally in a secure way
#' using `set_datim`.
#'
#' @param username DATIM Username, defaults to using `datim_user()` if blank
#' @param password DATIM password, defaults to using `datim_pwd()` if blank
#' @param baseurl base url for the API, default = https://final.datim.org/
#'
#' @export
#' @return Datim country names
#' @seealso [set_datim()] to store DATIM authentication;
#'   [load_secrets()] to load credentials into session
#'
#' @examples
#' \dontrun{
#'  load_secrets()
#'  ous <- identify_ouuids() }

identify_ouuids <- function(username, password,
                            baseurl = "https://final.datim.org/"){

  check_internet()

  if(missing(username))
    username <- datim_user()

  if(missing(password))
    password <- datim_pwd()

  ous <- baseurl %>%
    paste0("api/organisationUnits?filter=level:eq:3") %>%
    httr::GET(httr::authenticate(username,password)) %>%
    httr::content("text") %>%
    jsonlite::fromJSON(flatten=TRUE) %>%
    purrr::pluck("organisationUnits")

  region_uids <- ous %>%
    dplyr::filter(stringr::str_detect(displayName, "Region")) %>%
    dplyr::pull(id)

  ctrys <- purrr::map_dfr(.x = region_uids,
                          .f = ~ baseurl %>%
                            paste0("api/organisationUnits?filter=level:eq:4&filter=path:like:", .x) %>%
                            httr::GET(httr::authenticate(username,password)) %>%
                            httr::content("text") %>%
                            jsonlite::fromJSON(flatten=TRUE) %>%
                            purrr::pluck("organisationUnits") %>%
                            dplyr::mutate(regional = TRUE))


  uids <- ous %>%
    dplyr::bind_rows(ctrys) %>%
    dplyr::arrange(displayName)

  uids <- uids %>%
    dplyr::rename(uid = id,
                  country = displayName) %>%
    dplyr::mutate(type = ifelse(is.na(regional), "OU", "Country")) %>%
    dplyr::select(-regional)

  return(uids)
}


#' Identify Facility/Community levels in org hierarchy
#'
#' `identify_levels` pulls from DATIM to return a dataframe with all PEPFAR
#' Operating Units and countries with their ISO codes and hierarhcy levels.
#' This is one of two components that feeds into `get_outable`.
#'
#' To access the UIDs, the user will need to have a DATIM account. You can
#' take advantage of storing you credentials locally in a secure way
#' using `set_datim`.
#'
#' @param username DATIM Username, defaults to using `datim_user()` if blank
#' @param password DATIM password, defaults to using `datim_pwd()` if blank
#' @param baseurl base url for the API, default = https://final.datim.org/
#'
#' @export
#' @seealso [set_datim()] to store DATIM authentication;
#'   [load_secrets()] to load credentials into session
#' @examples
#' \dontrun{
#'  #table for all OUs
#'   load_secrets()
#'   identify_levels() }

identify_levels <- function(username, password,
                            baseurl = "https://final.datim.org/"){

  check_internet()

  if(missing(username))
    username <- datim_user()

  if(missing(password))
    password <- datim_pwd()

  df_levels <- baseurl %>%
    paste0(.,"api/dataStore/dataSetAssignments/orgUnitLevels") %>%
    httr::GET(httr::authenticate(username,password)) %>%
    httr::content("text") %>%
    jsonlite::fromJSON(flatten=TRUE) %>%
    purrr::map_dfr(dplyr::bind_rows) %>%
    dplyr::mutate_if(is.character, ~ dplyr::na_if(., ""))

  #adjust for regional missions
  df_levels <- df_levels %>%
    dplyr::mutate(name4 = ifelse(is.na(name4), name3, name4),
                  iso4 = ifelse(is.na(iso4), iso3, iso4))

  #rename
  df_levels <- df_levels %>%
    dplyr::rename(operatingunit = name3,
                  operatingunit_iso = iso3,
                  country_iso = iso4,
                  psnu = prioritization) %>%
    dplyr::rename_with(.cols= where(is.integer), ~ paste0(., "_lvl")) %>%
    dplyr::select(dplyr::everything(), country_lvl, psnu_lvl,
                  community_lvl, facility_lvl) %>%
    dplyr::rename(country = name4)

  return(df_levels)
}


#' @title Identify Reporting Period from MSD File
#'
#' @param msd_file MSD File, should include publication date
#' @param clean    Should be period be clean? default is true
#'
#' @return reporting period
#' @export
#'
#' @examples
#' \dontrun{
#'
#'  library(glamr)
#'
#'  pd = return_latest(si_path(), "OU_IM") %>% identify_pd()
#' }
#'
identify_pd <- function(msd_file, clean = TRUE) {

  # Extract release date from msd file
  release_date <- msd_file %>%
    stringr::str_extract("\\d{8}") %>%
    lubridate::ymd() %>%
    base::as.character()

  # Pepfar data calendar
  release_dates <- pepfar_data_calendar %>%
    dplyr::pull(entry_close)

  # Check validity
  if (!release_date %in% release_dates) {
    base::message(crayon::red("Invalid and / or non-pepfar release date"))
    return(NULL)
  }

  # Identify reporting period
  pd <- pepfar_data_calendar %>%
    rowwise() %>%
    mutate(period = base::paste0("FY", fiscal_year, "Q", quarter)) %>%
    ungroup() %>%
    filter(entry_close == release_date) %>%
    pull(period)

  # Reformat period
  if (clean) {
    pd <- pd %>%
      stringr::str_sub(start = 3, end = 4) %>%
      stringr::str_replace(string = pd, pattern = ., replacement = "") %>%
      stringr::str_to_upper()
  }
  else {
    pd <- pd %>%
      stringr::str_replace("Q", "qtr") %>%
      stringr::str_to_lower()
  }

  return(pd)
}


#' @title Get Org UIDS
#' @note Use with caution. Use `get_ouorguids()` for levels below 3
#'
#' @param level    Org level
#' @param username DATIM Username, recommend using `datim_user()`
#' @param password DATIM password, recommend using `datim_pwd()`
#' @param baseurl base url for the API, default = https://final.datim.org/
#'
#' @return ORG UIDS as tibble
#' @export
#'
#' @examples
#' \dontrun{
#'  library(glamr)
#'
#'  # All orgunit level 3 uids + names
#'  orgs <- get_orguids(level = 3)
#' }
#'
get_orguids <-
  function(level = 3,
           username = NULL,
           password = NULL,
           baseurl = "https://final.datim.org/"){

    # Params
    lvl <- {{level}}

    user <- base::ifelse(base::is.null({{username}}),
                         glamr::datim_user(), {{username}})

    pass <- base::ifelse(base::is.null({{password}}),
                         glamr::datim_pwd(), {{password}})

    # Query ou
    orgs <- baseurl %>%
      paste0("api/organisationUnits",
             "?filter=level:eq:", lvl) %>%
      httr::GET(httr::authenticate(user, pass)) %>%
      httr::content("text") %>%
      jsonlite::fromJSON(flatten = TRUE) %>%
      purrr::pluck("organisationUnits") %>%
      dplyr::rename(uid = id, orgunit = displayName) %>%
      tibble::as_tibble()

    return(orgs)
  }


#' @title Get list of OU Orgs at specific level
#' @note  Use `get_orguids()` for levels above 4
#'
#' @param ouuid    OU uid
#' @param level    org level
#' @param username DATIM Username
#' @param password DATIM password, recommend using `mypwd()`
#' @param baseurl  base url for the API, default = https://final.datim.org/
#'
#' @return ORG UIDS as tibble
#' @export
#'
#' @examples
#' \dontrun{
#'
#'  library(glamr)
#'
#'  cntry <- "Zambia"
#'
#'  uid <- get_ouuid(cntry)
#'
#'  lvl <- get_ouorglevel(cntry, org_type = "prioritization")
#'
#'  orgs <- get_ouorgs(ouuid = uid, level = lvl)
#' }
#'
get_ouorgs <-
  function(ouuid,
           level = 4,
           username = NULL,
           password = NULL,
           baseurl = "https://final.datim.org/"){

    package_check('httr')
    package_check('jsonlite')

    # Params
    uid <- {{ouuid}}

    lvl <- {{level}}

    user <- base::ifelse(base::is.null({{username}}),
                         glamr::datim_user(), {{username}})

    pass <- base::ifelse(base::is.null({{password}}),
                         glamr::datim_pwd(), {{password}})

    # Query ou
    orgs <- baseurl %>%
      paste0("api/organisationUnits",
             "?filter=level:eq:", lvl,
             "&filter=path:like:", uid,
             "&paging=false&format=json") %>%
      httr::GET(httr::authenticate(user, pass)) %>%
      httr::content("text") %>%
      jsonlite::fromJSON(flatten = TRUE) %>%
      purrr::pluck("organisationUnits")

    # Check data
    if (base::is.null(orgs)) {
      base::cat(
        crayon::red(
          paste0("\nNo orgunits found for uid = ",
                 uid, " & level = ", lvl, "\n")))

      return(NULL)
    }

    # Clean up
    orgs <- orgs %>%
      dplyr::rename(uid = id, orgunit = displayName) %>%
      tibble::as_tibble()

    return(orgs)
  }


#' @title Get OU Org UIDS
#'
#' @param add_details Add countries for regional ou, default is false
#' @param username    DATIM Username, recommend using `datim_user()`
#' @param password    DATIM password, recommend using `datim_pwd()`
#' @param baseurl     base url for the API, default = https://final.datim.org/
#'
#' @return OU UIDS as tibble
#' @export
#'
#' @examples
#' \dontrun{
#'  library(glamr)
#'
#'  # OU Org UIDs
#'  ous <- get_ouuids()
#' }
#'
get_ouuids <-
  function(add_details = FALSE,
           username = NULL,
           password = NULL,
           baseurl = "https://final.datim.org/"){


    # Params
    user <- base::ifelse(base::is.null({{username}}),
                         glamr::datim_user(), {{username}})

    pass <- base::ifelse(base::is.null({{password}}),
                         glamr::datim_pwd(), {{password}})

    # Query ou
    ous <- get_orguids(level = 3,
                       username = user,
                       password = pass,
                       baseurl = baseurl) %>%
      dplyr::rename(operatingunit = orgunit)

    # Add details if needed
    if (add_details == TRUE) {

      # Query R OUs / Countries
      countries <- ous %>%
        dplyr::filter(stringr::str_detect(operatingunit, " Region$")) %>%
        base::split(1:base::nrow(.)) %>%
        purrr::map_dfr(function(obj){

          cntries <- get_ouorgs(obj$uid, 4) %>%
            dplyr::rename(country = orgunit) %>%
            dplyr::mutate(operatingunit = obj$operatingunit) %>%
            dplyr::relocate(operatingunit, .after = 1)

          return(cntries)
        })

      # Combine
      ous <- ous %>%
        dplyr::mutate(
          country = dplyr::case_when(
            stringr::str_detect(operatingunit, " Region$") == TRUE ~ NA_character_,
            TRUE ~ operatingunit)) %>%
        dplyr::bind_rows(countries) %>%
        dplyr::arrange(operatingunit, country)
    }

    return(ous)
  }


#' @title Get Operatingunit / Country Org UID
#'
#' @param operatingunit Operatingunit name
#' @param username      Datim Account username, recommend using `datim_user()`
#' @param password      Datim Account Password, recommend using `datim_pwd()`
#'
#' @return uid
#' @export
#'
#' @examples
#' \dontrun{
#'   library(glamr)
#'
#'   # get orgunit for specific OU/Country: kenya
#'   get_ouuid(operatingunit = "Kenya")
#' }
#'
get_ouuid <-
  function(operatingunit,
           username = NULL,
           password = NULL,
           baseurl = "https://final.datim.org/") {

    # Params
    ou <- stringr::str_to_upper({{operatingunit}})

    user <- base::ifelse(base::is.null(username),
                         glamr::datim_user(),
                         {{username}})

    pass <- base::ifelse(base::is.null(password),
                         glamr::datim_pwd(),
                         {{password}})

    # Get all ou uids
    ous <- get_ouuids(
      add_details = TRUE,
      username = user,
      password = pass,
      baseurl = baseurl) %>%
      dplyr::filter(
        stringr::str_to_upper(operatingunit) == ou |
          stringr::str_to_upper(country) == ou)


    if (base::nrow(ous) == 0) {
      base::cat("\nInvalid PEPFAR Operatingunit / Country: ",
                crayon::red(ou, "\n"))

      return(NULL)
    }

    # OU/Country uid
    if (stringr::str_detect(ou, " region")) {
      ous <- ous %>% dplyr::filter(is.na(country))
    }

    # Get uid
    ouuid <- ous %>%
      dplyr::pull(uid) %>%
      dplyr::first()

    return(ouuid)
  }


#' @title Get all orgunits levels in org hierarchy
#' @note  Similar to `glamr::identify_levels()` and `glamr::get_outable()`
#'
#' @param username DATIM username, recommed using `datim_user()`
#' @param password DATIM password, recommend using `datim_pwd()`
#' @param baseurl  base API url, default = https://final.datim.org/
#'
#' @return df
#' @export
#'
#' @examples
#' \dontrun{
#'   library(glamr)
#'
#'   # Get PEPFAR Org Levels
#'   get_levels()
#'  }
#'
get_levels <-
  function(username = NULL,
           password = NULL,
           baseurl = "https://final.datim.org/"){

    # Params
    user <- base::ifelse(base::is.null(username),
                         glamr::datim_user(),
                         {{username}})

    pass <- base::ifelse(base::is.null(password),
                         glamr::datim_pwd(),
                         {{password}})

    # Query data
    df_levels <- baseurl %>%
      paste0(.,"api/dataStore/dataSetAssignments/orgUnitLevels") %>%
      httr::GET(httr::authenticate(user, pass)) %>%
      httr::content("text") %>%
      jsonlite::fromJSON(flatten = TRUE) %>%
      purrr::map_dfr(dplyr::bind_rows) %>%
      dplyr::mutate_if(is.character, ~ dplyr::na_if(., ""))

    # Adjust for non-regional missions
    df_levels <- df_levels %>%
      dplyr::mutate(name4 = ifelse(is.na(name4), name3, name4),
                    iso4 = ifelse(is.na(iso4), iso3, iso4))

    # rename
    df_levels <- df_levels %>%
      dplyr::rename(operatingunit = name3,
                    countryname = name4,
                    operatingunit_iso = iso3,
                    country_iso = iso4)

    return(df_levels)
  }


#' Get OU Org level
#'
#' @param operatingunit Operatingunit name
#' @param country       Country name (default = Operatingunit)
#' @param org_type      Orgunit type (country_lvl, prioritization, community, facility_lvl)
#' @param username      Datim Account username
#' @param password      Datim Account Password
#' @param base_url      Datim Base URL
#'
#' @return uid
#' @export
#'
#' @examples
#' \dontrun{
#'  library(glamr)
#'
#'  cntry <- "Zambia"
#'
#'  # Get country org level
#'  get_ouorglevel(cntry)
#'
#'  # Get community org level
#'  get_ouorglevel(cntry, org_type = "community")
#' }
#'
get_ouorglevel <-
  function(operatingunit,
           country = NULL,
           org_type = "prioritization",
           username = NULL,
           password = NULL,
           baseurl = "https://final.datim.org/") {

    # params
    ou = {{operatingunit}}

    cntry <- base::ifelse(base::is.null(country), ou, {{country}})

    type <- {{org_type}}

    user <- base::ifelse(base::is.null(username),
                         glamr::datim_user(),
                         {{username}})

    pass <- base::ifelse(base::is.null(password),
                         glamr::datim_pwd(),
                         {{password}})

    # Levels
    df_lvls <- get_levels(user, pass, baseurl)

    # level name
    if (!stringr::str_to_lower(type) %in% base::names(df_lvls)) {
      base::cat(base::paste0("\nOrg_type is not available: ",
                             crayon::red(type), "\n"))

      return(NULL)
    }

    # filter ou/country
    df_lvls <- df_lvls %>%
      dplyr::filter(operatingunit == ou,
                    countryname == cntry)

    # records
    if (nrow(df_lvls) == 0) {
      base::cat(crayon::red("\nThere is no match for ou/country options\n"))

      return(NULL)
    }

    # Level
    lvl <- df_lvls %>% dplyr::pull(type)

    return(lvl)
  }


#' @title Identify OU/Org Label
#'
#' @param operatingunit  Operating unit
#' @param country        Country name
#' @param org_level      OU Org level, default is set to 4, PSNU
#' @param username       Datim account username
#' @param password       Datim account password
#' @param baseurl        Datim base url
#'
#' @return Org level label
#' @export
#'
#' @examples
#' \dontrun{
#'   library(glamr)
#'
#'   get_ouorglabel(operatingunit = "Zambia", org_level = 5)
#' }
#'
get_ouorglabel <- function(operatingunit,
                           country = NULL,
                           org_level = 4,
                           username = NULL,
                           password = NULL,
                           baseurl = "https://final.datim.org/") {
  # Label
  lbl <- NULL

  if (org_level <= 3) {
    lbl <- case_when(
      org_level == 3 ~ "country",
      org_level == 2 ~ "region",
      org_level == 1 ~ "global",
      TRUE ~ NA_character_
    )

    return(lbl)
  }

  # Country
  if (base::is.null(country)) {
    country <- operatingunit
  }

  # Levels
  df_lvls <- get_levels(username, password, baseurl) %>%
    tidyr::pivot_longer(country:tidyselect::last_col(),
                 names_to = "label",
                 values_to = "level")

  df_lvls %<>%
    dplyr::filter(operatingunit == operatingunit,
           countryname == country,
           level == org_level)

  if (base::is.null(df_lvls) | base::nrow(df_lvls) == 0) {
    return(glue::glue("orglvl_{org_level}"))
  }

  lbl <- df_lvls %>%
    dplyr::pull(label) %>%
    base::sort() %>%
    last()

  return(lbl)
}


#' Get Orgs uids by level
#'
#' @param ouuid        Operatingunit uid
#' @param level        Orgunit level
#' @param username     Datim Account username
#' @param password     Datim Account Password
#' @param baseurl      Datim base url
#'
#' @return             list of uids
#' @export
#'
#' @examples
#' \dontrun{
#'  library(glamr)
#'
#'  # Set country of interest
#'  cntry <- "Zambia"
#'
#'  # Get OU/Country orgunit uid
#'  uid <- get_ouuid(cntry)
#'
#'  # Get org level for psnu
#'  lvl <- get_ouorglevel(cntry, org_type = "prioritization")
#'
#'  # Retreived all uids for level 4 (SNU1)
#'  get_ouorguids(ouuid = uid, level = 4)
#' }
#'
get_ouorguids <-
  function(ouuid, level,
           username = NULL,
           password = NULL,
           baseurl = "https://final.datim.org/") {

    # params
    uid <- {{ouuid}}

    lvl <- {{level}}

    user <- base::ifelse(base::is.null(username),
                         glamr::datim_user(),
                         {{username}})

    pass <- base::ifelse(base::is.null(password),
                         glamr::datim_pwd(),
                         {{password}})

    # Query orgunits
    orgs <- get_ouorgs(ouuid = uid,
                       level = lvl,
                       username = user,
                       password = pass,
                       baseurl = baseurl)

    # Check data
    if (base::is.null(orgs)) {
      base::cat(
        crayon::red(
          paste0("\nNo org uids found\n")))

      return(NULL)
    }

    # extract list of uids
    lvl_uids <- orgs %>% dplyr::pull(uid)

    # return
    return(lvl_uids)
  }






