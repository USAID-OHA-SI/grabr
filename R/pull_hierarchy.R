#' @title Pull Hierarchy Data from DATIM
#'
#' @description internal funciton to extract orgunit hierarchy
#'
#' @note  Migratated from Wavelength
#'
#' @param ou_uid UID for the country, recommend using identify_ouuids
#' @param username DATIM username, recommend using datim_user
#' @param password DATIM password, recommend using datim_pwd
#' @param add_geom Add geometry column to the output, default is false
#' @param baseurl API base url, default = https://final.datim.org/
#' @keywords internal
#' @examples
#' \dontrun{
#' #get OU UID
#'   ouuid <- identify_ouuids() %>% dplyr::filter(ou == "Kenya")
#' #pull hierarchy (paths are all UIDs)
#'   df <- hierarchy_extract(ouuid, username = myuser, password = datim_pwd(myuser))
#'   }

hierarchy_extract <- function(ou_uid, username, password,
                              add_geom = FALSE,
                              baseurl = "https://final.datim.org/"){

  glamr::package_check("curl")
  glamr::package_check("httr")
  glamr::package_check("jsonlite")

  stopifnot(curl::has_internet())

  # datim credentials
  accnt <- lazy_secrets("datim", username, password)

  # Clean url
  baseurl <- get_baseurl(baseurl)

  #compile url
  url <- baseurl %>%
    paste0("/api/organisationUnits?filter=path:like:", ou_uid,
           "&fields=id,name,path,level")

  # Geometry column is optional
  if (add_geom) {
    url <- paste0(url, ",geometry")
  }

  url <- paste0(url, "&paging=false")

  #pull data from DATIM
  url %>%
    datim_execute_query(
      username = accnt$username,
      password = accnt$password,
      flatten = FALSE
    ) %>%
    purrr::pluck("organisationUnits") %>%
    tibble::as_tibble()
}



#' @title Clean up DATIM Hierarchy Path
#'
#' @description Internal function to Clean Hierarchy
#'
#' @note  Migratated from Wavelength
#' @keywords internal
#'
#' @param df data frame created by hierarchy_extract()
#'

hierarchy_clean <- function(df){

  #create header for each level of the org hierarchy
    levels <- df$path %>%
      stringr::str_count("/") %>%
      max()

    headers <- paste0("orglvl_", seq(1:levels))

  #separate out path into each level of the org hierarchy (UIDs)
    df <- df %>%
      dplyr::mutate(path = stringr::str_remove(path, "^/")) %>%
      tidyr::separate(path, headers, sep = "/", fill = "right") %>%
      dplyr::select(-orglvl_1, -orglvl_2)

  #store uids in order to keep the psnuuid in hierarchy_rename()
    df_uids <- df %>%
      dplyr::select(name, id, dplyr::starts_with("orglvl")) %>%
      dplyr::rename_with(~ stringr::str_replace(., "org","uid"))

  #convert level names of the org hierarchy from UIDs to names
    df_key <- dplyr::select(df, name, id)

    df <- df %>%
      dplyr::mutate_at(dplyr::vars(dplyr::starts_with("orglvl")),
                       ~ plyr::mapvalues(., df_key$id, df_key$name, warn_missing = FALSE))

  #add uids back on
    df <- dplyr::left_join(df, df_uids, by = c("name", "id"))

  #clean up coordinates, removing polygons and separating into lat-long
    if(var_exists(df, "geometry")) {

      #extract coordinates from nested df
        df <- df %>%
          dplyr::mutate(geom_type = geometry$type,
                        coordinates = geometry$coordinates) %>%
          dplyr::select(-geometry)

      #for point data, unnest coordinate from list
        sites <- df %>%
          dplyr::filter((geom_type == "Point" | is.na(geom_type)) & !is.na(coordinates))  %>%
          dplyr::select(-geom_type) %>%
          tidyr::unnest_wider(coordinates, names_sep = "_")

        if(nrow(sites) > 0){
          sites <- sites %>%
            dplyr::rename(longitude = "coordinates_1", latitude = "coordinates_2") %>%
            dplyr::mutate_at(dplyr::vars("longitude", "latitude"), as.double) %>%
            dplyr::select(id, latitude, longitude)

          #bind coordinates onto hierarchy table
          df <- dplyr::left_join(df, sites, by = "id")
        }

      #remove unnecessary columns
        df <- df %>%
          dplyr::select(-geom_type, -coordinates)
    }

  return(df)
}

#' @title Rename Hierarchy
#'
#' @note Rename from Levels to OU/SNU1/PSNU/Facility
#'
#' @description Internal function to Rename Hierarchy
#'
#' @param df        data frame created by hierarchy_extract and hierarchy_clean
#' @param country   county name, eg "Malawi" or "Nepal"
#' @param username  DATIM username, recommend using datim_user
#' @param password  DATIM password, recommend using datim_pwd
#' @param baseurl   API base url, default = https://final.datim.org/
#'
#' @return Cleaned/Renamed data
#' @keywords internal

hierarchy_rename <- function(df, country, username, password,
                             baseurl = "https://final.datim.org/"){

  glamr::package_check("curl")
  glamr::package_check("httr")
  glamr::package_check("jsonlite")

  stopifnot(curl::has_internet())

  # datim credentials
  accnt <- lazy_secrets("datim", username, password)

  # Clean url
  baseurl <- get_baseurl(baseurl)

  if(!country %in% unique(df$orglvl_3))
    df <- dplyr::filter(df, orglvl_4 == country)

  # Get and clean country levels
  df_ou_info <- get_levels(username = accnt$username,
                           password = accnt$password,
                           baseurl = baseurl)

  df_ou_info <- country %>%
    purrr::map_dfr(function(.x) {
      dplyr::filter(df_ou_info, countryname == .x)
    })


  # Rename columns
  if(NROW(df_ou_info) > 0 && df_ou_info$facility > 0){
    #identify levels
    ou_country <- df_ou_info$country
    ou_psnu <- df_ou_info$prioritization
    ou_fac <- df_ou_info$facility
    ou_comm <- df_ou_info$community

    #identify the lowest orglvl in order to add a psnu col for targets (otherwise snu1 & orgunit)
    lowest_lvl <- df %>%
      dplyr::select(dplyr::starts_with("orglvl_")) %>%
      names() %>%
      dplyr::last() %>%
      stringr::str_sub(-1) %>%
      as.numeric()

    #clean up orgunits, keeping just OU, PSNU, Community and Facility
    df <- df %>%
      dplyr::rename_at(dplyr::vars(dplyr::matches("name|Organisation unit")), ~ "orgunit")

    if(var_exists(df, "id"))
      df <- dplyr::rename(df, orgunituid = id)

    if(ou_country == 3) {
      df <- df %>%
        dplyr::mutate(countryname = orglvl_3) %>%
        dplyr::relocate(countryname, .after = "orglvl_3")
    } else {
      df <- df %>%
        dplyr::mutate(countryname = orglvl_4) %>%
        dplyr::relocate(countryname, .after = "orglvl_3")
    }

    if("orglvl_4" %in% names(df)) {
      df <- df %>%
        dplyr::mutate(snu1 = orglvl_4) %>%
        dplyr::relocate(snu1, .after = "countryname")
    }

    if(!!paste0("orglvl_", ou_psnu) %in% names(df))
      df <- dplyr::rename(df, psnu = !!paste0("orglvl_", ou_psnu))

    if(lowest_lvl + 1 == ou_psnu && !"psnu" %in% names(df)) #for targets
      df <- dplyr::mutate(df, psnu = orgunit)

    if(ou_psnu == ou_comm && !!paste0("orglvl_", ou_comm) %in% names(df)){
      df <- df %>%
        dplyr::mutate(community = psnu) %>%
        dplyr::relocate(community, .after = "psnu")
    } else if (!!paste0("orglvl_", ou_comm) %in% names(df)){
      df <- dplyr::rename(df, community = !!paste0("orglvl_", ou_comm))
    }

    if(!!paste0("orglvl_", ou_fac) %in% names(df))
      df <- dplyr::rename(df, facility = !!paste0("orglvl_", ou_fac))

    df <- dplyr::rename(df, operatingunit = orglvl_3)

    #add in psnu uid
    if(!!paste0("uidlvl_", ou_psnu) %in% names(df)){
      df <- df %>%
        dplyr::rename(psnuuid = !!paste0("uidlvl_", ou_psnu)) %>%
        dplyr::relocate(psnuuid, .after = "psnu")
    }

    #reorder and remove unused vars
    df <- df %>%
      dplyr::select(orgunit, orgunituid, dplyr::everything()) %>%
      dplyr::select(-dplyr::starts_with("orglvl_"), -dplyr::starts_with("uidlvl_"))

    return(df)

  } else {
    return(NULL)
  }

}



#' @title Extract country name from OU or country name
#'
#' @param df data frame
#'
#' @return Unique country names
#' @keywords internal

hierarchy_identify_ctry <- function(df){

  #pull orglvl_3 which is out
    country_name <- unique(df$orglvl_3)

  #for regional missions, need to pull country name from orglvl_4
    if(stringr::str_detect(country_name, "Region")) {
      country_name <- unique(df$orglvl_4) %>% setdiff(NA)
    }

  return(country_name)
}


#' @title Extract PEPFAR Org Hierarchy
#'
#' @note This function is migrated from Wavelength and is similar to datim_orgunits
#'
#' @param ou_uid UID for the country, recommend using identify_ouuids
#' @param username DATIM username, recommend using datim_user
#' @param password DATIM password, recommend using datim_pwd
#' @param add_geom Add geometry column to the output, default is false
#' @param baseurl API base url, default = https://final.datim.org/
#' @param folderpath_output provide the full path to the folder for saving
#'
#' @export
#'
#' @examples
#' \dontrun{
#' #get OU UID
#'   ouuid <- identify_ouuids() %>% dplyr::filter(country == "Kenya")
#' #pull hierarchy (paths are all UIDs)
#'   df <- datim_pull_hierarchy(ouuid, username = myuser, password = mypwd(myuser)) }

datim_pull_hierarchy <- function(ou_uid, username, password,
                                 add_geom = FALSE,
                                 baseurl = "https://final.datim.org/",
                                 folderpath_output = NULL){

  #print(ou_uid)
  # datim credentials
  accnt <- lazy_secrets("datim", username, password)

  # Clean url
  baseurl <- get_baseurl(baseurl)

  # Extract Org. Hierarchy
  df <- hierarchy_extract(ou_uid = ou_uid,
                          username = accnt$username,
                          password = accnt$password,
                          add_geom = add_geom,
                          baseurl = baseurl)

  # Clean and rename
  df <- hierarchy_clean(df)

  country_name <- hierarchy_identify_ctry(df)

  df <- purrr::map_dfr(.x = country_name,
                       .f = ~ hierarchy_rename(df, .x,
                                               username = accnt$username,
                                               password = accnt$password,
                                               baseurl = baseurl))

  # Export
  if(!is.null(folderpath_output) && fs::dir_exists(folderpath_output)){

    cat("\nExporting ...\n")

    #compile file name  and export data
    filename <- paste(
        country_name,
        "OrgHierarchy",
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

  return(df)
}
