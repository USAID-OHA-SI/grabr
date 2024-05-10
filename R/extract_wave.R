#' Pull Genie Data from PDAP Wave
#'
#' This function provides streamlined API access to PDAP Wave data, the
#' successor to DATIM Genie. PDAP Wave API simplifies the requests that
#' previously needed to be made with DATIM and returns a dataset back that
#' matches the MSD structure. Further documentation can be found at
#' \url{https://wave.test.pdap.pepfar.net/api/docs#/}.
#'
#' Users must pass their query filter in a list form into \code{request_body},
#' which matches what you would manually do in Genie previously. You can proceed
#' with either POST or GET requests to access PSNUxIM, OUxIM, and SitexIM data.
#'
#' This function was adapted from code developed and shared by Derek Wood
#' (GHSD/PRIME).
#'
#' @param request_body elements to pass into the PDAP Wave POST API
#' @param folderpath_dwnld where to download, default = "Data"
#' @param psd_type Type of PEPFAR Structured dataset: "psnu_im" (default),
#'   "ou_im", or "site_im"
#' @param request_type API request type: "POST" (default) or "GET
#' @param username DATIM username, if blank looks for stored credentials
#'   (\code{glamr::set_datim()}) and then prompts for credentials if not found
#' @param password DATIM password, if blank looks for stored credentials
#'   (\code{glamr::set_datim()}) and then prompts for credentials if not found
#'
#' @return list of request and stored data in zip
#' @export
#'
#' @examples
#' \dontrun{
#'  library(tidyverse)
#'  library(glamr)
#'
#'  #get country uid for API
#' cntry_uid <- pepfar_country_list %>%
#'  filter(country == "Tanzania") %>%
#'  pull(country_uid)
#'
#'  #establish parameters to pass into POST API
#'  post_body <- list(
#'    daily_frozen='daily',
#'    fiscal_year=list(2023, 2024),
#'    funding_agency = list("USAID"),
#'    indicator=list("TX_CURR","TX_ML","TX_CURR_LAG2", "TX_NET_NEW","TX_NEW",
#'                   "TX_RTT","PMTCT_STAT", "PMTCT_STAT_POS", "PMTCT_ART"),
#'    uid_hierarchy_list=list(str_glue('-|-|{cntry_uid}')))
#'
#'  #run POST API
#'  wave_process_query(post_body)
#'
#'  #load data
#'  df_wave <- return_latest("Data") %>%
#'     read_psd()
#'     }

wave_process_query <- function(request_body,
                               folderpath_dwnld = "Data",
                               psd_type = c("psnu_im", "ou_im", "site_im"),
                               request_type = c("POST", "GET"),
                               username,
                               password){

  #establish session
  sess_token <- est_session(username, password)

  #ensure only one request_type
  request_type <- request_type[1]
  request_accepted <-  c("POST", "GET")
  if(!request_type %in% request_accepted)
    cli::cli_abort("The provided {.code request_type} is not one of the accepted inputs: {.code {request_accepted}}")

  #ensure only one psd_type
  psd_type <- psd_type[1]
  psd_accepted <-  c("psnu_im", "ou_im", "site_im")
  if(!psd_type %in%  psd_accepted)
    cli::cli_abort("The provided {.code psd_type} is not one of the accepted inputs: {.code {psd_accepted}}")

  #export path
  zip_path <- est_export_path(post_body, folderpath_dwnld, psd_type)

  #submit request & store data locally
  if(request_type == "POST"){
    #DATIM POST request
    httr::POST(
      stringr::str_glue('https://{api_host}/api/data/{psd_type}'),
      httr::set_cookies(wave_session = api_login_result$cookies$value),
      encode='json',
      body={post_body},
      httr::write_disk(zip_path, overwrite=TRUE)
    )
  } else {
    #DATIM GET request
    httr::GET(
      stringr::str_glue('https://{api_host}/api/data/{psd_type}'),
      httr::set_cookies(wave_session = api_login_result$cookies$value),
      query=flattenbody({post_body}),
      httr::write_disk(zip_path, overwrite=TRUE)
    )
  }


  #notification
  cli::cli_alert_info("The PDAP Wave successfully executed and the output is saved as {.file {zip_path}}")

  invisible()


}

#' Establish PDAP Wave Session
#'
#' @inheritParams wave_process_query
#' @param datim_hostname DATIM URL: 'genie.testing.datim.org'
#' @param api_host API address: 'wave.test.pdap.pepfar.net'
#'
#' @return session token
#' @keywords internal

est_session <- function(username,
                        password,
                        datim_hostname = 'genie.testing.datim.org',
                        api_host = 'wave.test.pdap.pepfar.net'){

  #store/prompt for credentials
  accnt <- grabr::lazy_secrets("datim", username, password)

  #establish DATIM session
  datim_results <- httr::GET(
    stringr::str_glue('https://{datim_hostname}/api/me'),
    httr::authenticate(accnt$username, accnt$password, type = "basic")
  )

  #establish DATIM API session
  api_login_result <- httr::GET(
    stringr::str_glue('https://{api_host}/api/Authenticate/datim'),
    httr::set_cookies(JSESSIONID = datim_results$cookies$value),
    query=list(datim_hostname=datim_hostname)
  )

  session_token <- api_login_result$cookies$value

  return(session_token)
}


#' Establish Export Path
#'
#' @inheritParams wave_process_query
#'
#' @return vector of the filename
#' @keywords internal
#'
est_export_path <- function(post_body, folderpath_dwnld, psd_type){

  #string type
  psd_type_string <- stringr::str_extract(psd_type, "^.*(?=_)") %>% toupper()

  #country
  cntry <- extract_cntry(post_body)

  #string daily_frozen
  daily_frozen <- stringr::str_to_sentence(post_body$daily_frozen)

  #output path
  zip_path <- file.path(folderpath_dwnld,
                        stringr::str_glue('PDAPWave-{psd_type_string}ByIMs-{cntry}-{daily_frozen}-{Sys.Date()}.zip'))

  return(zip_path)
}

#' Extract Country Name
#'
#' @inheritParams wave_process_query
#'
#' @return vector country's name
#' @keywords internal
#'
extract_cntry <- function(request_body){

  #country uid
  uid <- stringr::str_sub(post_body$uid_hierarchy_list[[1]], start = -11)

  if(is.null(cntry_uid))
    cli::cli_abort("Missing country UID in {.field request_body}, which should be structured like {.code uid_hierarchy_list=list(stringr::str_glue('-|-|cntry_uid')}")

  #country name
  name <- glamr::pepfar_country_list %>%
    dplyr::filter(country_uid == uid) %>%
    dplyr::pull(country) %>%
    stringr::str_remove_all("( |')")

  if(length(name) == 0)
    cli::cli_abort("Check the OU UID in {.field request_body} to ensure it is valid")

  return(name)
}


#' Flatten Body
#'
#' A query can only have one value per name, so take
#' any values that contain vectors length >1 and
#' split them up. For example list(x=1:2, y="a") becomes list(x=1, x=2, y="a").
#' This code was developed and shard by Derek Wood (GHSD/PRIME)
#'
#' @param x list
#' @keywords internal
flattenbody <- function(x) {

  if (all(lengths(x)<=1)) return(x);
  do.call("c", mapply(function(name, val) {
    if (length(val)==1 || any(c("form_file", "form_data") %in% class(val))) {
      x <- list(val)
      names(x) <- name
      x
    } else {
      x <- as.list(val)
      names(x) <- rep(name, length(val))
      x
    }
  }, names(x), x, USE.NAMES = FALSE, SIMPLIFY = FALSE))
}
