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
#' @param username DEPRECATED - DATIM username, if blank looks for stored credentials
#'   (\code{glamr::set_datim()}) and then prompts for credentials if not found
#' @param password DEPRECATED -DATIM password, if blank looks for stored credentials
#'   (\code{glamr::set_datim()}) and then prompts for credentials if not found
#' @param token session token; by default this is left blank and will provide
#'   prompts to guide you through the process of establishing a session token,
#'   via \code{wave_est_session}
#' @param api_host API address: 'wave.test.pdap.pepfar.net'
#'
#' @return list of request and stored data in zip
#' @export
#' @family wave
#' @importFrom lifecycle deprecated
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
                               username = deprecated(),
                               password = deprecated(),
                               token,
                               api_host = "https://wave.pdap.pepfar.net"){

  #establish session
  if(missing(token))
    session_token <- wave_est_session()

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
  zip_path <- est_export_path(request_body, folderpath_dwnld, psd_type)

  cli::cli_inform("Executing request...")

  #submit request & store data locally
  if(request_type == "POST"){
    #DATIM POST request
    httr::POST(
      stringr::str_glue('{api_host}/api/data/{psd_type}'),
      httr::set_cookies(wave_session = session_token),
      encode = 'json',
      body = request_body,
      httr::write_disk(zip_path, overwrite=TRUE)
    )
  } else {
    #DATIM GET request
    httr::GET(
      stringr::str_glue('{api_host}/api/data/{psd_type}'),
      httr::set_cookies(wave_session = session_token),
      query = flattenbody({request_body}),
      httr::write_disk(zip_path, overwrite=TRUE)
    )
  }


  #notification
  cli::cli_alert_success("The PDAP Wave successfully executed!")
  cli::cli_alert_info("The output saved to {.file {zip_path}}")

  invisible()


}

#' Establish PDAP Wave Session
#'
#' @inheritParams wave_process_query
#' @param timeout time session is valid for, default = 20
#'
#' @return session token
#' @family wave
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
#'  #get a session token
#'  sess_token <- wave_est_session()
#'
#'  #run POST API
#'  wave_process_query(post_body)
#'
#'  #load data
#'  df_wave <- return_latest("Data") %>%
#'     read_psd()
#' }
wave_est_session <- function(timeout = 20,
                             api_host = "https://wave.pdap.pepfar.net"){

  #check if token exists/is value before continuing
  session_token <- wave_check_token()

  #EXIT if credentials are already stored
  if(!is.null(session_token))
    return(session_token)

  #State Okta Url
  okta_url <- 'https://state.okta.com/login/default'

  #provide manual instruction to user
  cli::cli_inform("Instructions:")
  cli::cli_li(c("Log into Dept of State Okta {.url {okta_url}}",
                "Launch the GHSD DATIM App"))

  utils::browseURL(okta_url)

  cli::cli_alert_info("Press [Enter] to continue to the next step.")
  invisible(readline())

  cli::cli_li(c("Navigate to the following PDAP Wave Session Info url {.url https://www.datim.org/pdapsession}",
                "Copy the text from the session info page into the pop up box"))

  utils::browseURL('https://www.datim.org/pdapsession')

  #prompt user for token from url
  session_info <- rstudioapi::askForPassword('Paste PDAP Wave Session text\n https://www.datim.org/pdapsession')

  #FAIL if input was no provided
  if(is.null(session_info))
    cli::cli_abort("No PDAP Wave Session input provided.")

  #response to establish a session token
  auth_result <- httr::POST(glue::glue("{api_host}/api/Authenticate/session"),
                            encode = 'json',
                            body = list(session_buster_token = session_info)
  )

  #catch and return an error
  if (auth_result$status_code != 200) {
    cli::cli_alert_danger("PDAP Wave login failed. {cli::col_red('status_code')} = {auth_result$status_code}")
    cli::cli_alert_info("PDAP Wave message: {httr::content(auth_result, 'text')}")
    cli::cli_abort("Authentication failed.")
  }

  #parse json
  jsonRespParsed <- httr::content(auth_result, as = "parsed")

  #grab session token
  session_token <- jsonRespParsed$token

  #store token in package environment for us
  wave_store_token(session_token)
  cli::cli_par()
  cli::cli_end()
  cli::cli_par()
  cli::cli_alert_success("Success! PDAP Wave Session initiated.")
  cli::cli_end()
  cli::cli_alert_info("Note: PDAP Wave Session token is valid for {timeout} minutes (~ {format(get('wave_token_time', envir = .my_package_env) + timeout * 60, '%I:%M%P')})")


  return(session_token)
}

#' Store token and time to package environment
#'
#' @param token PDAP Wave token
#' @return store token and time to to package environment
#' @keywords internal
#'
wave_store_token <- function(token) {
  assign("wave_token", token, envir = .my_package_env)
  assign("wave_token_time", Sys.time(), envir = .my_package_env)
}


#' Check if PDAP Wave token is already established
#'
#' @inheritParams wave_est_session
#'
#' @return token (if it exists and is valid)
#' @keywords internal
#'
wave_check_token <- function(timeout = 20){

  #EXIT if token is not stored yet
  if (!exists("wave_token", envir = .my_package_env))
    return(NULL)

  #determine session start time
  elapsed_time <- difftime(Sys.time(),
                           get("wave_token_time", envir = .my_package_env),
                           units = "mins")

  if (elapsed_time > timeout) {
      cli::cli_alert_warning("Your PDAP Wave session has expired.
                          Follow the instructions below to reestablish your session.")
      rm(list = c("wave_token", "wave_token_time"), envir = .my_package_env)
      return(NULL)
    }

  #get token if it exists and isn't expired
  session_token <- get("wave_token", envir = .my_package_env)

  return(session_token)
}


#' Establish Export Path
#'
#' @inheritParams wave_process_query
#'
#' @return vector of the filename
#' @keywords internal
#'
est_export_path <- function(request_body, folderpath_dwnld, psd_type){

  #string type
  psd_type_string <- stringr::str_extract(psd_type, "^.*(?=_)") %>% toupper()

  #country
  cntry <- extract_cntry(request_body)

  #string daily_frozen
  daily_frozen <- stringr::str_to_sentence(request_body$daily_frozen)

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
  uid <- stringr::str_extract(request_body$uid_hierarchy_list[[1]], "(?<=\\{).*(?=\\})")

  if(is.null(uid))
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
