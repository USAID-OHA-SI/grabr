#' PEPFAR Panorama Base URL
#' @name PEPFAR Panorama base url
#'
pano_url = "https://pepfar-panorama.org"

#' Check internet connection
#'
#' @return A boolean or stops
#' @keywords internal

check_internet <- function(){

  # Assuming google is always up
  host <- "google.com"

  #if(!curl::has_internet())
  if(is.null(curl::nslookup(host, error = FALSE)))
    stop("No internet connection. Cannot excecute API.")
}


#' Check if package exists
#'
#' @param pkg package name
#'
#' @return warning message if package is not installed
#' @export
#' @family utility

package_check <- function(pkg){
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(paste("Package", pkg,
               "needed for this function to work. Please install it."),
         call. = FALSE)
  }
}

#' @title Get base url from a link
#'
#' @param url DATIM API end points
#'
#' @return Base url without trailing slash
#' @export
#' @family utility

get_baseurl <- function(url) {

  if (missing(url) | is.null(url))
    url <- "https://final.datim.org"

  # Split url into subsections
  url_parts <- urltools::url_parse(url)

  # Combine scheme and domain
  base_url <- ifelse(is.na(url_parts$scheme), "https", url_parts$scheme)

  base_url <- base_url %>% paste0("://", url_parts$domain)

  # Add port if nay
  if (!is.na(url_parts$port)) {
    base_url <- base_url %>%
      paste0(":", url_parts$port)
  }

  # Remove any trailing slashes
  base_url %>% stringr::str_remove("\\/*$|\\*$")
}

#' Check if variable exist
#'
#' @param df data frame to check against
#' @param var quoted variable of interest
#' @export
#' @family utility
#' @examples
#' \dontrun{
#' var_exists(df, "val") }
var_exists <- function(df, var) {

  var %in% names(df)

}

#' Lazy checking/loading of credentials
#'
#' This function is useful within another function. It check whether a username
#' or password has been provided by the user and, if not, checks if they were
#' stored via `glamr` or prompts user to provide credentials through interactive
#' prompt.
#'
#' @param service account, either datim", "pano", or "s3"
#' @param username account username or s3 access key
#' @param password account password or s3 secret key
#'
#' @return returns a list of 2 - username/access and password/secret
#' @export
#' @keywords internal
#' @examples
#' \dontrun{
#'   accnt <- lazy_secrets("datim", username = username, password = password)
#'   datim_dimensions(accnt$username, accnt$password)
#' }
lazy_secrets <- function(service = c("datim", "pano", "s3"),
                         username, password){

  check_internet()

  #ensure only one is picked
  service <- match.arg(service)

  #use stored secrets created under glamr if available (and nothing provided)
  if(service != "s3" && missing(username) && glamr::is_stored(service)){
    username <- keyring::key_list(service)[1, 2]
    password <- keyring::key_get(service, username)
  }

  #if s3, use stored secrets created under glamr if available (and nothing provided)
  if(service == "s3" && missing(username) && glamr::is_stored(service)){
    username <- glamr::get_s3key("access")
    password <- glamr::get_s3key("secret")
  }

  #if no username, prompt (UI) for username
  if(missing(username) && !glamr::is_stored(service))
    username <- getPass::getPass(
      glue::glue("Provide {service} {ifelse(service == 's3', 'access key','username')}")
      )

  #if no username, prompt (UI) for username
  if(missing(password) && !glamr::is_stored(service))
    password <- getPass::getPass(
      glue::glue("Provide {service} {ifelse(service == 's3', 'secret key','password')}")
    )

  #if s3, change names from user/pass to access/secret key
  if(service != "s3"){
    accnt_info <- list(username = username,
                       password = password)
  } else {
    accnt_info <- list(access_key = username,
                       secret_key = password)
  }


  return(invisible(accnt_info))

}
