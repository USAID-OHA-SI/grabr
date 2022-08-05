#' Check internet connection
#'
#' @return A boolean or stops
#' @keywords internal

check_internet <- function(){

  if(!curl::has_internet())
    stop("No internet connection. Cannot excecute API.")
}


#' Check if package exists
#'
#' @param pkg package name
#'
#' @export
#' @keywords internal

package_check <- function(pkg){
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(paste("Package", pkg, "needed for this function to work. Please install it."),
         call. = FALSE)
  }
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

  if(service != "s3" && missing(username) && glamr::is_stored(service)){
    username <- keyring::key_list(service)[1, 2]
    password <- keyring::key_get(service, username)
  }

  if(service == "s3" && missing(username) && glamr::is_stored(service)){
    username <- glamr::get_s3key("access")
    password <- glamr::get_s3key("secret")
  }

  if(missing(username) && !glamr::is_stored(service))
    username <- getPass::getPass(
      glue::glue("Provide {service} {ifelse(service == 's3', 'access key','username')}")
      )

  if(missing(password) && !glamr::is_stored(service))
    password <- getPass::getPass(
      glue::glue("Provide {service} {ifelse(service == 's3', 'secret key','password')}")
    )

  if(service != "s3"){
    accnt_info <- list(username = username,
                       password = password)
  } else {
    accnt_info <- list(access_key = username,
                       secret_key = password)
  }


  return(invisible(accnt_info))

}
