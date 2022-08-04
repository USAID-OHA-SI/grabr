#' Check internet connection
#'
#' @return A boolean or stops
#' @keywords internal

check_internet <- function(){

  if(!curl::has_internet())
    stop("No internet connection. Cannot excecute API.")
}


#' Test status of query
#'
#' @param res response
#'
#' @return A boolean or stops
#' @keywords internal

check_status <- function(res){

  if(httr::status_code(res) == 200)
    stop("The API returned an error")
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


#' Test if service is stored in credential manager
#'
#' @param service account, either "email", "datim", "pano", or "s3"
#'
#' @export
#' @return A boolean

is_stored <- function(service = c("datim", "email", "pano", "s3")){

  package_check('keyring')

  accounts <- keyring::key_list()$service

  {{service}} %in% accounts

}

#' Test if option variable is loaded in current session
#'
#' @param opt_var option variable ("datim", "email", "path_msd", "path_datim", "path_vector", "path_raster")
#'
#' @return A boolean
#' @keywords internal

is_loaded <- function(opt_var){

  !is.null(getOption({{opt_var}}))

}


#' Test if package is installed locally
#'
#' @param package name of R package to check
#'
#' @return A boolean
#' @keywords internal

is_installed <- function(package){
  {{package}} %in% rownames(installed.packages())
}


