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


