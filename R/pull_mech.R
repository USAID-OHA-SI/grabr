#' Pull Partner/Mechanism Info from DATIM
#'
#' @param usaid_only specify if only USAID mechansism should be returned, default = TRUE
#' @param ou_sel option to specify an operating unit, default = NULL
#' @param folderpath_output provide the full path to the folder for saving
#'
#' @export
#'
#' @examples
#' \dontrun{
#' #pull mechanism/partner information
#' df <- pull_mech() }

pull_mech <- function(usaid_only = TRUE,
                      ou_sel = NULL,
                      folderpath_output = NULL){

  .Deprecated("datim_mechs")

  package_check("curl")

  stopifnot(curl::has_internet())

  #url for SQL View stored as a csv
    sql_view_url <- "https://www.datim.org/api/sqlViews/fgUtV6e9YIX/data.csv"

  #pull from DATIM
    df <- readr::read_csv(sql_view_url,
                          col_types = readr::cols(.default = "c"))

  #fitler for OU
    if(!is.null(ou_sel))
      df <- dplyr::filter(df, ou %in% c(ou_sel))

  #filter for USAID mechs if specified
    if(usaid_only == TRUE)
      df <- dplyr::filter(df, agency == "USAID")

  #rename variables to match MSD
    df <- df %>%
      dplyr::select(operatingunit = ou,
                    fundingagency = agency,
                    mech_code = code,
                    primepartner = partner,
                    mech_name = mechanism)

  #remove mech_code from mech_name
    df <- df %>%
        dplyr::mutate(mech_name = stringr::str_remove(mech_name, "0000[0|1] |[:digit:]+ - "))

  #remove award information from mech_name
    df <- df %>%
      dplyr::mutate(mech_name = stringr::str_remove(mech_name, "^(720|AID|GHAG|U2GP).* - "))

  #export
    hfr_export(df, folderpath_output, type = "mechanisms")

  # Export
  if(!is.null(folderpath_output) & fs::dir_exists(folderpath_output)){

    cat("\nExporting ...\n")

    #compile file name  and export data
    filename <- paste(
        ou_sel,
        "Mechanisms",
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
