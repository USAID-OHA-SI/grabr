#' @title Create an active session for PEPFAR Panorama
#'
#' @param username Username for PEPFAR Panorama Account. Recommend using `pano_user()`
#' @param password Password for PEPFAR Panorama Account. Recommend using `pano_pwd()`
#' @param baseurl  PEPFAR Panorama base url
#'
#' @return login session
#' @export
#'
#' @examples
#' \dontrun{
#'   library(grabr)
#'
#'   s <- pano_session("<my-pano-user>", "<my-password>")
#' }
#'
pano_session <- function(username,
                         password,
                         baseurl = "https://pepfar-panorama.org") {

  # Login form token
  login_form <- base::paste0(baseurl, "/forms/generate_nonce")

  # Login process
  login_url <- base::paste0(baseurl, "/forms/mstrauth/")

  # Check user's credentials
  accnt <- lazy_secrets("pano", username, password)

  # Get token
  login_token <- httr::GET(url = login_form)

  login_token_req <- httr::content(login_token, as = "parsed")

  token <- NULL

  if (!base::is.null(login_token_req) & base::is.list(login_token_req) & login_token_req$status == 1) {
    token <- login_token_req$token_nonce
  }else {
    usethis::ui_stop("FORM ERROR - Unnable to generate login form token.")
  }

  # Data for login post submission
  login_body <- base::list(
    "project" = "PEPFAR",
    "username" = accnt$username,
    "pw" = accnt$password,
    "nonce_code" = token
  )

  # Send post request to server
  login_req <- httr::POST(url = login_url, body = login_body)

  login_sess <-  httr::content(login_req, as = "parsed")

  # validate status
  if (!base::is.null(login_sess) & base::is.list(login_sess) & login_sess$status == 1) {
    return(invisible(login_sess$mstr_session))
  }

  # Stop and display errors
  usethis::ui_stop("LOGIN ERROR - Unable to create a valid session")
}


#' @title Read html content from web page
#'
#' @param page_url URL of the page to be extracted
#' @param session  Valid and active login session
#'
#' @return html content
#'
#' @examples
#' \dontrun{
#'   library(grabr)
#'
#'   s <- pano_session("<my-pano-user>", "<my-password>")
#'   url <- "https://pepfar-panorama.org/forms/downloads"
#'
#'   cont <- pano_content(page_url = url, session = s)
#' }
#'
pano_content <- function(page_url, session) {

  page <- httr::GET(page_url, httr::set_cookies("formsSessionState" = session))

  if (!base::is.null(page) & !is.null(httr::content(page, "text"))) {
    page <- page %>%
      httr::content("text") %>%
      rvest::read_html()
  } else {
    base::stop("ERROR - Unable to extract page content")
  }

  return(page)
}


#' @title Extract data elements details from html content
#'
#' @param page_html html content
#' @param page_url  Curretn html page url
#'
#' @return html element
#'
#' @examples
#' \dontrun{
#'   library(grabr)
#'
#'   s <- pano_session("<my-pano-user>", "<my-password>")
#'   url <- "https://pepfar-panorama.org/forms/downloads"
#'
#'   cont <- pano_content(page_url = url, session = s)
#'
#'   elts <- pano_elements(page_html = cont, page_url = url)
#' }
#'
pano_elements <- function(page_html,
                          page_url = NULL) {

  # Base ulr for download page
  download_url <- "https://pepfar-panorama.org/forms/downloads"

  if (base::is.null(page_url)) {
    page_url = download_url
  }

  # Extract Parent url
  parent_path <- page_url %>%
    stringr::str_replace(download_url, "") %>%
    stringr::str_replace_all("%20", " ") %>%
    stringr::str_remove("^/") %>%
    stringr::str_remove("^/$")

  page_items <- page_html %>%
    rvest::html_elements("li>a") %>%
    rvest::html_text()

  page_hrefs <- page_html %>%
    rvest::html_elements("li>a") %>%
    rvest::html_attr("href") %>%
    stringr::str_remove_all(., "\\\\$")

  page_metas <- page_html %>%
    rvest::html_elements("li") %>%
    rvest::html_attr("class")

  df_elements <- tibble::tibble(
      parent = parent_path, #page_name,
      item = page_items,
      type = page_metas,
      path = page_hrefs
    ) %>%
    dplyr::mutate(path = base::paste0(page_url, "/", page_hrefs))
}


#' @title Extract data items from url
#'
#' @param page_url  Current html page url
#' @param username Username for PEPFAR Panorama Account. Recommend using `pano_user()`
#' @param password Password for PEPFAR Panorama Account. Recommend using `pano_pwd()`
#' @param session    Login session, only used within other `pano_extract_*()`
#'
#' @return data items as data frame
#' @export
#'
#' @examples
#' \dontrun{
#'   library(grabr)
#'
#'   s <- pano_session("<my-pano-user>", "<my-password>")
#'   url <- "https://pepfar-panorama.org/forms/downloads"
#'
#'   items <- pano_items(page_url = url, session = s)
#' }
#'
pano_items <- function(page_url, username, password, session = NULL) {

  # Clean url page
  path <- page_url
  baseurl = get_baseurl(page_url)

  # Check URL is from PANO
  if (stringr::str_detect(page_url, pano_url, negate = TRUE)) {
    usethis::ui_stop("LINK IS NOT PANO RELATED")
  }

  # Generate session if not present
  if (is.null(session)) {
    accnt <- lazy_secrets("pano", username, password)
    session <- pano_session(accnt$username, accnt$password)
  }

  # Extract items from page content
  items <- pano_content(page_url = path, session = session) %>%
    pano_elements(page_url = path)

  return(items)
}



#' @title Download file from PEPFAR Panorama
#'
#' @param item_url   URL for the item to be downlaoded
#' @param username   Username for PEPFAR Panorama Account. Recommend using `pano_user()`
#' @param password   Password for PEPFAR Panorama Account. Recommend using `pano_pwd()`
#' @param session    Login session, only used within other `pano_extract_*()`
#' @param dest_path  Location and name of the destination file
#' @param uncompress If yes, the downloaded zip file will be decompressed. Default is FALSE
#'
#' @return file content as binary
#' @export
#'
#' @examples
#' \dontrun{
#'   library(tidyverse)
#'   library(grabr)
#'   library(glamr)
#'
#'   url <- "https://pepfar-panorama.org/forms/downloads"
#'
#'   elts <- pano_items(page_html = url,
#'                      username = pano_user(), password = pano_pwd())
#'
#'   f_url <- elts %>% filter(type == "file zipfile") %>% pull(path) %>% first()
#'
#'   pano_download(item_url = url, session = s, dest = "Data") }
#'
pano_download <- function(item_url,
                          username,
                          password,
                          session = NULL,
                          dest_path = NULL,
                          uncompress = FALSE) {

  # Generate session if not present
  if (is.null(session)) {
    accnt <- lazy_secrets("pano", username, password)
    session <- pano_session(accnt$username, accnt$password)
  }

  # Default destination folder
  if (base::is.null(dest_path)) {
    usethis::ui_warn("Missing destination path - file will be placed in the recommanded SI/MER Data Path")
    dest_path <- glamr::si_path("path_msd")
  }

  if(!base::dir.exists(dest_path)) {
    base::cat(crayon::red("\nDestination is not a valid directory\n"))
    base::stop("Invalid Directory")
  }

  # Filename
  dfile <- item_url %>%
    base::basename() %>%
    urltools::url_decode() %>%
    base::file.path(dest_path, .)

  # Download file
  item_url %>%
    httr::GET(.,
              httr::write_disk(path = dfile, overwrite=TRUE),
              httr::set_cookies("formsSessionState" = session))

  # Unzip
  if (uncompress & stringr::str_ends(dfile, ".zip")) {
    zip::unzip(dfile, overwrite = TRUE, exdir = dest_path)
  }
}


#' @title Unpack Pano files directories
#'
#' @param df_pano  Panorama output files items
#' @param session  Valid and active login session
#'
#' @return unnested data frame containing output files
#'
#' @examples
#' \dontrun{
#'   library(tidyverse)
#'   library(grabr)
#'
#'   s <- pano_session("<my-pano-user>", "<my-password>")
#'   url <- "https://pepfar-panorama.org/forms/downloads"
#'
#'   cont <- pano_content(page_url = url, session = s)
#'
#'   elts <- pano_elements(page_html = cont, page_url = url)
#'
#'   pano_unpack(df_pano = elts, session = s)
#' }
#'
pano_unpack <- function(df_pano, session) {

  # filter levels
  df_items <- df_pano %>%
    dplyr::filter(type == "dir")

  # check for valid rows
  if (base::nrow(df_items) == 0) {
    base::print(glue::glue("No directories found in the list of Pano items"))

    return(df_items)
  }

  base::message("unpacking ...")
  base::print(df_items$item %>% base::paste(collapse = ", "))

  # Extract items
  df_items <- df_items %>%
    dplyr::select(item, path) %>%
    purrr::pmap_dfr(function(item, path){
      items <- pano_content(page_url = path, session = session) %>%
        pano_elements(page_url = path)

      return(items)
    })

  # Loop through all nested directories
  if ("dir" %in% df_items$type) {
    df_items <- df_items %>%
      pano_unpack(session = session) %>%
      dplyr::bind_rows(df_items, .)
  }

  return(df_items)
}


#' @title Extract data outputs from Panorama
#'
#' @note This function combines `pano_session()`, `pano_content()`, `pano_elements()`, and in some cases `pano_unpack()`
#'
#' @param item         Panorama data type. Eg: mer, financial, sims, narratives
#' @param version      Data release version: Initial or Clean, defaults to
#'  current version if blank
#' @param fiscal_year  Reporting Fiscal year, defaults to current FY if blank
#' @param quarter      Reporting Quarter, defaults to current quarter if blank
#' @param unpack       If TRUE, unpack nested directories
#' @param username     Panorama username, recommend using `glamr::pano_user()`
#' @param password     Panorama password, recommend using `glamr::pano_pwd()`
#' @param session    Login session, only used within other `pano_extract_*()`
#' @param baseurl     Panorama base url
#'
#' @return list of output files as data frame
#' @export
#'
#' @examples
#' \dontrun{
#'   library(tidyverse)
#'   library(grabr)
#'
#'   pano_extract(item = "mer")
#' }
#'
pano_extract <- function(item = "mer",
                         version,
                         fiscal_year,
                         quarter,
                         unpack = FALSE,
                         username,
                         password,
                         session = NULL,
                         baseurl = "https://pepfar-panorama.org") {

  # Links
  data_path <- "/forms/downloads"
  data_url <- base::paste0(baseurl, data_path)

  # Generate session if not present
  if (is.null(session)) {
    accnt <- lazy_secrets("pano", username, password)
    session <- pano_session(accnt$username, accnt$password)
  }

  # Search Item
  s_item <- stringr::str_to_lower(item)

  #current periods information (to provide if missing)
  df_pd_info <- glamr::pepfar_data_calendar %>%
    dplyr::filter(msd_release <= Sys.Date()) %>%
    dplyr::slice_tail()

  if(missing(fiscal_year)){
    fiscal_year <- df_pd_info$fiscal_year
  }

  if(missing(quarter)){
    quarter <- df_pd_info$quarter
  }

  if(missing(version)){
    version <- df_pd_info$type
  }

  # archived files: update based on fy & qtr
  archive <- NULL

  # Current releases
  if(!stringr::str_to_lower(version) %in% c("initial", "clean")){
    version <- df_pd_info$type
    # usethis::ui_warn("INPUT - Invalid input for version. Enter either 'Initial' or 'Clean'. Defaulting to {df_pd_info$type}.")
  }

  # Search key
  s_dir <- base::paste0(s_item, " FY", fiscal_year, " Q", quarter, " ", stringr::str_to_sentence(version))

  # Extract Main directories
  dir_items <- pano_content(
    page_url = data_url,
    session = session) %>%
    pano_elements()

  if (base::is.null(dir_items) | base::nrow(dir_items) == 0) {
    base::stop("ERROR - No data items found on the main downlaod page")
  }

  # Find Data item
  dt_item <- dir_items %>%
    dplyr::filter(stringr::str_detect(stringr::str_to_lower(item), base::paste0("^", s_item))) %>%
    dplyr::pull(item)

  # Extract Reporting Period
  rep_pd <- dt_item %>%
    stringr::str_extract_all("\\d") %>%
    base::unlist()

  curr_rep_fy <- rep_pd[1:4] %>%
    base::paste0(collapse = "") %>%
    base::as.integer()

  curr_rep_qtr <- rep_pd[5] %>% base::as.integer()

  curr_item <- dt_item %>%
    #stringr::str_extract("(Pre|Post|Initial|Clean)")
    stringr::str_extract("(?<=Q\\d[:space:]).*")

  if (fiscal_year < curr_rep_fy |
      quarter < curr_rep_qtr |
      !stringr::str_detect(s_dir, curr_item)) {
    archive = TRUE
  } else {
    archive = FALSE
  }

  # Retrieve sub-folders from previous releases
  if (archive == TRUE) {
    s_prev_dir <- base::paste0("Previous ", s_item, " Releases")

    base::message("Looking into previous releases ...")
    base::print(stringr::str_to_lower(s_prev_dir))

    # Previous sub-directories
    dir_items <- dir_items %>%
      dplyr::filter(
        stringr::str_detect(
          stringr::str_to_lower(item),
          stringr::str_to_lower(s_prev_dir)))

    if (base::is.null(dir_items) | base::nrow(dir_items) == 0) {
      base::stop("ERROR - No items found for this release")
    }

    dir_items <- dir_items %>%
      dplyr::pull(path) %>%
      purrr::map_dfr(~pano_items(page_url = .x, username = accnt$username, password = accnt$password))

    # Dir structure change for previous Mer Items
    if (s_item == "mer"){
      s_prev_subdir <- base::paste0(s_item, " FY", fiscal_year)

      dir_items <- dir_items %>%
        dplyr::filter(
          stringr::str_detect(
            stringr::str_to_lower(item),
            stringr::str_to_lower(s_prev_subdir))) %>%
        dplyr::pull(path) %>%
        purrr::map_dfr(~pano_items(page_url = .x, username = accnt$username, password = accnt$password))
    }
  }

  # Narrow search directories
  dir_items <- dir_items %>%
    dplyr::filter(
      stringr::str_detect(
        stringr::str_to_lower(item),
        stringr::str_to_lower(s_dir)))

  base::message("Search directorie(s): ")
  base::print(base::unique(dir_items$item) %>% paste(collapse = ", "))

  # Extract immediate items
  df_items <- dir_items %>%
    dplyr::pull(path) %>%
    purrr::map_dfr(~pano_items(page_url = .x, username = accnt$username, password = accnt$password))

  # Check for valid data
  if (base::is.null(df_items) | base::nrow(df_items) == 0) {
    base::cat(crayon::red("\nEMPTY - No data items found for this search\n"))
    return(dir_items)
  }

  # Unpack all the folders
  if (unpack == TRUE && "dir" %in% df_items$type) {
    base::message("Unpacking required for ...")

    dirs <- df_items %>%
      dplyr::filter(type == "dir") %>%
      dplyr::pull(item) %>%
      base::paste(collapse = ", ")

    base::print(dirs)

    df_items <- df_items %>%
      dplyr::filter(type == "dir") %>%
      pano_unpack(session = session) %>%
      dplyr::bind_rows(df_items)
  }

  #Download files
  df_files <- dir_items %>%
    dplyr::bind_rows(df_items)

  # Return files
  return(df_files)
}


#' @title Downloads Country Specific MSDs
#'
#' @param operatingunit PEPFAR Operating Unit. Default is set to NULL for
#'  to return global datasets
#' @param version       Data release version: "initial" or "clean", defaults to
#'  current version
#' @param fiscal_year   Reporting Fiscal year, defaults to current version
#' @param quarter       Reporting Quarter: Single digit quarters, defaults to
#'  current version
#' @param level         Org level, options are psnu" (default), "ou", "site",
#'  or "nat"
#' @param dest_path     Directory path to download file. Default set to
#'  `si_path()`
#' @param username      Panorama username, recommend using `glamr::pano_user()`,
#'   which is the default if left blank
#' @param password      Panorama password, recommend using `glamr::pano_pwd()`,
#'   which is the default if left blank
#' @param baseurl     Pano base url
#'
#' @export
#'
#' @examples
#' \dontrun{
#'  pano_extract_msd(operatingunit = "Zambia", level = "site")
#' }
#'
pano_extract_msd <- function(operatingunit = NULL,
                             version,
                             fiscal_year,
                             quarter,
                             level = c("psnu", "ou", "site", "nat"),
                             dest_path,
                             username,
                             password,
                             baseurl = "https://pepfar-panorama.org") {

  # URL
  url <- base::file.path(baseurl, "forms/downloads")

  # Pano Access
  accnt <- lazy_secrets("pano", username, password)

  #use path_msd if output not provided
  if (missing(dest_path)) {
    dest_path <-  glamr::si_path("path_msd")
  }

  #check if valid OU
  if(!is.null(operatingunit) &&
     !operatingunit %in% unique(glamr::pepfar_country_list$operatingunit))
    usethis::ui_stop("The {usethis::ui_field('operatingunit')} you provided (\\
                     {usethis::ui_field({operatingunit})}) is not valid")

  #default to psnu (first option) if no level specified
  level <- level[1]

  #check valid level
  if(!level %in% c("ou", "psnu", "site", "nat"))
    usethis::ui_stop("The {usethis::ui_field('level')} you provided (\\
                     {usethis::ui_field({level})}) is not valid")

  #stop if no valid destination path set
  if (!file.exists(dest_path)) {
    usethis::ui_stop("No {usethis::ui_field('dest_path')} provided or valid \\
                     and no default path stored in your \\
                     {usethis::ui_path('.Rprofile')} through \\
                     {usethis::ui_code('glamr::set_paths()')}")
  }

  #establish pano session
  sess <- pano_session(username = accnt$username,
                       password = accnt$password,
                       baseurl = baseurl)

  # IDENTIFY CURRENT PERIOD
  recent_fldr <- url %>%
    pano_content(session = sess) %>%
    pano_elements() %>%
    dplyr::filter(stringr::str_detect(item, "^MER")) %>%
    dplyr::pull(item)

  # Current Release
  curr_version <- base::ifelse(stringr::str_detect(recent_fldr, "Post|Clean"), "clean", "initial")
  curr_fy <- stringr::str_extract(recent_fldr, "[:digit:]{4}") %>% as.numeric()
  curr_qtr <- stringr::str_extract(recent_fldr, "(?<=Q)[:digit:]") %>% as.numeric()

  #fill missing param with current state
  if(missing(version))
    version <- curr_version

  if(missing(fiscal_year))
    fiscal_year <- curr_fy

  if(missing(quarter))
    quarter <- curr_qtr

  #print out parameters for users
  base::print(glue::glue("Download parameters\\
                         \nOU: {ifelse(is.null(operatingunit),'Global',operatingunit)}\\
                         \nLevel: {toupper({level})}\\
                         \nRelease: {version}\nFiscal Year: {fiscal_year}\\
                         \nQuarter: {quarter}"))

  # Data items
  df_pano <- pano_extract(item = "mer",
                          version = {{version}},
                          fiscal_year = {{fiscal_year}},
                          quarter = {{quarter}},
                          unpack = T,
                          baseurl = baseurl)

  #add OU name(s) to the search parameters
  if(!is.null(operatingunit)){
    ou_filter <- operatingunit %>%
      paste(collapse = "|") %>%
      paste0("(", ., ").zip$")
  } else {
    ou_filter <- "_\\d{1}.zip$"
  }

  # Search Key
  s_key <- level %>%
    base::paste0("^mer_.*_", .,  "_",
                 ifelse(level == "nat", "subnat", "im"), "_.*", ou_filter) %>%
    stringr::str_to_lower()


  # Filter Pano items down to what is specified in the parameters
  df_pano <- df_pano %>%
    dplyr::filter(type == "file zip_file",
           stringr::str_detect(stringr::str_to_lower(item), s_key))

  #flag error if no results to download
  if (nrow(df_pano) == 0){
    usethis::ui_oops("ERROR - Unknown options ou/level.")
    invisible(NULL)
  }

  #download all files identified through search
  purrr::walk(df_pano$path,
              function(.x) {
                print(basename(.x))
                pano_download(item_url = .x,
                              session = sess,
                              dest_path = dest_path)
                })

  #message
  usethis::ui_done("All done!...:)")
}


#' @title Downloads All Global + OU Specific MSDs
#'
#' @param operatingunit PEPFAR Operating Unit. Default is set to NULL for global datasets
#' @param add_global    Add global datasets in this extract? Default is TRUE
#' @param items         Panorama data set, default option is `mer`
#' @param archive       Logical, should the old files be archived? default=FALSE
#' @param dest_path     Directory path to download file. Default set to `glamr::si_path()`
#' @param username      Panorama username, recommend using `glamr::pano_user()`
#' @param password      Panorama password, recommend using `glamr::pano_pwd()`
#' @param baseurl      Panorama base url, default="https://pepfar-panorama.org"
#'
#' @export
#'
#' @examples
#' \dontrun{
#'  dir_mer <- si_path()
#'
#'  pano_extract_msds(operatingunit = "Zambia",
#'                    archive = TRUE,
#'                    dest_path = dir_mer)
#' }
pano_extract_msds <- function(operatingunit,
                              add_global = TRUE,
                              items = "mer",
                              archive = FALSE,
                              dest_path,
                              username,
                              password,
                              baseurl = "https://pepfar-panorama.org") {

  # URL
  url <- base::file.path(baseurl, "forms/downloads")

  # Pano Access
  accnt <- lazy_secrets("pano", username, password)

  #use path_msd if output not provided
  if (missing(dest_path)) {
    dest_path <-  glamr::si_path("path_msd")
  }

  #stop if no valid destination path set
  if (!file.exists(dest_path)) {
    usethis::ui_stop("No {usethis::ui_field('dest_path')} provided or valid \\
                     and no default path stored in your \\
                     {usethis::ui_path('.Rprofile')} through \\
                     {usethis::ui_code('glamr::set_paths()')}")
  }

  sess <- pano_session(username = accnt$username,
                       password = accnt$password,
                       baseurl = baseurl)

  # IDENTIFY CURRENT PERIOD
  recent_fldr <- url %>%
    pano_content(session = sess) %>%
    pano_elements() %>%
    dplyr::filter(stringr::str_detect(item, "^MER")) %>%
    dplyr::pull(item)

  # Release
  curr_release <- stringr::str_extract(recent_fldr, "(?<=Q\\d{1}[:space:]).*")
  curr_status <- base::ifelse(stringr::str_detect(recent_fldr, "Post|Clean"), "clean", "initial")
  curr_fy <- stringr::str_extract(recent_fldr, "[:digit:]{4}") %>% as.numeric()
  curr_qtr <- stringr::str_extract(recent_fldr, "(?<=Q)[:digit:]") %>% as.numeric()

  base::print(glue::glue("Download parameters:\\
                         \nItems: {toupper({items})}\\
                         \nRelease: {curr_release}\\
                         \nFiscal Year: {curr_fy}\\
                         \nQuarter: {curr_qtr}"))

  # Extract Data items
  items <- pano_extract(item = {{items}},
                        version = curr_status,
                        fiscal_year = curr_fy,
                        quarter = curr_qtr,
                        username = accnt$username,
                        password = accnt$password,
                        unpack = TRUE)

  # Archive existing files

  # Identify archive folder

  dir_archive <- dest_path %>%
    base::dir(path = .,
              pattern = "archive",
              full.names = TRUE,
              ignore.case = TRUE)

  # Move files

  if(archive) {

    # Check for archive folder
    if (base::identical(dir_archive, base::character(0))) {
      dir_archive <- base::file.path(dest_path, "Archive")

      base::message("Adding archive folder ...")
      base::message(dir_archive)

      base::dir.create(dir_archive)
    }

    # move files
    base::message("Archiving files ....")

    files_old <- dest_path %>%
      base::list.files(path = .,
                       pattern = "^MER_Structured_Dataset|.*.xlsx$|.*.pdf$",
                       full.names = TRUE)

    files_old %>%
      purrr::walk(function(.x){
        file_name <- base::basename(.x)
        arch_name <- base::file.path(dir_archive, file_name)

        base::message(base::paste0(file_name, " => ", arch_name))

        fs::file_move(.x, arch_name)
      })

    usethis::ui_done("Archiving completed!")
  }

  # Global and/or OU Specific MSD

  if (add_global) {

    files_down <- items %>%
      dplyr::filter(
        stringr::str_detect(
          stringr::str_to_lower(item),
          base::paste0("_(", stringr::str_to_lower(paste(operatingunit, collapse = "|")), ").zip")) |
        (stringr::str_detect(parent, glue::glue("{curr_release}$|{curr_release}/FY15-20$")) &
           stringr::str_detect(item, "OU_IM|PSNU|PSNU_IM|PSNU_IM_DREAMS|NAT_SUBNAT")),
        type == "file zip_file") %>%
      dplyr::pull(path)

  } else {

    files_down <- items %>%
      dplyr::filter(
        stringr::str_detect(
          stringr::str_to_lower(item),
          base::paste0("_(", stringr::str_to_lower(paste(operatingunit, collapse = "|")), ").zip")),
        type == "file zip_file") %>%
      dplyr::pull(path)
  }

  # Notification
  usethis::ui_info("Downloading files [{length(files_down)}] ...")

  # Download
  files_down %>%
    purrr::walk(function(.x) {
      base::print(basename(.x))
      pano_download(item_url = .x, session = sess, dest_path = dest_path)
    })

  usethis::ui_done("All done!...:)")
}

