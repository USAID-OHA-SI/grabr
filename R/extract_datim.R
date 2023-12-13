#' @title Get PEPFAR/DATIM dimensions
#'
#' @param username DATIM Account Username
#' @param password DATIM Account passward
#' @param var      Column name to pull all values from, default is NULL, options are: id, dimension
#' @param baseurl  DATIM API End point, , default value is `https://final.datim.org`
#'
#' @export
#' @return Dimensions as tibble or list of ids / dimension names
#'
#' @examples
#' \dontrun{
#'   library(grabr)
#'
#'   datim_dimensions()
#' }
#'
datim_dimensions <- function(username, password,
                             var = NULL,
                             baseurl = "https://final.datim.org") {

  # datim credentials
  accnt <- lazy_secrets("datim", username, password)

  # Update URL + remove paging
  url <- get_baseurl(baseurl) %>%
    paste0("/api/dimensions?paging=false")

  # Query datim
  dims <- url %>%
    datim_execute_query(accnt$username, accnt$password, flatten = TRUE) %>%
    purrr::pluck("dimensions") %>%
    tibble::as_tibble() %>%
    janitor::clean_names() %>%
    dplyr::rename(dimension = display_name)

  if (!base::is.null(var) && var %in% c("id", "dimension")) {
    dims <- dims %>% dplyr::pull(!!sym(var))
  }

  return(dims)
}

#' @title Get PEPFAR/DATIM Dimension ID
#'
#' @param name      Dimension name
#' @param username  DATIM Account Username, recommended using glamr::datim_user()`
#' @param password  DATIM Account passward, recommended using glamr::datim_pwd()`
#' @param baseurl   DATIM API End point, default value is `https://final.datim.org/`
#'
#' @return dimension uid
#' @export
#'
#' @examples
#' \dontrun{
#'   library(grabr)
#'   datim_dimension("OU Level")
#' }
#'
datim_dimension <- function(name,
                            username,
                            password,
                            baseurl = "https://final.datim.org/") {
  # ID
  dim_id <- NULL

  # datim credentials
  accnt <- lazy_secrets("datim", username, password)

  # Query dimensions
  .df_dims <- datim_dimensions(username = accnt$username,
                               password = accnt$password,
                               baseurl = baseurl)

  if (base::is.null(.df_dims) | base::nrow(.df_dims) == 0 | !name %in% .df_dims$dimension) {
    base::message(crayon::red(glue::glue("There is no '{name}' dimension. Check spelling.")))
    return(NULL)
  }

  # Return dimension id
  dim_id = .df_dims %>%
    dplyr::filter(dimension == name) %>%
    dplyr::pull(id)

  return(dim_id)
}


#' @title Get PEPFAR/DATIM Dimension Items
#'
#' @param dimension Dimension name
#' @param username  DATIM Account Username
#' @param password  DATIM Account passward
#' @param var       column name to pull values from, id or item
#' @param fields    list of column names to return, this will overwrite `var`
#' @param baseurl   DATIM API end point
#'
#' @export
#' @return Dimension's items as tibble or vector
#'
#' @examples
#' \dontrun{
#'   library(grabr)
#'
#'   datim_dim_items(dimension = "Funding Agency")
#'   datim_dim_items(dimension = "Funding Agency", var = "item")
#' }
#'
datim_dim_items <- function(dimension,
                            username,
                            password,
                            var = NULL,
                            fields = NULL,
                            baseurl = "https://final.datim.org/"){

  # datim credentials
  accnt <- lazy_secrets("datim", username, password)

  # clean up url / paging
  baseurl <- get_baseurl(baseurl)

  # Get dimension id
  dim_id <- datim_dimension(name = dimension,
                            username = accnt$username,
                            password = accnt$password,
                            baseurl = baseurl)

  # Update url with id and paging
  url_dims <- glue::glue("{baseurl}/api/dimensions/{dim_id}/items?paging=false")

  # Request specific fields
  if (!base::is.null(fields)) {
    url_dims <- fields %>%
      base::paste0(collapse = ",") %>%
      base::paste0(url_dims, "&fields=", .)
  }

  # Get items
  items <- url_dims %>%
    datim_execute_query(accnt$username, accnt$password, flatten = TRUE)

  items <- items %>%
    purrr::pluck("items") %>%
    tibble::as_tibble()

  if (base::is.null(items) | base::nrow(items) == 0) {
    base::message(glue::glue("Dimension: {dimension}, reponse null / empty"))
    base::stop("Invalid dimension name")
  }

  items <- items %>% janitor::clean_names()

  if ("display_name" %in% base::names(items)) {
    items <- items %>% dplyr::rename(item = display_name)
  }

  # Return values from variable name
  if (!base::is.null(var) && var %in% c("id", "item") && var %in% base::names(items)) {
    items <- items %>% dplyr::pull(!!sym(var))
  }

  return(items)
}


#' @title Get dimension / item id
#'
#' @param dimension Dimension name
#' @param name      Item name
#' @param username  DATIM Account Username
#' @param password  DATIM Account password
#' @param baseurl   DATIM API end point
#'
#' @export
#' @return UID of item
#'
#' @examples
#' \dontrun{
#'   library(grabr)
#'
#'   datim_dim_item(dimension = "Funding Agency", name = "USAID")
#'
#'   datim_dim_item(dimension = "Targets / Results", name = "MER Results")
#'   datim_dim_item(dimension = "Targets / Results", name = "MER Targets")
#' }
#'
datim_dim_item <- function(dimension, name,
                           username, password,
                           baseurl = "https://final.datim.org/") {

  item_id <- NULL

  # datim credentials
  accnt <- lazy_secrets("datim", username, password)

  # Get dimension's items
  items <- datim_dim_items(dimension = dimension,
                           username = accnt$username,
                           password = accnt$password,
                           baseurl = baseurl)

  if (is.null(items) || nrow(items) == 0) {
    base::message(glue::glue("Dimension: {dimension}, response is null or empty"))
    base::stop("Unable to identify items under this dimension")
  }

  # validate item
  if ( name %in% items$item) {
    item_id <- items %>%
      dplyr::filter(item == {{name}}) %>%
      dplyr::pull(id)

  } else {
    base::message(glue::glue("Dimension: {dimension}, Item: {name}, response = {nrow(items)}"))
    base::stop("unable to locate item name")
  }

  return(item_id)
}


#' @title Build PEPFAR/DATIM Query dimension
#'
#' @param dimension Dimension name
#' @param items     Item name
#' @param username  DATIM Account Username
#' @param password  DATIM Account passward
#' @param baseurl   DATIM API End point
#'
#' @return Valid DATIM Query Params url
#' @export
#'
#' @examples
#' \dontrun{
#'   library(grabr)
#'
#'   datim_dim_url(dimension = "Sex")
#'
#'   datim_dim_url(
#'     dimension = "Disaggregation Type",
#'     items = "Age/Sex/HIVStatus"
#'   )
#'
#'   datim_dim_url(
#'     dimension = "Disaggregation Type",
#'     items = c("Age/Sex", "Age/Sex/HIVStatus")
#'   )
#'
#' }
#'
#'
datim_dim_url <- function(dimension,
                          items = NULL,
                          username,
                          password,
                          baseurl = "https://final.datim.org/") {

  # datim credentials
  accnt <- lazy_secrets("datim", username, password)

  # Query params
  dim_query <- NULL

  # Get specific items from dimension
  if (!base::is.null(items)) {

    dim_id <- datim_dimension(name = dimension,
                              username = accnt$username,
                              password = accnt$password,
                              baseurl = baseurl)

    dim_query <- items %>%
      purrr::map(function(item) {

        dim <- datim_dim_item(dimension = dimension,
                              name = item,
                              username = accnt$username,
                              password = accnt$password,
                              baseurl = baseurl) %>%
          base::unlist() %>%
          base::paste(collapse = ';') %>%
          base::paste0("dimension=", dim_id, ":", .)

        return(dim)

      }) %>%
      base::unlist() %>%
      base::paste(collapse = '&')

    return(dim_query)
  }

  # Get all items from dimension
  dim_query <- dimension %>%
    purrr::map(function(dim) {

      dim_id <- datim_dimension(name = dim,
                                username = accnt$username,
                                password = accnt$password,
                                baseurl = baseurl)

      dim_items <- datim_dim_items(dimension = dim,
                                   username = accnt$username,
                                   password = accnt$password,
                                   baseurl = baseurl) %>%
        dplyr::pull(item) %>%
        purrr::map(~datim_dim_item(dimension = dim,
                                   name = .x,
                                   baseurl = baseurl,
                                   username = accnt$username,
                                   password = accnt$password)) %>%
        base::unlist() %>%
        base::paste(collapse = ';') %>%
        base::paste0("dimension=", dim_id, ":", .)

      return(dim_items)

    }) %>%
    base::unlist() %>%
    base::paste(collapse = '&')

  return(dim_query)
}


#' @title Execute Datim Query
#'
#' @param url       API Base url & all query parameters
#' @param username  Datim username, recommend using `glamr::datim_user()`
#' @param password  Datim password, recommend using `glamr::datim_pwd()`
#' @param flatten   Should query json result be flatten? Default is false
#'
#' @return returns query results as json object, or NULL when error occurs.
#' @export
#'
#' @examples
#' \dontrun{
#'   library(grabr)
#'
#'   datim_execute_query(
#'     url = 'https://www.datim.org/api/sqlViews/<uid>?format=json',
#'     username =glamr::datim_user(),
#'     password =glamr::datim_pwd(),
#'     flatten = TRUE
#'   )
#' }
#'
datim_execute_query <- function(url,
                                username,
                                password,
                                flatten = FALSE) {

  # clean url
  query_url <- url %>%
    urltools::url_decode() %>%
    gsub(" ", "%20", .)

  baseurl <- get_baseurl(query_url)

  # datim credentials
  accnt <- lazy_secrets("datim", username, password)

  # Run query
  .data_json <- base::tryCatch({

      # Execute Query
      .res <- query_url %>%
          httr::GET(httr::authenticate(accnt$username, accnt$password))

      # Make sure execution went well
      if (!exists(".res")) {
        usethis::ui_stop(paste0("UNABLE TO EXECUTE QUERY: ", query_url))
      }

      # Reject non 200 (OK) responses
      if (base::is.list(.res) & "status_code" %in% names(.res) & .res$status_code != 200L) {

        .status <- httr::http_status(.res)

        base::message(paste0("UNSUCCESSFUL REQUEST: ", stringr::str_to_upper(.status$reason), " - ", .status$message))

        return(NULL)
      }

      # Get data from query response
      .data <- .res %>%
        httr::content("text") %>%
        jsonlite::fromJSON(flatten = flatten)

      return(.data)

    },
    warning = function(warn) {
      base::message(crayon::yellow("WARNINGS: "))
      base::print(warn)
    },
    error = function(err) {
      base::print(err)
      usethis::ui_stop("UNABLE TO EXECUTE QUERY.")
    })

  return(.data_json)
}


#' @title Process Datim Query results
#'
#' @param url       Datim API Call url
#' @param username  Datim username, recommend using `glamr::datim_user()`
#' @param password  Datim password, recommend using `glamr::datim_pwd()`
#'
#' @return Data as tibble or NULL when error occurs.
#' @export
#'
#' @examples
#' \dontrun{
#'  library(grabr)
#'  datim_process_query("<full-api-call-url>")
#' }
#'
datim_process_query <- function(url,
                                username,
                                password) {

  # datim credentials
  accnt <- lazy_secrets("datim", username, password)

  # run the query
  res_json <- url %>%
    datim_execute_query(accnt$username, accnt$password)

  # check for valid json result
  if (base::is.null(res_json)) {
    base::message(crayon::red("No data available for your query"))
    return(NULL)
  }

  if (base::length(res_json$rows) == 0) {
    base::message(crayon::red("No data available for your query"))
    return(NULL)
  }

  # Extract Metadata
  metadata <- purrr::map_dfr(res_json$metaData$items, dplyr::bind_rows, .id = "from")

  # Extract data
  df <- tibble::as_tibble(x = res_json$rows,
                          .name_repair = ~ res_json$headers$column)

  orguids <- df$`Organisation unit`

  # Reformat OrgHierarchy columns, if any
  if (stringr::str_detect(url, "hierarchyMeta=true")) {

    orgpath <- dplyr::bind_rows(res_json$metaData$ouHierarchy) %>%
      tidyr::pivot_longer(cols = tidyselect::everything(),
                          names_to = "key",
                          values_to = "value")

    levels <- orgpath$value %>%
      stringr::str_count("/") %>%
      max() + 1

    headers <- paste0("orglvl_", seq(1:levels))

    df <- dplyr::left_join(x = df,
                           y = orgpath,
                           by = c("Organisation unit" = "key")) %>%
      tidyr::separate(value, headers, sep = "/")
  }

  # Clean and augmente data
  df <- df %>%
    dplyr::mutate_all(~plyr::mapvalues(., metadata$from, metadata$name, warn_missing = FALSE)) %>%
    dplyr::mutate(Value = as.numeric(Value)) %>%
    dplyr::bind_cols(orgunituid = orguids) %>%
    dplyr::relocate(orgunituid, .before = 1)

  return(df)
}


#' @title Query PEPFAR/DATIM targets/results data
#'
#' @param ou          Operatingunit
#' @param level       Organization hierarchy level
#' @param pe          Reporting period. This can be expressed as relative or fixed periods
#'                    Eg.: "THIS_FINANCIAL_YEAR", "2020Oct", "QUARTERS_THIS_YEAR", "2021Q2"
#'                    default is "THIS_FINANCIAL_YEAR"
#' @param ta          Technical Area, valid option can be obtain from glamr::datim_dim_items("Technical Area")`
#' @param value       Type of value to return, MER Targets or Results or both
#' @param disaggs     Disaggregation Types. This depends on the value of ta
#' @param dimensions  Additional dimensions and/or columns. This depends on values of ta and disaggs
#' @param property    Type of name
#' @param metadata    Should metadata be included
#' @param hierarchy    Should additional hirarchy level be included
#' @param baseurl     DATIM API End point url
#' @param username    Datim username
#' @param password    Datim username
#' @param verbose     Display all notifications
#'
#' @return data as tibble
#' @export
#'
#' @examples
#' \dontrun{
#'  library(grabr)
#'
#'  datim_query(ou = "Nigeria", ta = "PLHIV")
#'
#'  datim_query(ou = "Nigeria", ta = "POP_EST")
#'
#'  datim_query(ou = "Nigeria",
#'              level = "country",
#'              ta = "TX_CURR",
#'              disaggs = "Age/Sex/HIVStatus",
#'              dimensions = c("Age: <15/15+  (Coarse)", "Sex"))
#' }
#'
datim_query <-
  function(ou, username, password,
           level = "prioritization",
           pe = "THIS_FINANCIAL_YEAR",
           ta = "PLHIV",
           value = NULL,
           disaggs = NULL,
           dimensions = NULL,
           property = "SHORTNAME",
           metadata = TRUE,
           hierarchy = TRUE,
           baseurl = "https://final.datim.org/",
           verbose = FALSE){

    # datim credentials
    accnt <- lazy_secrets("datim", username, password)

    # Notifications
    if (verbose) {
      base::print(glue::glue("Running query for:"))
      base::print(glue::glue("OU = {ou}"))
      base::print(glue::glue("period = {base::paste(pe, collapse = ', ')}"))
      base::print(glue::glue("level = {level}"))
      base::print(glue::glue("tech area = {ta}"))
      base::print(glue::glue("value = {base::paste(value, collapse = ', ')}"))
      base::print(glue::glue("disaggs = {base::paste(disaggs, collapse = ', ')}"))
      base::print(glue::glue("dimensions = {base::paste(dimensions, collapse = ', ')}"))
    }

    # Org OU
    ou_uid <- get_ouuid(ou,
                        username = accnt$username,
                        password = accnt$password,
                        baseurl = baseurl)

    if (base::is.null(ou_uid)) {
      base::message(crayon::red("OU/Country [{ou}] is invalid or unavailable"))
      return(NULL)
    }

    # Org Level
    org_lvl <- get_ouorglevel(ou, org_type = level,
                              username = accnt$username,
                              password = accnt$password,
                              baseurl = baseurl)

    if (base::is.null(org_lvl)) {
      base::message(crayon::red("Org level [{level}] is invalid or unavailable"))
      return(NULL)
    }

    # ou/org level
    url_core <-
      paste0(baseurl, "/analytics?",
             "dimension=ou:", ou_uid, ";LEVEL-", org_lvl)

    # Notifications
    if (verbose) {
      base::print(glue::glue("CORE URL = {url_core}"))
    }

    # Targets/Results
    dim_tr_name <- "Targets / Results"

    dim_tr <- datim_dimension(name = dim_tr_name,
                              username = accnt$username,
                              password = accnt$password,
                              baseurl = baseurl)

    # Get value types
    if (!base::is.null(value)) {
      # Get user's options
      dim_tr_value <- value %>%
        purrr::map(~datim_dim_item(dimension = dim_tr_name,
                                   name = .x,
                                   username = accnt$username,
                                   password = accnt$password,
                                   baseurl = baseurl)) %>%
        base::unlist() %>%
        base::paste0(collapse = ";")

    } else {
      # Default options: use all items
      dim_tr_value <- datim_dim_items(dimension = dim_tr_name,
                                      var = "item",
                                      username = accnt$username,
                                      password = accnt$password,
                                      baseurl = baseurl) %>%
        purrr::map(~datim_dim_item(dimension = "Targets / Results",
                                   name = .x,
                                   username = accnt$username,
                                   password = accnt$password,
                                   baseurl = baseurl)) %>%
        base::unlist() %>%
        base::paste0(collapse = ";")
    }

    # Indicators
    dim_ta <- datim_dimension(name = "Technical Area",
                              username = accnt$username,
                              password = accnt$password,
                              baseurl = baseurl)

    dim_ta_ind <- datim_dim_item(dimension = "Technical Area",
                                 name = ta,
                                 username = accnt$username,
                                 password = accnt$password,
                                 baseurl = baseurl)

    # Periods
    periods <- pe %>% base::paste(collapse = ";")

    # Pe, ta & tr
    url_type <-
      paste0("dimension=pe:", periods, "&",                  # period
             "dimension=", dim_ta, ":", dim_ta_ind, "&",     # technical area: PLHIV, POP_EST
             "dimension=", dim_tr, ":", dim_tr_value, "&")   # Targets / Results: Targets

    # Notifications
    if (verbose) {
      base::print(glue::glue("TYPE URL = {url_type}"))
    }

    # Disaggs params
    url_disaggs <- NULL

    if (!base::is.null(disaggs)) {
      url_disaggs <- disaggs %>%
        purrr::map(~datim_dim_url(dimension = "Disaggregation Type",
                                  items = .x,
                                  username = accnt$username,
                                  password = accnt$password,
                                  baseurl = baseurl)) %>%
        base::unlist() %>%
        base::paste(collapse = '&')
    }

    # Notifications
    if (verbose) {
      base::print(glue::glue("DISAGGS URL = {url_disaggs}"))
    }

    # Dimensions params
    url_dims <- NULL

    if (!base::is.null(dimensions)) {
      url_dims <- dimensions %>%
        purrr::map(~datim_dim_url(dimension = .x,
                                  username = accnt$username,
                                  password = accnt$password,
                                  baseurl = baseurl)) %>%
        base::unlist() %>%
        base::paste(collapse = '&')
    }

    # Notifications
    if (verbose) {
      base::print(glue::glue("DIMS URL = {url_dims}"))
    }

    # Other params
    skipmeta <- base::ifelse(!metadata, "true", "false") # Get the reverse
    orgsmeta <- base::ifelse(hierarchy, "true", "false")

    url_meta <- glue::glue("displayProperty={property}&skipMeta={skipmeta}&hierarchyMeta={orgsmeta}")

    # Notifications
    if (verbose) {
      base::print(glue::glue("META URL = {url_meta}"))
    }

    # Combine url parts
    url <- c(url_core, url_type)

    # include sections
    if (!base::is.null(url_disaggs)) {
      # disaggs section
      url <- url %>% base::append(url_disaggs)

      if (!base::is.null(url_dims)) {
        # dimension sections: depend on disaggs
        url <- url %>% base::append(url_dims)
      }
    }

    # metadata section
    url <- url %>%
      base::append(url_meta) %>%
      base::paste(collapse = '&')

    # Notifications
    if (verbose) {
      base::print(glue::glue("QUERY URL = {url}"))
    }

    # Process query
    df <- datim_process_query(url)

    return(df)
  }


#' @title Extract PLHIV and General POP Estimates from datim
#'
#' @param ou        Operatingunit
#' @param username  Datim account username
#' @param password  Datim account password
#' @param level     Organization level
#' @param fy        Fiscal Year
#' @param hierarchy Should additional organizational hierarchy be added?, default is FALSE
#' @param baseurl   DATIM API base url
#'
#' @return PLHIV and POP_EST Data
#' @export
#'
#' @examples
#' \dontrun{
#'   library(grabr)
#'
#'   datim_pops(ou = "Nigeria")
#'
#'   datim_pops(ou = "Nigeria", fy = 2021)
#'
#'   datim_pops(ou = "Nigeria", level = "psnu", fy = 2021, hierarchy = TRUE)
#' }
datim_pops <- function(ou, username, password,
                       level = "country",
                       fy = NULL,
                       hierarchy = FALSE,
                       baseurl = "https://final.datim.org/") {

  # datim credentials
  accnt <- lazy_secrets("datim", username, password)

  # level
  lvl <- level %>% stringr::str_to_lower()

  if (!lvl %in% c("country", "psnu", "prioritization")) {
    base::message(crayon::red(glue::glue("Invalid level: {level}")))
    return(NULL)
  }

  if (lvl == "psnu") {
    lvl <- "prioritization"
  }

  # fiscal year & Period
  if (base::is.null(fy)) {
    fy <- lubridate::quarter(base::Sys.Date(),
                             with_year = TRUE,
                             fiscal_start = 10) %>%
      stringr::str_extract("^\\d{4}")
  }

  period <- fy %>%
    base::as.integer() %>%
    base::c(1, .) %>%
    base::diff() %>%
    base::as.character() %>%
    base::paste0("Oct")

  # PLHIV
  df_plhiv <- ou %>%
    datim_query(ou = .,
                level = lvl,
                pe = period,
                ta = 'PLHIV',
                value = "MER Targets",
                disaggs = "Age/Sex/HIVStatus",
                dimensions = c("Sex", "Age: Semi-fine age"),
                hierarchy = hierarchy,
                username = accnt$username,
                password = accnt$password,
                baseurl = baseurl)

  if (base::is.null(df_plhiv)) {
    df_plhiv <- tibble::tibble()
    base::message(crayon::red(glue::glue("Could not extract PLHIV Data for {ou}")))
  }

  # POP_EST
  df_pop <- ou %>%
    datim_query(ou = .,
                level = lvl,
                pe = period,
                ta = 'POP_EST',
                value = "MER Targets",
                disaggs = "Age/Sex",
                dimensions = c("Sex", "Age: Semi-fine age"),
                hierarchy = hierarchy,
                username = accnt$username,
                password = accnt$password,
                baseurl = baseurl)

  if (base::is.null(df_pop)) {
    df_pop <- tibble::tibble()
    base::message(crayon::red(glue::glue("Could not extract POP_EST Data for {ou}")))
  }

  # Merge both
  df <- df_plhiv %>% dplyr::bind_rows(df_pop)

  # Check data validity
  if (base::nrow(df) == 0) {
    return(tibble::tibble())
  }

  # Clean up data
  df <- df %>%
    janitor::clean_names() %>%
    dplyr::select(-targets_results) %>%
    dplyr::rename(fiscal_year = period,
                  orgunit = organisation_unit,
                  indicator = technical_area,
                  standardizeddisaggregate = disaggregation_type,
                  trendsfine = age_semi_fine_age) %>%
    dplyr::mutate(fiscal_year = stringr::str_extract(fiscal_year, "\\d{4}$"),
                  trendsfine = stringr::str_remove(trendsfine, "\\(Specific\\)"),
                  trendsfine = stringr::str_remove(trendsfine, "\\(Inclusive\\)"),
                  trendsfine = stringr::str_trim(trendsfine, side = "both"),
                  trendsfine = stringr::str_replace(trendsfine, "<1", "<01"),
                  trendsfine = stringr::str_replace(
                    trendsfine, "^\\d{1}-",
                    base::paste0("0", stringr::str_extract(trendsfine, "^\\d{1}"), "-")),
                  trendsfine = stringr::str_replace(
                    trendsfine,
                    "-\\d{1}$",
                    base::paste0("-0", stringr::str_extract(trendsfine, "\\d{1}$"))))

  # Clean up hierarchy
  if (hierarchy) {

    # Remove global/regional info
    df <- df %>%
      #dplyr::select(-tidyselect::ends_with("2")) %>%  # Region
      dplyr::select(-tidyselect::ends_with("1"))       # Global

    # rename cols
    cols <- df %>%
      base::names() %>%
      purrr::map(function(.x){
        if (stringr::str_detect(.x, "_\\d")) {
          lbl <- get_ouorglabel(
            operatingunit = ou,
            org_level = stringr::str_extract(.x, "\\d$"),
            username = accnt$username,
            password = accnt$password,
            baseurl = baseurl)

          return(lbl)
        }
        return(.x)
      }) %>%
      base::unlist()

    base::names(df) <- cols
  }

  return(df)
}


#' @title DATIM Analytics API
#'
#' @note Consider using glamr::datim_process_query()` and/or glamr::datim_execute_query()`
#'
#' @param url supply url for API call
#' @param username DATIM Username, defaults to using glamr::datim_user()` if blank
#' @param password DATIM password, defaults to using glamr::datim_pwd()` if blank
#'
# #@export
#' @return  API pull of data from DATIM
#' @examples
#' \dontrun{
#'  myurl <- paste0(baseurl, "api/29/analytics.json?
#'                  dimension=LxhLO68FcXm:udCop657yzi&
#'                  dimension=ou:LEVEL-4;HfVjCurKxh2&
#'                  filter=pe:2018Oct&
#'                  displayProperty=SHORTNAME&outputIdScheme=CODE")
#'
#'  df_targets <- extract_datim(myurl,glamr::datim_user(),glamr::datim_pwd())
#'
#'  }
#'
extract_datim <- function(url, username, password) {

  check_internet()

  # datim credentials
  accnt <- lazy_secrets("datim", username, password)

  json <- url %>%
    httr::GET(httr::authenticate(accnt$username,accnt$password)) %>%
    httr::content("text") %>%
    jsonlite::fromJSON()

  if ( NROW(json$rows) > 0 ) {
    metadata <- purrr::map_dfr(json$metaData$items, dplyr::bind_rows, .id = "from")

    suppressMessages(
    df <- tibble::as_tibble(json$rows, .name_repair = ~ json$headers$column)
    )

    orguids <- df$`Organisation unit`

    if (stringr::str_detect(url, "hierarchyMeta=true")){

      orgpath <- dplyr::bind_rows(json$metaData$ouHierarchy) %>%
        tidyr::gather()

      levels <- orgpath$value %>%
        stringr::str_count("/") %>%
        max() + 1

      headers <- paste0("orglvl_", seq(1:levels))

      df <- dplyr::left_join(df, orgpath, by = c("Organisation unit" = "key")) %>%
        tidyr::separate(value, headers, sep = "/")
    }


    df <- df %>%
      dplyr::mutate_all(~plyr::mapvalues(., metadata$from, metadata$name, warn_missing = FALSE)) %>%
      dplyr::mutate(Value = as.numeric(Value)) %>%
      dplyr::bind_cols(orgunituid = orguids) %>%
      convert_datim_pd_to_qtr()

    return(df)

  } else {

    return(NULL)

  }
}


#' @title Query Datim SQLViews
#' @note  This function should be used to identify Datim SQLView and Extract Data
#'
#' @param username  Datim username
#' @param password  Datim password
#' @param view_name Datim SQLView name
#' @param dataset   Return SQLView dataset or uid? Default is false
#' @param datauid   Data UID
#' @param query     SQLView Query params, a list containing type and params key value pairs
#' @param baseurl  Datim API Base URL
#'
#' @export
#' @return SQLView uid or dataset as data frame
#'
#' @examples
#' \dontrun{
#'   library(grabr)
#'
#'   datim_sqlviews(
#'     username =glamr::datim_user(),
#'     password =glamr::datim_pwd(),
#'     view_name = "A list of OUs",
#'     dataset = TRUE
#'   )
#' }
#'
datim_sqlviews <- function(username, password,
                           view_name = NULL,
                           dataset = FALSE,
                           datauid = NULL,
                           query = NULL,
                           baseurl = NULL) {

  # datim credentials
  accnt <- lazy_secrets("datim", username, password)

  # Base url
  if (missing(baseurl) | is.null(baseurl))
    baseurl <- "https://final.datim.org"

  baseurl <- get_baseurl(baseurl)

  # Other Options
  end_point <- "/api/sqlViews/"

  options <- "?format=json&paging=false"

  # API URL
  api_url <- baseurl %>% paste0(end_point, options)

  # Query data
  .data <- api_url %>%
    datim_execute_query(accnt$username, accnt$password, flatten = TRUE) %>%
    purrr::pluck("sqlViews") %>%
    tibble::as_tibble() %>%
    dplyr::rename(uid = id, name = displayName)

  # Filter if needed
  if (!base::is.null(view_name)) {

    #usethis::ui_info("Searching for SQL View: {view_name} ...")

    .data <- .data %>%
      dplyr::filter(stringr::str_to_lower(name) == stringr::str_to_lower(view_name))
  }

  # Number of rows
  rows = base::nrow(.data)

  # Return only ID when results is just 1 row
  if(rows == 0) {
    base::warning("No match found for the requested SQL View")
    return(NULL)
  }
  # Flag non-unique sqlview names
  else if (rows > 1 && dataset == TRUE) {
    base::warning("There are more than 1 match for the requested SQL View data. Please try to be specific.")
    print(.data)
    return(NULL)
  }
  # Return only ID when results is just 1 row
  else if (rows == 1 && dataset == FALSE) {
    return(.data$uid)
  }
  # Return SQLVIEW data
  else if(base::nrow(.data) == 1 && dataset == TRUE) {

    dta_uid <- .data$uid

    dta_url <- baseurl %>%
      paste0(end_point, dta_uid, "/data", options, "&fields=*") #:identifiable, :nameable

    # Apply varialbe or field query if needed
    if (!is.null(query)) {

      q <- names(query$params) %>%
        purrr::map_chr(~paste0(.x, "=", query$params[.x])) %>%
        paste0(collapse = "&") %>%
        paste0("QUERY PARAMS: type=", query$type, "&", .)

      #print(print(glue::glue("SQL View Params: {q}")))

      if (query$type == "variable") {
        vq <- names(query$params) %>%
          purrr::map_chr(~paste0(.x, ":", query$params[.x])) %>%
          paste0(collapse = "&") %>%
          paste0("&var=", .)

        #print(glue::glue("SQL View variable query: {vq}"))

        dta_url <- dta_url %>% paste0(vq)
      }
      else if (query$type == "field") {
        fq <- names(query$params) %>%
          purrr::map_chr(~paste0("filter=", .x, ":eq:", query$params[.x])) %>%
          paste0(collapse = "&") %>%
          paste0("&", .)

        #print(glue::glue("SQL View field query: {fq}"))

        dta_url <- dta_url %>% paste0(fq)
      }
      else {
        usethis::ui_stop("Error - Invalid query type: {query$type}")
      }
    }

    #print(glue::glue("SQL View url: {dta_url}"))

    # Query data
    .data <- dta_url %>%
      datim_execute_query(accnt$username, accnt$password, flatten = TRUE)

    # Detect Errors
    if (!is.null(.data$status)) {
      usethis::ui_info("Status: {data$status}")

      if(!is.null(.data$message)) {
        usethis::ui_info("Message: {data$message}")
      }

      return(NULL)
    }

    # Headers
    headers <- .data %>%
      purrr::pluck("listGrid") %>%
      purrr::pluck("headers") %>%
      dplyr::pull(column)

    # Data
    .data <- .data %>%
      purrr::pluck("listGrid") %>%
      purrr::pluck("rows") %>%
      tibble::as_tibble(.name_repair = "unique") %>%
      janitor::clean_names() %>%
      magrittr::set_colnames(headers)
  }

  return(.data)
}


#' @title Pull Orgunits SQLView
#'
#' @param cntry    Country name
#' @param username Datim username
#' @param password Datim password
#' @param reshape  Unpack parent org units
#' @param baseurl Datim API Base URL, default to https://final.datim.org/
#'
#' @export
#' @return OU/Country Orgunits as a data frame
#'
#' @examples
#' \dontrun{
#'   library(grabr)
#'
#'   datim_orgunits(
#'    cntry = "Mozambique",
#'    username = glamr::datim_user(),
#'    password = glamr::datim_pwd()
#'   )
#' }
datim_orgunits <- function(cntry, username, password,
                           reshape = FALSE,
                           baseurl = "https://final.datim.org/") {

  # datim credentials
  accnt <- lazy_secrets("datim", username, password)

  # Clean url
  baseurl <- get_baseurl(baseurl) %>% paste0("/")

  # Get PEPFAR Countries
  if(!cntry %in% glamr::pepfar_country_list$country) {
    usethis::ui_stop(glue::glue("Invalid country name: {cntry}"))
  }

  # Get Country ISO Code
  cntry_iso <- glamr::pepfar_country_list %>%
    dplyr::filter(country == cntry) %>%
    dplyr::pull(country_iso) %>%
    dplyr::first()

  # Get org units
  .orgs <- datim_sqlviews(
    username = accnt$username,
    password = accnt$password,
    view_name = "Data Exchange: Organisation Units",
    dataset = TRUE,
    query = list(
      type = "variable",
      params = list("OU" = cntry_iso)
    ),
    baseurl = baseurl
  )

  # Reshape and clean data
  if (!reshape) return(.orgs)

  clean_orgunits(.orgs, cntry, username, password, baseurl)
}

#' @title Clean Orgunits SQLView
#'
#' @param .orgs    Raw data from `datim_orgunits()`
#' @param cntry    Country name
#' @param username Datim username
#' @param password Datim password
#' @param baseurl  Datim API Base URL
#'
#' @return OU/Country Orgunits as a data frame
#'
clean_orgunits <- function(.orgs, cntry, username, password, baseurl) {

  # Country Org. Levels

  .cntry_levels <- get_levels(
      username = username,
      password = password,
      reshape = T
    ) %>%
    dplyr::filter(countryname == cntry) %>%
    dplyr::arrange(level) %>%
    dplyr::mutate(level = as.character(level))

  # Initial Cleaning

  .orgs <- .orgs %>%
    dplyr::rename_with(.cols = tidyselect::contains("internal_id"),
                       .fn = ~ stringr::str_replace(., "internal_id", "uid")) %>%
    dplyr::rename_with(.cols = tidyselect::ends_with("parent"),
                       .fn = ~paste0(., "_name")) %>%
    dplyr::left_join(
      dplyr::select(.cntry_levels, orgunit_level = level, orgunit_label = label),
      by = "orgunit_level") %>%
    dplyr::select(orgunit_uid, orgunit_code, orgunit_level, orgunit_label, orgunit_name,
                  orgunit_parent_uid, orgunit_parent_name,
                  moh_id, tidyselect::everything())

  # Set a reference data sets

  .df_ref_orgs <- .orgs %>%
    dplyr::select(orgunit_uid, orgunit_name,
                  orgunit_level, orgunit_label,
                  orgunit_parent_uid, orgunit_parent_name)

  .df_ref_orgs <- .df_ref_orgs %>%
    dplyr::left_join(
      .df_ref_orgs[, c("orgunit_uid", "orgunit_name",
                       "orgunit_level", "orgunit_label")],
      by = c("orgunit_parent_uid" = "orgunit_uid",
             "orgunit_parent_name" = "orgunit_name"),
      keep = FALSE,
      suffix = c("", "_parent")) %>%
    dplyr::rename_with(
      .cols = tidyselect::ends_with("_parent"),
      .fn = ~ stringr::str_remove(str_replace(., "orgunit_", "orgunit_parent_"), "_parent$")
    )

  # Set addtional datasets

  .df_ext_orgs <- .orgs %>%
    dplyr::select(orgunit_uid, orgunit_code, moh_id)

  # Add full hierarchy

  .orgs_clean <- .orgs %>%
    dplyr::distinct(orgunit_level, orgunit_label) %>%
    dplyr::arrange(desc(orgunit_level)) %>%
    purrr::pmap(function(orgunit_level, orgunit_label){

      org_level = orgunit_level
      org_label = orgunit_label

      # Get a orgs for current levels
      .df_lvl <- .orgs %>%
        dplyr::select(orgunit_uid, orgunit_name, orgunit_level, orgunit_label) %>%
        dplyr::filter(orgunit_level == org_level, orgunit_label == org_label)

      # Append all parents
      .orgs %>%
        dplyr::arrange(desc(orgunit_level)) %>%
        dplyr::filter(orgunit_level != max(orgunit_level)) %>%
        dplyr::distinct(orgunit_level, orgunit_label) %>%
        purrr::pwalk(function(orgunit_level, orgunit_label) {

          parent_level <- orgunit_level
          parent_label <- orgunit_label

          # Get Previous org level / label
          child_label <-.orgs %>%
            dplyr::distinct(orgunit_level, orgunit_label) %>%
            dplyr::arrange(desc(orgunit_level)) %>%
            dplyr::mutate(child_label = lag(orgunit_label, 1)) %>%
            dplyr::filter(orgunit_label == parent_label, !is.na(child_label)) %>%
            dplyr::pull(child_label)

          #print(paste0("Parent <==> ", parent_label, " [", child_label, "]"))

          # Build dynamic join columns

          juid <- NULL
          jname <- NULL
          jlvl <- NULL
          jlbl <- NULL

          if (child_label == "facility" & any(stringr::str_detect(names(.df_lvl), "orgunit_"))) {
            juid = "orgunit_uid"
            jname = "orgunit_name"
            jlvl = "orgunit_level"
            jlbl = "orgunit_label"
          }
          else if (child_label != "facility" && any(stringr::str_detect(names(.df_lvl), child_label))) {
            juid <- paste0(child_label, "_uid")
            jname <- paste0(child_label, "_name")
            jlvl <- paste0(child_label, "_level")
            jlbl <- paste0(child_label, "_label")
          }

          # Add parent elements

          #print(paste0(c(juid, jname, jlvl, jlbl), collapse = "; "))

          if (!is.null(juid) & org_level < parent_level) {

            #print(paste0(org_label, " >>> ", parent_label))

            .df_lvl <<- .df_lvl %>%
              dplyr::left_join(dplyr::filter(.df_ref_orgs, orgunit_label == child_label),
                               by = c("orgunit_uid", "orgunit_name",
                                      "orgunit_level", "orgunit_label"),
                               relationship = "many-to-one") %>%
              dplyr::mutate(dplyr::across(tidyselect::matches("orgunit_parent_(uid|level|label)"), ~ "~")) %>%
              dplyr::mutate(dplyr::across(tidyselect::matches("orgunit_parent_(name)"),
                                   ~ paste0("Orgunit is above ", parent_label, " level"))) %>%
              dplyr::rename_with(.cols = tidyselect::contains("_parent_"),
                                 .fn = ~ stringr::str_replace(., "orgunit_parent", parent_label))
          }
          else if (!is.null(juid) & org_level == parent_level) {

            #print(paste0(org_label, " === ", parent_label))

            .df_lvl <<- .df_lvl %>%
              dplyr::left_join(filter(.df_ref_orgs, orgunit_label == child_label),
                               by = c("orgunit_uid", "orgunit_name",
                                      "orgunit_level", "orgunit_label"),
                               relationship = "many-to-one") %>%
              dplyr::mutate(
                orgunit_parent_uid = orgunit_uid,
                orgunit_parent_name = orgunit_name,
                orgunit_parent_level = orgunit_level,
                orgunit_parent_label = orgunit_label
              ) %>%
              dplyr::rename_with(.cols = tidyselect::contains("_parent_"),
                                 .fn = ~ stringr::str_replace(., "orgunit_parent", parent_label))
          }
          else if (!is.null(juid)) {

            #print(paste0(org_label, " <<< ", parent_label))

            .df_lvl <<- .df_lvl %>%
              dplyr::left_join(filter(.df_ref_orgs, orgunit_label == child_label),
                               by = stats::setNames(c("orgunit_uid", "orgunit_name",
                                                      "orgunit_level", "orgunit_label"),
                                                    c(juid, jname, jlvl, jlbl)),
                               relationship = "many-to-one") %>%
              dplyr::rename_with(.cols = tidyselect::contains("_parent_"),
                                 .fn = ~ stringr::str_replace(., "orgunit_parent", parent_label))
          }

        })

      return(.df_lvl)

    }) %>%
    purrr::list_rbind() %>%
    dplyr::rename(level = orgunit_level, orgunit_type = orgunit_label) %>%
    dplyr::rename_with(.fn = ~ stringr::str_remove(., "_name$")) %>%
    dplyr::rename_with(.fn = ~ stringr::str_replace(., "prioritization", "psnu")) %>%
    dplyr::rename_with(.fn = ~ stringr::str_replace(., "_uid$", "uid")) %>%
    dplyr::select(-tidyselect::contains(c("_level", "_label"))) %>%
    dplyr::left_join(.df_ext_orgs, by = c("orgunituid" = "orgunit_uid")) %>%
    dplyr::relocate(orgunit_code, moh_id, .after = orgunituid)

  return(.orgs_clean)
}


#' @title Extract Mechanisms infos from Datim
#'
#' @param cntry    Country name
#' @param username Datim username
#' @param password Datim password
#' @param agency   Agency name
#' @param baseurl  Datim API Base URL
#'
#' @export
#' @return OU Mechanisms as data frame
#'
#' @examples
#' \dontrun{
#'   library(grabr)
#'
#'   datim_mechs(
#'    cntry = "Mozambique",
#'    username = glamr::datim_user(),
#'    password = glamr::datim_pwd()
#'   )
#' }
datim_mechs <- function(cntry, username, password,
                        agency = "USAID",
                        baseurl = NULL) {

  user_params <- list("ou" = cntry)

  if (!is.null(agency)) user_params['agency'] = agency

  .mechs <- datim_sqlviews(
    username,
    password,
    view_name = "Mechanisms partners agencies OUS Start End",
    dataset = TRUE,
    query = list(
      type = "field",
      params = user_params
    ),
    baseurl = baseurl
  )

  # Reshape Results - mech code, award number, and name separations chars
  sep_chrs <- c("[[:space:]]+",
                "[[:space:]]+-",
                "[[:space:]]+-[[:space:]]+",
                "[[:space:]]+-[[:space:]]+-",
                "[[:space:]]+-[[:space:]]+-[[:space:]]+",
                "[[:space:]]+-[[:space:]]+-[[:space:]]+-",
                "-[[:space:]]+",
                "-[[:space:]]+-",
                "-[[:space:]]+-[[:space:]]+",
                "-[[:space:]]+-[[:space:]]+-",
                "-[[:space:]]+-[[:space:]]+-[[:space:]]+",
                "-[[:space:]]+-[[:space:]]+-[[:space:]]+-")

  # Reshape Results - separation
  .mechs %>%
    dplyr::rename(
      mech_code = code,
      operatingunit = ou,
      prime_partner_name = partner,
      prime_partner_uid = primeid,
      funding_agency = agency,
      operatingunit = ou
    ) %>%
    dplyr::mutate(
      mech_name = stringr::str_remove(mechanism, mech_code),
      mech_name = stringr::str_replace_all(mech_name, "\n", ""),
      award_number = dplyr::case_when(
        stringr::str_detect(prime_partner_name, "^TBD") ~ NA_character_,
        TRUE ~ stringr::str_extract(
          mech_name,
          pattern = "(?<=-[:space:])[A-Z0-9]+(?=[:space:]-[:space:])"
        )
      ),
      mech_name = dplyr::case_when(
        !is.na(award_number) ~ stringr::str_remove(mech_name, award_number),
        TRUE ~ mech_name
      ),
      mech_name = stringr::str_remove(
        mech_name,
        paste0("^", rev(sep_chrs), collapse = "|")
      )
    ) %>%
    dplyr::select(uid, mech_code, mech_name, award_number, mechanism, everything())
}
