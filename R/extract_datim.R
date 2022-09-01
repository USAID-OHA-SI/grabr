#' @title Get PEPFAR/DATIM dimensions
#'
#' @param url      DATIM API End point
#' @param username DATIM Account Username
#' @param password DATIM Account passward
#' @param var      Column name to pull all values from, default is NULL, options are: id, dimension
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
datim_dimensions <- function(url = "https://final.datim.org/api/dimensions",
                             username,
                             password,
                             var = NULL) {

  # datim credentials
  accnt <- lazy_secrets("datim", username, password)

  # paging
  if (!stringr::str_detect(url, "paging"))
    url <- base::paste0(url, "?paging=false")

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
#' @param url       DATIM API End point, default value is `https://final.datim.org/api/dimensions`
#' @param username  DATIM Account Username, recommended using glamr::datim_user()`
#' @param password  DATIM Account passward, recommended using glamr::datim_pwd()`
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
                            url = "https://final.datim.org/api/dimensions",
                            username,
                            password) {

  dim_id <- NULL

  # datim credentials
  accnt <- lazy_secrets("datim", username, password)

  # Query dimensions
  df_dims <- datim_dimensions(url = url,
                              username = accnt$username,
                              password = accnt$password)

  if (base::is.null(df_dims) | base::nrow(df_dims) == 0 | name %ni% df_dims$dimension) {
    base::message(crayon::red(glue::glue("There is no '{name}' dimension. Check spelling.")))
    return(NULL)
  }

  dim_id = df_dims %>%
    dplyr::filter(dimension == name) %>%
    dplyr::pull(id)

  return(dim_id)
}


#' @title Get PEPFAR/DATIM Dimension Items
#'
#' @param dimension Dimension name
#' @param var       column name to pull values from, id or item
#' @param fields    list of column names to return, this will overight `var`
#' @param url       DATIM API end point
#' @param username  DATIM Account Username
#' @param password  DATIM Account passward
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
                            var = NULL,
                            fields = NULL,
                            url = "https://final.datim.org/api/dimensions",
                            username,
                            password){

  # datim credentials
  accnt <- lazy_secrets("datim", username, password)

  # clean up url / paging
  url <- stringr::str_remove(url, "\\?.*")

  # Get dimension id
  dim_id <- datim_dimension(url = url,
                            name = dimension,
                            username = accnt$username,
                            password = accnt$password)

  # Update url with id and paging
  url_dims <- glue::glue("{url}/{dim_id}/items?paging=false")

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
#' @param url       DATIM API end point
#' @param username  DATIM Account Username
#' @param password  DATIM Account passward
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
                           url = "https://final.datim.org/api/dimensions",
                           username = NULL,
                           password = NULL) {

  item_id <- NULL

  # datim credentials
  accnt <- lazy_secrets("datim", username, password)

  # Get dimension's items
  items <- datim_dim_items(dimension = dimension,
                           url = url,
                           username = accnt$username,
                           password = accnt$password)

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
#' @param url       DATIM API End point
#' @param username  DATIM Account Username
#' @param password  DATIM Account passward
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
                          url = "https://final.datim.org/api/dimensions",
                          username,
                          password) {

  # datim credentials
  accnt <- lazy_secrets("datim", username, password)

  # Query params
  dim_query <- NULL

  # Get specific items from dimension
  if (!base::is.null(items)) {

    dim_id <- datim_dimension(name = dimension,
                              url = url,
                              username = accnt$username,
                              password = accnt$password)

    dim_query <- items %>%
      purrr::map(function(item) {

        dim <- datim_dim_item(dimension = dimension,
                              name = item,
                              url = url,
                              username = accnt$username,
                              password = accnt$password) %>%
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
                                url = url,
                                username = accnt$username,
                                password = accnt$password)

      dim_items <- datim_dim_items(dim) %>%
        dplyr::pull(item) %>%
        purrr::map(~datim_dim_item(dimension = dim,
                                   name = .x,
                                   url = url,
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
#' @return returns query results as json object
#' @export
#'
#' @examples
#' \dontrun{
#'   library(grabr)
#'
#'   execute_datim_query(
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
  # datim credentials
  accnt <- lazy_secrets("datim", username, password)

  # Run query
  json <- base::tryCatch({
    url %>%
      httr::GET(httr::authenticate(accnt$username, accnt$password)) %>%
      httr::content("text") %>%
      jsonlite::fromJSON(flatten = flatten)
  },
  warning = function(warn) {
    base::message(crayon::red("Query execution warnings:"))
    base::print(warn)
  },
  error = function(err) {
    base::message(crayon::red("Unable to execute your query:"))
    base::print(err)
    return(NULL)
  })

  return(json)
}


#' @title Process Datim Query results
#'
#' @param url       Datim API Call url
#' @param username  Datim username, recommend using `glamr::datim_user()`
#' @param password  Datim password, recommend using `glamr::datim_pwd()`
#'
#' @return Data as tibble
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
  res_json <- datim_execute_query(url, accnt$username, accnt$password)

  # check for valid json result
  if ( base::is.null(res_json)) {
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
  function(ou,
           level = "prioritization",
           pe = "THIS_FINANCIAL_YEAR",
           ta = "PLHIV",
           value = NULL,
           disaggs = NULL,
           dimensions = NULL,
           property = "SHORTNAME",
           metadata = TRUE,
           hierarchy = TRUE,
           baseurl = "https://final.datim.org/api",
           username,
           password,
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
    ou_uid <- get_ouuid(ou, username = accnt$username, password = accnt$password)

    if (base::is.null(ou_uid)) {
      base::message(crayon::red("OU/Country [{ou}] is invalid or unavailable"))
      return(NULL)
    }

    # Org Level
    org_lvl <- get_ouorglevel(ou, org_type = level,
                              username = accnt$username,
                              password = accnt$password)

    if (base::is.null(org_lvl)) {
      base::message(crayon::red("Org level [{level}] is invalid or unavailable"))
      return(NULL)
    }

    # ou/org level
    url_core <-
      paste0(baseurl, "/29/analytics?",
             "dimension=ou:", ou_uid, ";LEVEL-", org_lvl)

    # Notifications
    if (verbose) {
      base::print(glue::glue("CORE URL = {url_core}"))
    }

    # Targets/Results
    dim_tr_name <- "Targets / Results"

    dim_tr <- datim_dimension(name = dim_tr_name)

    # Get value types
    if (!base::is.null(value)) {
      # Get user's options
      dim_tr_value <- value %>%
        purrr::map(~datim_dim_item(dimension = dim_tr_name, name = .x)) %>%
        base::unlist() %>%
        base::paste0(collapse = ";")

    } else {
      # Default options: use all items
      dim_tr_value <- datim_dim_items(dimension = dim_tr_name, var = "item") %>%
        purrr::map(~datim_dim_item(dimension = "Targets / Results", name = .x)) %>%
        base::unlist() %>%
        base::paste0(collapse = ";")
    }

    # Indicators
    dim_ta <- datim_dimension(name = "Technical Area")
    dim_ta_ind <- datim_dim_item(dimension = "Technical Area", name = ta)

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
        purrr::map(~datim_dim_url(dimension = "Disaggregation Type", items = .x)) %>%
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
        purrr::map(~datim_dim_url(dimension = .x)) %>%
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
#' @param level     Organization level
#' @param fy        Fiscal Year
#' @param hierarchy Should additional organizational hierarchy be added?, default is FALSE
#' @param username       Datim account username
#' @param password       Datim account password
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
datim_pops <- function(ou,
                       level = "country",
                       fy = NULL,
                       hierarchy = FALSE,
                       username,
                       password) {

  # datim credentials
  accnt <- lazy_secrets("datim", username, password)

  # level
  lvl <- level %>% stringr::str_to_lower()

  if (lvl %ni% c("country", "psnu", "prioritization")) {
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
                password = accnt$password)

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
                hierarchy = hierarchy)

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
            org_level = stringr::str_extract(.x, "\\d$"))

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
