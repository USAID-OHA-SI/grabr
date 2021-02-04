#' @title Get S3 Buckets list
#'
#' @param access_key S3 Access Key ID
#' @param secret_key S3 Secret Access Key
#'
#' @return S3 Backets list as tibble
#' @export
#'
#' @examples
#' \dontrun{
#' s3_buckets() }
#'
s3_buckets <- function(access_key = NULL,
                       secret_key = NULL) {

  # Check keys
  if (is.null(access_key))
    access_key = glamr::get_s3key("access")

  if (is.null(secret_key))
    secret_key = glamr::get_s3key("secret")

  # Get S3 Buckets as tibble
  aws.s3::bucket_list_df(
      key = access_key,
      secret = secret_key
    ) %>%
    dplyr::as_tibble() %>%
    janitor::clean_names()
}



#' @title Get S3 Bucket objects list
#'
#' @param bucket      S3 Bucket name
#' @param prefix      Limit response by key
#' @param n           Max number of record, default = 1000
#' @param access_key  S3 Access Key ID
#' @param secret_key  S3 Secret Access Key
#'
#' @return S3 Objects list as tibble
#' @export
#'
#' @examples
#' \dontrun{
#' s3_objects("sample-bucket") }
#'
s3_objects <- function(bucket,
                       prefix = "ddc/uat",
                       n = 1000,
                       access_key = NULL,
                       secret_key = NULL) {

  # Check keys
  if (is.null(access_key))
    access_key = glamr::get_s3key("access")

  if (is.null(secret_key))
    secret_key = glamr::get_s3key("secret")

  # Get & clean objects
  objects <- aws.s3::get_bucket_df(
      bucket = bucket,
      prefix = prefix,
      max = n,
      key = access_key,
      secret = secret_key
    ) %>%
    dplyr::as_tibble() %>%
    janitor::clean_names() %>% #glimpse()
    dplyr::rename(etag = e_tag) %>%
    dplyr::mutate(
      etag = stringr::str_remove_all(etag, '\"'),
      last_modified = lubridate::ymd(base::as.Date(last_modified)),
      size = base::as.integer(size)
    ) %>%
    dplyr::relocate(bucket, .before = 1)

  return(objects)
}


#' @title Unpack Objects Key
#'
#' @param df_objects   S3 objects as df
#' @param rmv_sysfiles Remove System object (logs, git folders, etc.)?
#' @param rmv_hidden   Remove Hidden objects (folders/files starting with dot) ?
#'
#' @return S3 Cleaned Objects list as tibble
#' @export
#'
#' @examples
#' \dontrun{
#' s3_objects("sample-bucket") %>% s3_unpack_keys()}
#'
s3_unpack_keys <-
  function(df_objects,
           rmv_sysfiles = TRUE,
           rmv_hidden = TRUE) {

    # Identify all prefixes (subfolders)
    paths <- df_objects %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        ndir = stringr::str_count(key, "/") + 1
      ) %>%
      dplyr::distinct(ndir) %>%
      dplyr::pull(ndir) %>%
      base::sort()

    # Set new colnames
    s3_paths <- base::paste0("path", base::seq(1, base::max(paths), 1))

    s3_paths_clean <- c("system",
                        "sys_env",
                        "sys_data_type",
                        "sys_use_case",
                        "sys_data_route",
                        "sys_data_object")

    # Stop here for non ddc sys bucket
    if (length(s3_paths) < length(s3_paths_clean))
      return(df_objects)

    # Continue
    s3_paths[1:length(s3_paths_clean)] <- s3_paths_clean

    # Separate paths & document
    df_objects <- df_objects %>%
      tidyr::separate(key, into = s3_paths, sep = "/",
                      remove = FALSE, fill = "right")

    # remove system files/folders
    if (rmv_sysfiles) {

      data_type <- s3_paths[3]

      df_objects <- df_objects %>%
        dplyr::filter(!!dplyr::sym(data_type) %in% c("raw", "processed"))
    }

    # remove hidden files / path (eg: .trifacta)
    if (rmv_hidden) {

      data_route <- s3_paths[5]
      data_object <- s3_paths[6]

      df_objects <- df_objects %>%
        dplyr::filter(
          !stringr::str_detect(dplyr::sym({{data_route}}), "^[.]"),
          !stringr::str_detect(dplyr::sym({{data_object}}), "^[.]")
        )
    }

    return(df_objects)
  }


#' @title Idenfity S3 Object type
#'
#' @param object S3 Object key
#'
#' @return file type: text, csv, excel, json, python, shell
#' @export
#'
#' @examples
#' \dontrun{
#' s3_objects("sample-bucket") %>%
#'   filter(str_detect(key, "^HFR")) %>%
#'   pull(key) %>%
#'   first() %>%
#'   s3_object_type()}
#'
s3_object_type <- function(object) {

  # idenfity Object type
  object_type = NULL

  if (stringr::str_ends(object, ".csv"))
    object_type = "csv"

  if (stringr::str_ends(object, ".xls|.xlsx"))
    object_type = "excel"

  if (stringr::str_ends(object, ".json"))
    object_type = "json"

  if (stringr::str_ends(object, ".txt"))
    object_type = "text"

  if (stringr::str_ends(object, ".Rdata|.Rda|.Rds"))
    object_type = "r"

  return(object_type)
}


#' @title Read content of S3 Objects
#'
#' @param bucket     S3 Bucket name
#' @param object     S3 Object key (id)
#' @param sheet      S3 Excel object sheet name / index
#' @param access_key S3 Access key id
#' @param secret_key S3 Secret Access key
#'
#' @return df
#' @export
#'
#' @examples
#' \dontrun{
#' s3_objects("sample-bucket") %>%
#'   filter(str_detect(key, "^HFR")) %>%
#'   pull(key) %>%
#'   first() %>%
#'   s3_read_object(bucket = "sample-bucket", object_key = .)}
#'
s3_read_object <- function(bucket, object,
                           sheet = NULL,
                           access_key = NULL,
                           secret_key = NULL) {

  # Check keys
  if (is.null(access_key))
    access_key = glamr::get_s3key("access")

  if (is.null(secret_key))
    secret_key = glamr::get_s3key("secret")

  # Object type
  object_type = s3_object_type(object)

  # Notification
  usethis::ui_info(base::paste0("TYPE - S3 Object type is: ", object_type))

  # Get object as raw data
  object_raw <- aws.s3::get_object(
    bucket = bucket,
    object = object,
    key = access_key,
    secret = secret_key
  )

  # Create connection to raw data
  conn <- base::rawConnection(object_raw, open = "r")

  # Close conn when done
  #base::on.exit({base::close(conn)})

  # Data
  df = NULL

  # Read content of raw data as tibble
  if (object_type == "excel") {

    # Notification
    usethis::ui_info("PROCESS - Downloading S3 Object ...")

    tmpfile <- base::tempfile(fileext = ".xlsx")

    # Save object to temp file
    s3_obj <- aws.s3::save_object(
      bucket = bucket,
      object = object_key,
      file = tmpfile,
      key = access_key,
      secret = secret_key
    )

    # Destination file
    usethis::ui_info(paste0("FILE - " , s3_obj))

    # Read file content
    usethis::ui_info("PROCESS - Reading data with {usethis::ui_code('Wavelength::hfr_import()')}")

    #df <- Wavelength::hfr_import(tmpfile)
    df <- NULL

    if (!is.null(sheet)) {
      df <- tempfile %>%
        readxl::read_excel(filepath,
                           sheet = sheet,
                           col_types = "text")
    }
    else {
      df <- tmpfile %>%
        readxl::excel_sheets() %>%
        stringr::str_subset(stringr::str_to_upper(.), "HFR") %>%
        purrr::map_dfr(.f = ~ readxl::read_excel(filepath,
                                                 sheet = .x,
                                                 skip = 1,
                                                 col_types = "text"))
    }

    # Close connection
    base::close(conn)

    # Clean up
    file.remove(tmpfile)

    # notify
    usethis::ui_info("PROCESS - Temp file removed")
  }
  else {
    # notify
    usethis::ui_info("PROCESS - Reading data with {usethis::ui_code('vroom::vroom()')}")

    # Read other file type with vroom
    df <- vroom::vroom(conn, col_types = c(.default = "c"))
  }

  # notify
  usethis::ui_info("PROCESS - Completed!")

  return(df)
}


#' @title Download S3 Objects
#'
#' @param bucket     S3 Bucket name
#' @param object     S3 Object key (id)
#' @param filepath   Full path of destination file
#' @param access_key S3 Access key id
#' @param secret_key S3 Secret Access key
#'
#' @return file name
#' @export
#'
#' @examples
#' \dontrun{
#' s3_objects("sample-bucket") %>%
#'   filter(str_detect(key, "^HFR")) %>%
#'   pull(key) %>%
#'   first() %>%
#'   s3_download()}
#'
s3_download <-
  function(bucket, object,
           filepath = NULL,
           access_key = NULL,
           secret_key = NULL) {

    # Check keys
    if (is.null(access_key))
      access_key = glamr::get_s3key("access")

    if (is.null(secret_key))
      secret_key = glamr::get_s3key("secret")

    # Notification
    usethis::ui_info("PROCESS - Downloading S3 Object ...")

    # temp file name
    if (is.null(filepath)) {
      filepath <- tools::file_ext(object) %>%
        paste0(".", .) %>%
        base::tempfile(fileext = .)
    }

    # Save object to temp file
    s3_obj <- aws.s3::save_object(
      bucket = bucket,
      object = object,
      file = filepath,
      key = access_key,
      secret = secret_key
    )

    # Destination file
    usethis::ui_info(paste0("FILE - ", s3_obj))

    # Done
    usethis::ui_info("PROCESS - Completed")

    return(s3_obj)
  }


#' @title            Upload file to S3 Bucket
#' @param filepath   Source file path
#' @param bucket     S3 backet name
#' @param prefix     S3 Prefix (folder structure)
#' @param object     Destination S3 object name (with file extention)
#' @param access_key S3 Access Key
#' @param secret_key S3 Secret Key
#'
#' @return boolean
#' @export
#'
#' @examples
#' \dontrun{
#' filepath %>% s3_upload(bucket = "test-bkt")}
#'
s3_upload <- function(filepath, bucket,
                      prefix = "",
                      object = NULL,
                      access_key = NULL,
                      secret_key = NULL) {

  # Check S3 keys
  if (is.null(access_key))
    access_key = glamr::get_s3key("access")

  if (is.null(secret_key))
    secret_key = glamr::get_s3key("secret")

  # s3 object: append prefix to file basename
  s3_object <- ifelse(is.null(object),
                      base::file.path(prefix, base::basename(filepath)),
                      object)

  # put object
  aws.s3::put_object(
    file =  filepath,
    object = s3_object,
    bucket = bucket,
    key = access_key,
    secret = secret_key
  )

}


#' @title Remove objects from S3 bucket
#'
#' @param objects S3 object keys (full path)
#' @param bucket     S3 backet name
#' @param access_key S3 Access Key
#' @param secret_key S3 Secret Key
#'
#'
#' @return boolean
#' @export
#'
#' @examples
#' \dontrun{
#' df_objects %>%
#'   pull(key) %>%
#'   first() %>%
#'   s3_remove(objects = .,
#'             bucket = "test-bkt")}
#'
s3_remove <- function(objects, bucket,
                      access_key = NULL,
                      secret_key = NULL) {

  # Check S3 keys
  if (is.null(access_key))
    access_key = glamr::get_s3key("access")

  if (is.null(secret_key))
    secret_key = glamr::get_s3key("secret")

  # delete objects from bucket
  aws.s3::delete_object(
    object = objects,
    bucket = bucket,
    key = get_s3key("access"),
    secret = get_s3key("secret")
  )
}