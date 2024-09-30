# grabr 2.1
* Resolve warning message that arose with `s3_objects` as a result of convering the file size using `as.integer` instead of `as.numeric` [2024-09-30]
* Address bug in `wave_process_query` relating to a missing and renamed objects [2024-05-30]
* Add functionality to pull data from PDAP Wave (new Genie API), `wave_process_query` [2024-05-10]

# grabr 2.0
* Update `lazy_secrets` to handle PDAP when working on PEPFAR Posit Workbench [2024-04-02]
* Resolving missing explicit references [2024-04-02]
* Add list of global variables [2024-04-02]

# grabr 1.2.3
* Adding new articles for data look up and extraction
* Resolve bug with `pano_items` which still used `session` as a parameter affecting `pano_extract` upstread [2024-01-08]
* Change instructions to install from rOpenSci [2024-01-04]
* Exclude `pano_session()`, `pano_content()` and `pano_elements()` form list of function exported [2023-12-12]
* Update `datim_*` functions to use `datim_execute_query()` that uses `tryCatch` for errors [2023-08-04] 
* Add `clean_orgunits()` to clean orgunits from `datim_orgunits()` [2023-12-12]
* Add `get_baseurl()` to clean up urls and remove trailing slashes [2023-08-04]
* Renamed `pull_hierarchy()` to `datim_pull_hierarchy()` [2023-12-12]
* Copied `pull_hierarchy()`, `pull_mech()` and `pull_mer()` along with their respective dependencies to this package [2023-08-04]

# grabr 1.1.0
* Update `datim_execute_query` to resolve new behavior from `urltools::url_encode` that started changing character other than spaces, having unexpected change to APIs [2022-06-22] 

# grabr 1.0.1
* Adding new datim sqlview functions: `datim_sqlview()` and `datim_orgunits()` [2023-04-14]
* More of these to be migrated from `MerQL` repo to this packages

# grabr 1.0.0
* Resolves a bug caused by a dplyr update when `s3_unpack_keys` is called from `s3_objects` [2023-02-16] 
* Resolve bug with `lazy_secrets` where there are too many default param by using `match.arg` [2023-02-09]
* Limit credential pop up to once when running `get_outable()` [2023-01-26]
* Clean up dependencies and remove `check_package()` which was conflicting with `glamr` [2022-12-01] 
* Replace `%ni%` with `! .. %in%..` as you cannot use an imported object from a GitHub package, `glamr` [2022-11-21] 
* Resolve outstanding issues around Pano folder name change affecting `pano_extract_msd` and `pano_extract_msds` [2022-09-29] 
* Adjust for Pano folder name change affecting `pano_extract` [2022-09-26]
* Resolve bug with `lazy_secrets` causing upstream errors with `get_ouuid` and other dependent functions [2022-09-01]
* Update `pano_extract` to provide current period information if missing in parameters [2022-08-22]
* Resolve Pano folder name change affecting `pano_extract` [2022-08-22]
* Add dependencies to `glamr` and `gagglr` [2022-08-15]
* Implement new internal function to handle authentication in decision tree - check for user provided, then check if stored via glamr, and lastly prompt for creds [2022-08-05]
* Clean up package/site [2022-08-04]
* Migrate full package over from `glamr` and truncate to API related functions [2022-08-04]
