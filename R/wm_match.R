# Pipeline for validating taxonomic names against WoRMS using 
#'
#' Pipeline for cleaning, matching, validating, and updating taxonomic names with
#' efficient handling of duplicate names, fuzzy matching, and referral to accepted 
#' synonyms or parents. Output needs reviewing!  
#' 
#'
#' @param taxa Taxa names to be validated, should not contain any NAs.
#' @param eval_as Named vector of potential substitute names for matching.
#'   Use `NULL` for no overrides. The original default is retained.
#' @param prio_marine Logical; prefer marine records when available. Defaults
#'   to `TRUE`.
#' @param prio_accepted Logical; prefer accepted records when available.Defaults
#'   to `TRUE`
#' @param exclude_extinct Logical; discard explicitly extinct records.Defaults
#'   to `TRUE`
#' @param batch_size Integer from 1 to 50; maximum names per exact-search
#'   request. Defaults to 49. The function does not validate this argument.
#' @return A data.frame with the original 17 validation columns, one row per
#'   distinct input name, in first-occurrence order. Populated Aphia IDs are
#'   character; columns containing only missing values may remain logical.
#' @details Cleaning trims white space, collapses repeated spaces, removes
#'   common qualifiers and parenthetical text, applies `eval_as` overrides,
#'   then removes punctuation. Distinct original names remain separate even
#'   when they share a cleaned lookup name.
#'
#'   Exact searches use [worrms::wm_records_names()] with its default marine
#'   restriction. Missing exact matches and responses containing 50 records
#'   fall back to individual TAXAMATCH requests with `marine_only = FALSE`.
#'   Candidates are filtered in this order: exclude explicitly extinct records,
#'   prefer marine records, then prefer accepted records. Remaining ties use the
#'   first record. Multiple-match labels reflect candidate counts before these
#'   preferences. Setting `prio_marine = FALSE` does not broaden exact searches.
#'
#'   Accepted or parent records are fetched individually when their identifier
#'   differs from the matched identifier; lookups are not cached. A failed exact
#'   batch stops the function. Errors inside the per-name lookup and taxonomy
#'   steps are generally suppressed by `try()`. If an accepted-record lookup
#'   fails, fields can still come from the matched record. Missing classification
#'   alone does not necessarily raise a flag. Review all results, including
#'   missing taxonomy, before using them in an analysis.
#'
#'   Progress messages report exact batches and original names and row positions
#'   needing fuzzy matching. Elapsed time is printed before a review reminder.
#'   Empty input and invalid batch sizes are unsupported.
#' @section Output columns:
#' \describe{
#'   \item{flag}{Numeric review indicator. 
#'               0: matched to accepted taxa without any changes.
#'               1: matched to accepted taxa with minor changes or multiple matching  - e.g. corrected typo in name.
#'               2: review necessary - e.g. no match, missing match method, changed identifier or rank, or a matched
#'               status other than accepted. Flag 2 takes precedence over flag 1.}
#'   \item{input_taxa}{Original string supplied to the function.}
#'   \item{eval_taxa}{String as assessed via API, potentially a substitute provided in the eval_as argument.}
#'   \item{match_taxa, worms_match, match_worms_status}{Matched name, match
#'     method, and the matched record's taxonomic status.}
#'   \item{match_aphia_id, match_rank}{Matched record's identifier and rank.}
#'   \item{valid_taxa, valid_aphia_id, valid_rank}{Accepted or parent record's
#'     name, identifier, and rank. See the error-handling caveat above.}
#'   \item{valid_phylum, valid_class, valid_order, valid_family, valid_genus}{
#'     Classification fields from the accepted or parent record.}
#'   \item{valid_species}{Accepted name when its rank is Species; otherwise NA.}
#' }
#' @examples
#' \dontrun{
#' validation <- wm_match(c("Abra alba", "Bivalvia Unid. (juv)"),
#'                        eval_as = c("Bivalvia Unid. (juv)" = "Bivalvia"))
#' validation[validation$flag > 0, ]
#' }
#' @seealso [add_wm_taxonomy()]
#' @export


wm_match <- function(taxa, eval_as = c("default" = "default") , prio_marine = TRUE, prio_accepted = TRUE, exclude_extinct = TRUE, batch_size = 49) { 
  
  # check for any NA
  if(anyNA(taxa)){stop("Error: Taxa should not contain any NAs.")}
  
  
  # setting system time to calculate time the function is running
  old <- Sys.time()  
  
  
  ###################################################################################################
  # INPUT CLEANING #
  ###################################################################################################
  
  checkUTF8 <- function(df, cols){
    # UTF encoding check
    for (col in cols){
      if (!all(stri_enc_isutf8(na.omit(df[[col]])))){
        df[[col]] <- stri_encode(df[[col]], from = "", to = "UTF-8")
        # If UTF-8 encoding not all correct, issue a warning
        if (!all(stri_enc_isutf8(na.omit(df[[col]])))){
          warning(paste("Check", col, "for data encoding: Is it UTF-8?"))
        }
      }
    }
    return(df)
  }
  
  
  # create data frame from input, check for UTF8 encoding 
  data <- 
    data.frame(flag = rep(0, length(taxa)), input_taxa = taxa) %>%
    checkUTF8(c("input_taxa"))
  
  # remove surplus spaces
  # add taxa so that it can be added to a URL;  abbr. such as sp., spp., cf. are removed  
  data <- 
    data%>%    
    mutate(eval_taxa = str_trim(input_taxa, side = "both") ) %>% # remove leading and trailing spaces
    mutate(eval_taxa = gsub("\\s+", " ", eval_taxa ))%>%         # remove double spaces
    mutate(eval_taxa = mgsub(eval_taxa, c(" sp\\."," cf", " cf\\."," spp\\.", " spp"), c("", "", "","", "")))%>% # remove double spaces
    mutate(eval_taxa = str_remove_all(eval_taxa, "\\s*\\([^\\)]*\\)"))%>%  # remove brackets and anything within - as usually causes trouble with fuzzy matching
    mutate(eval_taxa = case_when(input_taxa %in% names(eval_as) ~ eval_as[input_taxa],
                                 TRUE ~ eval_taxa))%>%
    mutate(eval_taxa = gsub("[[:punct:]]", "", eval_taxa)) # remove all special characters such as ~!@#$%^&*(){}_+:\"<>?,./;'[]-=. Do this to eval_as names too as characters might brake the API link 
  
  # only keep unique combinations
  data <- distinct(data)
  
  
  ###################################################################################################
  # CREATING DF TO FILL #
  ###################################################################################################
  
  
  # adds columns to the df to be filled by the function 
  data$match_taxa <- NA
  data$worms_match <- NA
  data$match_aphiaID <- NA
  data$match_rank <- NA
  data$worms_status <- NA
  data$valid_aphiaID <- NA
  data$valid_taxa <- NA
  data$valid_rank <- NA
  data$valid_phylum <- NA
  data$valid_class <- NA
  data$valid_order <- NA
  data$valid_family <- NA
  data$valid_genus <- NA
  data$valid_species <- NA
  
  
  ###################################################################################################
  # Direct matching in batches of batch_size #
  ###################################################################################################
  
  # direct matching is done via the worrms package which allows for batch matching at a time.
  
  eval_taxa <- data$eval_taxa
  
  # how many batches of batch_size?
  batch_match_iterations <- ceiling(length(eval_taxa) / batch_size)
  
  worms_records <- list()
  
  message("Direct matching with WoRMS ...")
  
  for (it in c(1:batch_match_iterations)) {
    
    # define batch of batch_size names
    batch <- na.omit(eval_taxa[(1 + ((it - 1) * batch_size)):(batch_size + ((it - 1) * batch_size))])
    
    # failsafe in case batch matching fails
    records <- list()
    
    # batch matching. ONLY WORRMS DEPENDENCY. wrapped in try() in case it fails
    # try(silent = T, {records <- wm_records_names(name = batch)})
    records <- wm_records_names(name = batch)
    
    # sleep_time <- 10
    # 
    # repeat {
    #   try({
    #     records <- wm_records_names(name = batch)
    #     # If we reached here, no error happened — exit the loop
    #     break
    #   }, silent = TRUE)
    #   
    #   message(paste0("Error 403. Waiting ",sleep_time," seconds before retrying..."))
    #   Sys.sleep(sleep_time)
    #   sleep_time <- sleep_time*2
    #   if(sleep_time >40){
    #     message("Timed out... trying next batch")
    #     break
    #                      }
    # }
    
    
    
    
    # combine with previous batches
    worms_records <- c(worms_records, records)
    
    # message about batch progress
    message(paste0("Batch ", it, "/", batch_match_iterations))
    
  }
  
  # cut batch matching result to length
  worms_records <- worms_records[1:length(eval_taxa)]
  
  message("")
  message("Fuzzy matching with WoRMS ...")
  
  
  
  
  ###################################################################################################
  # Taxa LOOP #
  ###################################################################################################
  
  # this loop will extract results of the batch matching and perform fuzzy matching if necessary
  
  its <- c(1:length(eval_taxa))
  
  
  # for loop
  for ( i in its){
    
    suppressWarnings(rm(species, species2, species_record, n_species_records, url,url1, url2))
    
    # assigning the taxa 
    species <- eval_taxa[i]
    
    ###################################################################################################
    # Extract from batch matchin results #
    ###################################################################################################
    
    # the loop extracts the exact matching result from batch matching     
    
    try(silent = TRUE, {
      
      # select the result from batch matching and return dataframe
      species_record <- worms_records[[i]] %>% data.frame()
      
      # store number of matches, if =50 the WoRMS REST was maxed out and we need to repeat with fuzzy matching
      if (nrow(species_record) == 50) {
        api_maxed_out = TRUE
      } else{
        api_maxed_out = FALSE
      }
      
      ## handling multiple matches in WoRMS
      
      # only keep matches that exactly match the search taxa (e.g. remove individual species when Genus was the search term)
      species_record <- species_record[species_record$scientificname == species, ]
      
      # only keeping hits with information for scientificname and status (e.g. accepted)
      species_record <- species_record[!is.na(species_record$scientificname) & !is.na(species_record$status),]
      
      # count number of species records
      n_species_record <- nrow(species_record)
      
      ## applying priorities
      
      # exclude extinct species (default but can be switched off))
      if(exclude_extinct){
        species_record <- species_record[ species_record$isExtinct == 0 | is.na(species_record$isExtinct) ,]
      }
      
      # prioritize marine species (default but can be switched off)
      if(prio_marine & 1 %in% species_record$isMarine ){
        species_record <- species_record[species_record$isMarine == 1 & !is.na(species_record$isMarine),]
      }
      
      # if there is still multiple hits select the one that is status=="accepted" (if there is one) (default but can be switched off)
      if(prio_accepted & "accepted" %in% species_record$status ){
        species_record <- species_record[species_record$status == "accepted",]
      }
      
      # if there is still multiple matches simply select first row
      if (nrow(species_record) > 1) {
        species_record <- species_record[1, ]
      }
      
      # assign match type
      if (n_species_record > 1){  
        data$worms_match[i] <- "multiple_exact"
      } else{
        data$worms_match[i] <- "exact"
      }
      
      # removes 'species record' if the API was maxed out (n=50) earlier.
      if (api_maxed_out | nrow(species_record) == 0) {
        rm(species_record)
      }
    })
    #closing try-statement
    
    ###################################################################################################
    # WoRMS MATCHING LOOP : FUZZY MATCHING #
    ###################################################################################################
    
    # if exact matching did not return anything or was maxed out, we fall back on fuzzy matching
    # for now, this is done one at a time = slow
    
    try(silent = TRUE, {
      if (exists("species_record") == FALSE) {
        try(silent = TRUE, {
          
          # print taxa name and index in the vector after each iteration to show progress
          message(paste(data$input_taxa[i], paste(i, length(its) , sep = "/")))
          
          # using the 'species' object to format an URL for fuzzy matching REST API
          # replacing spaces and bracket in species name to paste into URL
          species2 <- gsub(" ", "%20", species)
          species2 <- gsub("[(]", "%28", species2)
          species2 <- gsub("[)]", "%29", species2)
          
          
          # URL for fuzzy matching is slightly different
          url1 <- "https://www.marinespecies.org/rest/AphiaRecordsByMatchNames?scientificnames%5B%5D=XXXX&marine_only=false"
          url2 <- gsub("XXXX", species2, url1)
          
          #species_record <- fromJSON(url2)
          
          resp <- GET(url2)#, user_agent("R-jsonlite/httr (WoRMS client)"))
          species_record <- fromJSON(content(resp, as = "text", encoding = "UTF-8"))
          
          # format of extracted species is a bit different (list), convert to df
          species_record <- species_record[[1]]
          
          
          ## handling multiple matches in WoRMS
          
          # only keeping hits with information for scientificname and status (e.g. accepted)
          species_record <- species_record[!is.na(species_record$scientificname) & !is.na(species_record$status),]
          
          # count number of species records
          n_species_record <- nrow(species_record)
          
          ## applying priorities
          
          # exclude extinct species (default but can be switched off))
          if(exclude_extinct){
            species_record <- species_record[ species_record$isExtinct == 0 | is.na(species_record$isExtinct) ,]
          }
          
          # prioritize marine species (default but can be switched off)
          if(prio_marine & 1 %in% species_record$isMarine ){
            species_record <- species_record[species_record$isMarine == 1 & !is.na(species_record$isMarine),]
          }
          
          # if there is still multiple hits select the one that is status=="accepted" (if there is one) (default but can be switched off)
          if(prio_accepted & "accepted" %in% species_record$status ){
            species_record <- species_record[species_record$status == "accepted",]
          }
          
          # if there is still multiple matches simply select first row
          if (nrow(species_record) > 1) {
            species_record <- species_record[1, ]
          }
          
          # assign match type
          if (n_species_record > 1) { 
            data$worms_match[i] <- paste0("multiple_fuzzy_" ,species_record$match_type)
          } else{
            data$worms_match[i] <- paste0("fuzzy_", species_record$match_type)
          }
          
        })
        #closing try-statement
        
      } #closing if-statement
    })
    #closing try-statement
    
    ###################################################################################################
    # WoRMS MATCHING LOOP : POPULATING results (1)
    ###################################################################################################
    
    try(silent = TRUE, {
      
      # storing some information from the match in data
      data$match_taxa[i] <- as.character(species_record$scientificname)
      
      # storing some information from the match in data
      data$match_aphiaID[i] <- as.character(species_record$AphiaID)
      
      data$match_rank[i] <- as.character(species_record$rank)
      
      # add valid aphia
      data$valid_aphiaID[i] <- as.character(species_record$valid_AphiaID)
      
      # if there is no valid aphiaID provided, this adds the aphia ID of the parent taxa
      if (is.na(data$valid_aphiaID[i])) {
        data$valid_aphiaID[i] <- species_record$parentNameUsageID
      }
      
      # add status
      data$worms_status[i] <- as.character(species_record$status)
      
    })
    # close try()
    
    ###################################################################################################
    # WoRMS MATCHING LOOP : PULLING ACCEPTED CLASSIFICATION #
    ###################################################################################################
    
    # now grab accepted aphiaID to pull record of accepted taxa if taxa was unaccepted
    try(silent = TRUE, {
      
      # if taxa is unaccepted but there exists a valid alternative aphiaID
      if (data$match_aphiaID[i] != data$valid_aphiaID[i]) {
        aphia <- data$valid_aphiaID[i]
        url1 <- "https://www.marinespecies.org/rest/AphiaRecordByAphiaID/XXXX"
        url2 <- gsub("XXXX", aphia, url1)
        
        # update species record
        
        #species_record <- fromJSON(url2)
        resp <- GET(url2)#, user_agent("R-jsonlite/httr (WoRMS client)"))
        species_record <- fromJSON(content(resp, as = "text", encoding = "UTF-8"))
        
        
      }
    })
    # close try()
    
    ###################################################################################################
    # WoRMS MATCHING LOOP : POPULATING results (2)
    ###################################################################################################
    
    # add taxonomic info based on the accepted taxa
    # everything individually wrapped in try() in case of incomplete info
    try(silent = TRUE, {
      try(silent = TRUE, {
        data$valid_taxa[i] <- species_record$valid_name
      })
      try(silent = TRUE, {
        data$valid_rank[i] <- species_record$rank
      })
      try(silent = TRUE, {
        data$valid_phylum[i] <- species_record$phylum
      })
      try(silent = TRUE, {
        data$valid_class[i] <- species_record$class
      })
      try(silent = TRUE, {
        data$valid_order[i] <- species_record$order
      })
      try(silent = TRUE, {
        data$valid_family[i] <- species_record$family
      })
      try(silent = TRUE, {
        data$valid_genus[i] <- species_record$genus
      })
      
      # Species is not specifically listed in worms record, if its rank=species use valid_taxa to populate species
      if (data$valid_rank[i] == "Species") {
        data$valid_species[i] <- data$valid_taxa[i]
      }
      
    })
    # close try()
    
    ###################################################################################################
    # WoRMS MATCHING LOOP : NO MATCH AND LOOP END
    ###################################################################################################
    
    # if neither exact nor fuzzy matching were successful adds information "no_match" and flags data
    try({
      if (exists("species_record") == FALSE) {
        data$worms_match[i] <- "no_match"
        data$worms_status[i] <- "no_match"
      }
    })
    # close try()
    
    # remove all objects created during the loop-interation to start from blank for next iteration
    suppressWarnings(rm(
      species,
      species2,
      species_record,
      n_species_records,
      url,
      url1,
      url2,
      aphia
    ))
    
    
  } # close main loop
  
  
  
  
  ###################################################################################################
  # FLAGGING #
  ###################################################################################################
  
  # flagging
  data <- data %>%
    mutate(
      flag = case_when(
        grepl("multiple", worms_match) ~ 1,    # flag if there were multiple matches
        input_taxa != data$valid_taxa ~ 1,     # flag if taxa changed
        TRUE ~ as.numeric(as.character(flag))  # keeps 0 values if conditions do not apply
      )
    )%>%
    mutate(
      flag = case_when(
        worms_match == "no_match" ~ 2,        # flag if taxa couldnt be matched
        match_aphiaID != valid_aphiaID ~ 2,   # flag if aphia ID changed
        is.na(worms_match) ~2,                # flag if there is no match type
        match_rank != valid_rank ~ 2,         # flag if rank changed  
        worms_status != "accepted" ~ 2,       # flag if taxa is not accepted
        TRUE ~ as.numeric(as.character(flag)) # keeps previous values if conditions do not apply
      )
    )
  
  
  ###################################################################################################
  # FORMATTNG OUtput #
  ###################################################################################################
  
  # rename columns
  data2 <- data %>% rename(
    match_worms_status = "worms_status",
    match_aphia_id = "match_aphiaID",
    valid_aphia_id = "valid_aphiaID",
  )
  
  # sorting columns
  data2 <- data2 %>% select(
    flag,
    input_taxa,
    eval_taxa,
    match_taxa,
    worms_match,
    match_worms_status,
    match_aphia_id,
    match_rank,
    valid_taxa,
    valid_aphia_id,
    valid_rank,
    valid_phylum,
    valid_class,
    valid_order,
    valid_family,
    valid_genus,
    valid_species
  )
  
  
  
  ###################################################################################################
  # FINAL MESSAGE
  ###################################################################################################
  
  message("")  
  message("DONE")
  print(Sys.time() - old)
  message("Please doublecheck results")
  
  return(data2)
  
} # close function
