library(logger)
library(MetaboCoreUtils)

# molfiles_smi <- "hr_msms_nist_all.smi"
# smiles_table <- read_lines(molfiles_smi) %>%
#   str_match("(.*)\\t(.*)\\?\\?(.*)$") %>%
#   magrittr::set_colnames(c("input", "smiles", "filler", "nistid")) %>%
#   as_tibble()
  
  

# smiles_blocks <- split(smiles_table$V2, seq_len(nrow(smiles_table)) %% 500)
# for(i in seq_along(smiles_blocks)) {
#   mol <- parse.smiles(smiles_blocks[[i]])
#   failures <- sum(map_lgl(mol, is.null))
#   log_info("checking block {i}: {failures} problems")
#   gc()
# }

# smiles_table_mapped <- smiles_table %>%
#   mutate(nistid = as.integer(nistid))


rewrite_charged_formula <- function(formula, charge) {
  if(charge == 0)
    return(formula)
  if(charge > 0)
    charge_sign <- paste(rep("+", charge), collapse = "")
  else
    charge_sign <- paste(rep("-", -charge), collapse = "")
  return(glue("[{formula}]{charge_sign}"))
}

get_smiles <- function(nistid_) {
  return(smiles_table_mapped[match(nistid_, smiles_table_mapped$nistid),]$smiles)
}



# pubchem_ref_smiles <- r"(C:\Daten\Unicorn\Data\pubchem_ref\pubchem_ref.db)"

# load_all("C:/Daten/git/SpectraMapping/")
# 

# # smiles_struc <- dbGetQuery(smiles_db, "SELECT * from inchi_to_smiles LIMIT 1")
# get_smiles_db <- function(inchikey, db) {
#   if(is.null(inchikey))
#     return(NA)
#   inchikey1 <- str_sub(inchikey, 1, 14)
#   smiles <- dbGetQuery(
#     db,
#     "SELECT smiles from inchi_to_smiles WHERE inchikey1 = ?",
#     inchikey1)
#   if(nrow(smiles) == 1)
#     return(smiles$smiles)
#   return(NA)
# }

inchikey_equiv <- function(key1, key2) {
  key1_str <- str_sub(key1, 1, 14)
  key2_str <- str_sub(key2, 1, 14)
  key1_stereo <- str_sub(key1, 16, 25)
  key2_stereo <- str_sub(key2, 16, 25)
  stereo_flat <- "UHFFFAOYSA"
  return((key1_str == key2_str))
}


process_spectra <- list()

.ps_remove_deuterated_spectra <- function(block, params) {
  deuterated <- str_detect(spectraData(block)$formula, fixed("D"))
  block <- block[!deuterated]
  if(sum(deuterated) > 0)
    log_warn("Removed {sum(deuterated)} deuterated spectra")
  block
}

process_spectra[["remove_deuterated_spectra"]] <- .ps_remove_deuterated_spectra

.ps_add_date <- function(block, params) {
  spec_date <- format(Sys.Date(), "%Y.%m.%d")
  if("date" %in% names(params))
    spec_date <- params$date
  block$date <- spec_date
  block
}

process_spectra[["add_date"]] <- .ps_add_date

# Add an arbitrary number of features to a Spectra object from a CSV or TSV file,
# where the first column is used as mapping identifier.
.ps_add_data <- function(block, params) {
  stopifnot("source" %in% names(params))
  extra_data <- NULL
  if(fs::path_ext(params$source) == "csv")
    extra_data <- readr::read_csv(params$source)
  if(fs::path_ext(params$source) == "tsv")
    extra_data <- readr::read_tsv(params$source)
  if(is.null(extra_data))
    stop(glue::glue("add_data: {params$source} - data format not supported"))
  id_col <- colnames(extra_data)[[1]]
  
  stopifnot(id_col %in% spectraVariables(block))
  # Basically dplyr join would do the job, 
  # but we need to work with all Spectra objects and play by the rules
  entry_match <- match(spectraData(block)$id_col, extra_data$id_col)
  
  if(any(is.na(entry_match)))
    stop("add_data: Missing data for some entries")
  
  data_cols <- colnames(extra_data)[-1]
  for(data_col in data_cols) {
    spectraData(block)[[data_col]] <- 
      extra_data %>% pull(data_col) %>% `[`(entry_match)
  }
  block
}

process_spectra[["add_data"]] <- .ps_add_data

.ps_remove_missing_smiles <- function(block, params) {
  smiles_empty <- ((spectraData(block)$smiles == "") | (is.na(spectraData(block)$smiles)))
  if(sum(smiles_empty) > 0) {
    log_warn("{sum(smiles_empty)} SMILES empty; spectra removed")
    #spectraData(block)$smiles[smiles_empty] <- spectraData(block)$smiles_db[smiles_empty]
    block <- block[!smiles_empty]
  }
  block
}

process_spectra[["remove_missing_smiles"]] <- .ps_remove_missing_smiles


.ps_compute_from_smiles <- function(block, params) {
  
  stopifnot("properties" %in% names(params))
  calc_props <- params$properties
  available_props <- c("exactmass", "formula", "inchikey", "inchi", "verify_inchikey")
  stopifnot(length(setdiff(calc_props, available_props))==0)
  
  
  # Generate molecule (needed for charge, inchi)
  spectraData(block)$molecule <- 
    parse.smiles(spectraData(block)$smiles)
  
  parse_error <- map_lgl(spectraData(block)$molecule, is.null)
  block <- block[!parse_error]
  if(sum(parse_error) > 0)
    log_warn("Removed {sum(parse_error)} spectra with illegal SMILES")
  
  
  # get charge (needed for correct MF)
  spectraData(block)$molecule_charge <- 
    spectraData(block)$molecule %>% 
    map_int(get.total.charge)
  
  if("exactmass" %in% calc_props)
    spectraData(block)$exactmass <- map2_dbl(
      spectraData(block)$formula,
      spectraData(block)$molecule_charge,
      ~ MetaboCoreUtils::calculateMass(.x) - 
        .y * RMassBank:::.emass
    )
  
  # get corrected MassBank-format MF, which is [C1H2]+ if pos charged etc.
  if("formula" %in% calc_props)
    spectraData(block)$formula <-
      map2_chr(
        spectraData(block)$formula,
        spectraData(block)$molecule_charge,
        rewrite_charged_formula
      )
  
  #browser()
  # get inchi and the correct inchikey (not -NA but -SA);
  # move the old inchikey to inchikey2
  if("verify_inchikey" %in% calc_props)
    spectraData(block)$inchikey2 <- 
      spectraData(block)$inchikey
  
  if("inchi" %in% calc_props)
    spectraData(block)$inchi <- 
      spectraData(block)$molecule %>% map_chr(get.inchi)
  
  if("inchikey" %in% calc_props)
    spectraData(block)$inchikey <- 
      spectraData(block)$molecule %>% map_chr(get.inchi.key)
  
  # delete the molecule again
  spectraData(block)$molecule <- NULL
  
  if(!("charge" %in% calc_props))
    spectraData(block)$molecule_charge <- NULL
  
  if("verify_inchikey" %in% calc_props) {
    mismatch_inchi <- !inchikey_equiv(
      spectraData(block)$inchikey,
      spectraData(block)$inchikey2)
    block <- block[!mismatch_inchi]
    if(sum(mismatch_inchi) > 0)
      log_warn("Removed {sum(mismatch_inchi)} spectra with inchikey mismatch")
    spectraData(block)$inchikey2 <- NULL
  }
  
  block
}

process_spectra[["compute_from_smiles"]] <- .ps_compute_from_smiles

.ps_compute_splash <- function(block, params) {
  spectraData(block)$splash <- map(
    peaksData(block) %>% as.list(),
    RMassBank:::getSplash
  )
    
  block
}

process_spectra[["compute_splash"]] <- .ps_compute_splash

.ps_remove_special_characters <- function(block, params) {
  # fix silly signs in name
  spectraData(block)$synonyms <- 
    spectraData(block)$synonyms %>% map(~ str_remove_all(.x, fixed("~")))
  spectraData(block)$synonyms <- 
    spectraData(block)$synonyms %>% map(~ str_replace_all(.x, fixed("="), fixed("-")))
  block
}

process_spectra[["remove_special_characters"]] <- .ps_remove_characters

.ps_add_inchikey2d <- function(block, params) {
  
  spectraData(block)$inchikey2d <- str_sub(
    spectraData(block)$inchikey, 1, 14)
  
  block
}

process_spectra[["add_inchikey2d"]] <- .ps_add_inchikey2d

proces_spectra_batch <- function(block, tasks) {
  
  block <- purrr::reduce(
    tasks,
    function(entry) {
      process_spectra[[entry$task]](block, entry$params)
    }, 
    .init = block
  )

  gc()
  return(block)
}
