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

process_spectra[["remove_deuterated_spectra"]] <- list(
  fun = .ps_remove_deuterated_spectra,
  params = list(),
  info = ""
)

.ps_add_date <- function(block, params) {
  spec_date <- format(Sys.Date(), "%Y.%m.%d")
  if("date" %in% names(params))
    spec_date <- params$date
  spectraData(block)$date <- spec_date
  block
}

process_spectra[["add_date"]] <- list(
  fun = .ps_add_date,
  params = list(date = "2023.10.19"),
  info = "Optional Date parameter (default: today)"
)

# Add an arbitrary number of features to a Spectra object from a CSV or TSV file,
# where the first column is used as mapping identifier.
.ps_add_data <- function(block, params) {
  stopifnot("source" %in% names(params))
  extra_data <- NULL
  if(fs::path_ext(params$source) == "csv")
    extra_data <- readr::read_csv(params$source, col_types = cols(col_character()))
  if(fs::path_ext(params$source) == "tsv")
    extra_data <- readr::read_tsv(params$source, col_types = cols(col_character()))
  if(is.null(extra_data))
    stop(glue::glue("add_data: {params$source} - data format not supported"))
  id_col <- colnames(extra_data)[[1]]
  
  stopifnot(id_col %in% spectraVariables(block))
  # Basically dplyr join would do the job, 
  # but we need to work with all Spectra objects and play by the rules
  entry_match <- match(as.character(spectraData(block)[[id_col]]), extra_data[[id_col]])
  
  str(entry_match)
  if(any(is.na(entry_match))) {
    log_warn(glue("add_data: Removed {sum(is.na(entry_match))} spectra with missing data"))
    block <- block[!is.na(entry_match)]
    entry_match <- entry_match[!is.na(entry_match)]
  }
  
  data_cols <- colnames(extra_data)[-1]
  for(data_col in data_cols) {
    data_fill_ <- extra_data %>% pull(data_col)
    spectraData(block)[[data_col]] <- data_fill_[entry_match]
    message(glue("filled {data_col}"))
    
  }
  block
}

process_spectra[["add_data"]] <- list(
  fun = .ps_add_data,
  params = list(source = "data.csv"),
  info = "source: a csv or tsv with the identifier in the first column"
)

.ps_remove_missing_smiles <- function(block, params) {
  smiles_empty <- ((spectraData(block)$smiles == "") | (is.na(spectraData(block)$smiles)))
  if(sum(smiles_empty) > 0) {
    log_warn("{sum(smiles_empty)} SMILES empty; spectra removed")
    #spectraData(block)$smiles[smiles_empty] <- spectraData(block)$smiles_db[smiles_empty]
    block <- block[!smiles_empty]
  }
  block
}

process_spectra[["remove_missing_smiles"]] <- list(
  fun = .ps_remove_missing_smiles,
  params = NULL,
  info = ""
)

.ps_compute_from_smiles <- function(block, params) {
  
  stopifnot("properties" %in% names(params))
  calc_props <- params$properties
  available_props <- c("exactmass", "formula", "inchikey", "inchi", "verify_inchikey")
  stopifnot(length(setdiff(calc_props, available_props))==0)
  
  
  # Generate molecule (needed for charge, inchi)
  smiles <-spectraData(block)$smiles
  if(is.list(smiles))
    smiles <- unlist(smiles)
  spectraData(block)$molecule <- 
    parse.smiles(smiles)
  parse_error <- map_lgl(spectraData(block)$molecule, is.null)
  block <- block[!parse_error]
  if(sum(parse_error) > 0)
    log_warn("Removed {sum(parse_error)} spectra with illegal SMILES")
  
  
  # get charge (needed for correct MF)
  spectraData(block)$molecule_charge <- 
    spectraData(block)$molecule %>% 
    map_int(get.total.charge)
  
  # get corrected MassBank-format MF, which is [C1H2]+ if pos charged etc.
  if("formula" %in% calc_props)
    spectraData(block)$formula <-
      map_chr(
        spectraData(block)$molecule,
        \(mol) rcdk::get.mol2formula(mol)@string
      )
  

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

process_spectra[["compute_from_smiles"]] <- list(
  fun = .ps_compute_from_smiles,
  params = list(properties= c("exactmass", "formula", "inchikey", "inchi", "verify_inchikey")),
  info = ""
)

.ps_compute_splash <- function(block, params) {
  spectraData(block)$splash <- map(
    peaksData(block) %>% as.list(),
    RMassBank:::getSplash
  )
    
  block
}

process_spectra[["compute_splash"]] <- list(
  fun = .ps_compute_splash,
  params = list(),
  info =""
)

.ps_remove_special_characters <- function(block, params) {
  # fix silly signs in name
  spectraData(block)$synonyms <- 
    spectraData(block)$synonyms %>% map(~ str_remove_all(.x, fixed("~")))
  spectraData(block)$synonyms <- 
    spectraData(block)$synonyms %>% map(~ str_replace_all(.x, fixed("="), fixed("-")))
  block
}

process_spectra[["remove_special_characters"]] <- list(
  fun = .ps_remove_special_characters,
  params = list(),
  info = ""
)

.ps_add_inchikey2d <- function(block, params) {
  
  spectraData(block)$inchikey2d <- str_sub(
    spectraData(block)$inchikey, 1, 14)
  
  block
}
process_spectra[["add_inchikey2d"]] <- list(
  fun = .ps_add_inchikey2d,
  params = list(),
  info = ""
)

.ps_add_variable <- function(block, params) {
  if("glue" %in% names(params))
    spectraData(block)[[params$variable]] <- 
      glue_data(spectraData(block), params$glue)
  else
    spectraData(block)[[params$variable]] <- params$value
  block
}

process_spectra[["add_variable"]] <- list(
  fun = .ps_add_variable,
  params = list(variable = "target", glue ="{superglue}", value = "value"),
  info = "Use either a glue string (from existing data) or a constant value"
)


process_spectra_batch <- function(block, tasks) {
  
  block <- purrr::reduce(
    tasks,
    function(block, entry) {
      process_spectra[[entry$task]]$fun(block, entry$params)
    }, 
    .init = block
  )

  gc()
  return(block)
}



# Fix arbitrary data in a Spectra object using a CSV input.
# Column 1 represents the matching 
.ps_fix_data <- function(block, params) {
  stopifnot("source" %in% names(params))
  extra_data <- NULL
  if(fs::path_ext(params$source) == "csv")
    extra_data <- readr::read_csv(params$source, col_types = cols(col_character()))
  if(fs::path_ext(params$source) == "tsv")
    extra_data <- readr::read_tsv(params$source, col_types = cols(col_character()))
  if(is.null(extra_data))
    stop(glue::glue("add_data: {params$source} - data format not supported"))
  id_col <- colnames(extra_data)[[1]]
  
  stopifnot(id_col %in% spectraVariables(block))
  # Basically dplyr join would do the job, 
  # but we need to work with all Spectra objects and play by the rules
  entry_match <- match(as.character(spectraData(block)[[id_col]]), extra_data[[id_col]])
    
  data_cols <- colnames(extra_data)[-1]
  for(data_col in data_cols) {
    # keep only rows where this replacement is defined
    extra_data_ <- extra_data |>
      dplyr::filter(!is.na(.data[[data_col]])) |>
      dplyr::filter(.data[[data_col]] != "")
    
    data_fill_ <- extra_data_ %>% pull(data_col)
    id_fill_ <- extra_data_ %>% pull(id_col)
    # find entries in Spectra that match the IDs in the cropped replacement table
    entry_match <- match(as.character(spectraData(block)[[id_col]]), id_fill_)
    entry_match_positions <- which(!is.na(entry_match))
    entry_match_found <- entry_match[!is.na(entry_match)]
    spectraData(block)[entry_match_positions, data_col] <- data_fill_[entry_match_found]
    message(glue("filled {data_col}"))
    
  }
  block
}

process_spectra[["fix_data"]] <- list(
  fun = .ps_fix_data,
  params = list(source = "data.csv"),
  info = "source: a csv or tsv with the identifier in the first column"
)