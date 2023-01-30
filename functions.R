library(logger)
molfiles_smi <- "hr_msms_nist_all.smi"
smiles_table <- read_lines(molfiles_smi) %>%
  str_match("(.*)\\t(.*)\\?\\?(.*)$") %>%
  magrittr::set_colnames(c("input", "smiles", "filler", "nistid")) %>%
  as_tibble()
  
  

# smiles_blocks <- split(smiles_table$V2, seq_len(nrow(smiles_table)) %% 500)
# for(i in seq_along(smiles_blocks)) {
#   mol <- parse.smiles(smiles_blocks[[i]])
#   failures <- sum(map_lgl(mol, is.null))
#   log_info("checking block {i}: {failures} problems")
#   gc()
# }

smiles_table_mapped <- smiles_table %>%
  mutate(nistid = as.integer(nistid))


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



pubchem_ref_smiles <- r"(C:\Daten\Unicorn\Data\pubchem_ref\pubchem_ref.db)"

# load_all("C:/Daten/git/SpectraMapping/")
# 

# smiles_struc <- dbGetQuery(smiles_db, "SELECT * from inchi_to_smiles LIMIT 1")
get_smiles_db <- function(inchikey, db) {
  if(is.null(inchikey))
    return(NA)
  inchikey1 <- str_sub(inchikey, 1, 14)
  smiles <- dbGetQuery(
    db,
    "SELECT smiles from inchi_to_smiles WHERE inchikey1 = ?",
    inchikey1)
  if(nrow(smiles) == 1)
    return(smiles$smiles)
  return(NA)
}

inchikey_equiv <- function(key1, key2) {
  key1_str <- str_sub(key1, 1, 14)
  key2_str <- str_sub(key2, 1, 14)
  key1_stereo <- str_sub(key1, 16, 25)
  key2_stereo <- str_sub(key2, 16, 25)
  stereo_flat <- "UHFFFAOYSA"
  return((key1_str == key2_str))
}



fill_missing_data <- function(block) {
  

  # generate accession number
  
  deuterated <- str_detect(spectraData(block)$formula, fixed("D"))
  block <- block[!deuterated]
  if(sum(deuterated) > 0)
    log_warn("Removed {sum(deuterated)} deuterated spectra")
  
  
  
  spectraData(block)$date <- format(Sys.Date(), "%Y.%m.%d")
  
  spectraData(block)$smiles <- get_smiles(spectraData(block)$nistid)
  # smiles_db <- dbConnect(
  #   SQLite(),
  #   pubchem_ref_smiles
  # )
  # spectraData(block)$smiles_db <- map_chr(
  #   spectraData(block)$inchikey,
  #   get_smiles_db,
  #   db = smiles_db
  # )
  # dbDisconnect(smiles_db)
  
  smiles_empty <- ((spectraData(block)$smiles == "") | (is.na(spectraData(block)$smiles)))
  if(sum(smiles_empty) > 0) {
    log_warn("{sum(smiles_empty)} SMILES empty; spectra removed")
    #spectraData(block)$smiles[smiles_empty] <- spectraData(block)$smiles_db[smiles_empty]
    block <- block[!smiles_empty]
  } 
  # 
  # smiles_na <- (is.na(spectraData(block)$smiles))
  # if(sum(smiles_na) > 0) {
  #   log_warn("{sum(smiles_na)} SMILES N/A after DB lookup, spectra removed")
  #   block <- block[!smiles_na]
  # }
  
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
  # get exact mass (not present in original)
  spectraData(block)$exactmass2 <- map2_dbl(
    spectraData(block)$formula,
    spectraData(block)$molecule_charge,
    ~ get.formula(.x, .y)@mass
  )
  # get corrected MassBank-format MF, which is [C1H2]+ if pos charged etc.
  
  spectraData(block)$formula <-
    map2_chr(
      spectraData(block)$formula,
      spectraData(block)$molecule_charge,
      rewrite_charged_formula
    )
  
  #browser()
  # get inchi and the correct inchikey (not -NA but -SA);
  # move the old inchikey to inchikey2
  spectraData(block)$inchikey2 <- 
    spectraData(block)$inchikey
  
  spectraData(block)$inchi <- 
    spectraData(block)$molecule %>% map_chr(get.inchi)
  spectraData(block)$inchikey <- 
    spectraData(block)$molecule %>% map_chr(get.inchi.key)
  
  
  
  # compute splash
  spectraData(block)$splash <- map(
    peaksData(block) %>% as.list(),
    RMassBank:::getSplash
  )
  
  # delete the molecule again
  spectraData(block)$molecule <- NULL
  
  # fix silly signs in name
  spectraData(block)$synonyms <- 
    spectraData(block)$synonyms %>% map(~ str_remove_all(.x, fixed("~")))
  spectraData(block)$synonyms <- 
    spectraData(block)$synonyms %>% map(~ str_replace_all(.x, fixed("="), fixed("-")))
  
  
  mismatch_inchi <- !inchikey_equiv(
    spectraData(block)$inchikey,
    spectraData(block)$inchikey2)
  block <- block[!mismatch_inchi]
  if(sum(mismatch_inchi) > 0)
    log_warn("Removed {sum(mismatch_inchi)} spectra with inchikey mismatch")
  
  gc()
  
  return(block)
}
