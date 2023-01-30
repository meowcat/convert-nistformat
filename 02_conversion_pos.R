library(devtools)
library(tidyverse)
library(Spectra)
library(rcdk)
library(rinchi)
library(furrr)
library(SpectraMapping)
library(progress)
library(glue)
plan(multisession)
library(DBI)
library(RSQLite)

options(SpectraMapping = list(verbose=2))

setwd("/data")
source("functions.R")
block_size <- 400
target_folder <- "/work/NI" # NIST

parallel <- FALSE
if(parallel) {
  do_walk <- function(...) future_walk(..., .progress = TRUE)
  do_map <-  function(...) future_map(..., .progress = TRUE)
} else {
  do_walk <- function(...) walk(..., .progress = TRUE)
  do_map <- function(...) map(..., .progress = TRUE)
  options(error=recover)
}

load("chunk_data_pos_small.RData")
# 
chunks_do <- 102
#45 block 139, many problems, full of shitty glycans
# #chunks_do <- 46:403
# chunks_do <- 65
chunk_data <- chunk_data[chunks_do,]

#prefix <- "NI"
#mapping <- system.file("mapping/nist-msp.yaml", package = "SpectraMapping")
mapping <- "mapping/nist-msp.yaml"
mapping_out <- "mapping/massbank.yaml"

fs::dir_create(target_folder, recurse = TRUE)


walk2(
  chunk_data$start_id_per_chunk,
  chunk_data$files_chunks,
  function(start_id, file_in) {
    message(glue("processing file {file_in}, id starting at {start_id}"))
    message("Reading")
    converting_read <- Spectra(
      file_in,
      source = MsBackendMapping(format = MsFormatMsp(parallel=FALSE, progress = TRUE))
    )
    message(glue("Mapping ({basename(mapping)})"))
    converting_map <- converting_read %>%
      SpectraMapping:::mapVariables(mapping)
    
    message("Assigning IDs and blocks")
    spectraData(converting_map)$processing_block <- seq_along(converting_map) %/% block_size
    # spectraData(converting_map)$accession <- sprintf(
    #   glue("{prefix}%06d"),
    #   seq_along(converting_map) + start_id)
    converting_blocks <- split(converting_map, spectraData(converting_map)$processing_block)
    message("\ncompleting the missing information")
    #browser()
    converting_blocks_completed <- 
      do_map(converting_blocks, ~ fill_missing_data(.x))
    message("\nexporting in MassBank format")
    target_filename <- glue("[target_folder]/{accession}.txt", .open = "[", .close = "]" )
    do_walk(converting_blocks_completed, function(block) {
      if(length(block) == 0) {
        log_warn("Block completely empty; not exported")
        return()
      }
      export(block,
             MsBackendMapping(format = MsFormatMassbank(
               parallel = FALSE,
               progress = TRUE,
               mapping = mapping_out
             )),
             file = target_filename,
             progress = FALSE)
    }#, .progress = TRUE
    )
    message(glue("\nChunk {file_in} completed")) 
  }
)
#})

  
