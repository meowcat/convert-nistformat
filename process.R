suppressPackageStartupMessages(library(devtools))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(Spectra))
suppressPackageStartupMessages(library(rcdk))
suppressPackageStartupMessages(library(rinchi))
suppressPackageStartupMessages(library(furrr))
suppressPackageStartupMessages(library(SpectraMapping))
suppressPackageStartupMessages(library(progress))
suppressPackageStartupMessages(library(glue))
suppressPackageStartupMessages(library(DBI))
suppressPackageStartupMessages(library(RSQLite))

plan(multisession)

options(SpectraMapping = list(verbose=2))

source("functions.R")


parallel <- FALSE

message(getwd())

if(!exists("settings_file"))
  settings_file <- "input/settings.yml"
settings <- yaml::read_yaml(settings_file)
str(settings)

mapping <- settings$mapping$input
mapping_out <- settings$mapping$output
filename_out <- settings$filename_out_schema
cache_folder <- settings$data$cache
block_size <- settings$files_per_block
target_folder <- settings$data$output

formats <- list(
  "MsFormatMassbank" = MsFormatMassbank,
  "MsFormatMsp" = MsFormatMsp,
  "MsFormatMgf" = MsFormatMgf
)
stopifnot(settings$formats$input %in% names(formats))
stopifnot(settings$formats$output %in% names(formats))
stopifnot(fs::file_exists(settings$mapping$input))
stopifnot(fs::file_exists(settings$mapping$output))

if(parallel) {
  do_walk <- function(...) future_walk(..., .progress = TRUE)
  do_map <-  function(...) future_map(..., .progress = TRUE)
} else {
  do_walk <- function(...) walk(..., .progress = TRUE)
  do_map <- function(...) map(..., .progress = TRUE)
  options(error=recover)
}


input_folder <- settings$data$input
if(settings$spectra_per_file > 0)
  input_folder <- fs::path(cache_folder, "chunks")

stopifnot(fs::dir_exists(input_folder))

fs::dir_create(target_folder, recurse = TRUE)

files_in <- fs::dir_ls(input_folder)
blocks <- files_in %>% split(seq_along(files_in) %/% block_size)
block_count <- length(blocks)

iwalk(
  blocks,
  function(block, i) {
    message(glue("processing block {i} of {block_count}"))
    message("Reading")
    converting_read <- Spectra(
      block,
      source = MsBackendMapping(format = formats[[settings$format$input]](
        parallel=FALSE, progress = FALSE))
    )
    
    message(glue("Mapping ({basename(mapping)})"))

    converting_map <- converting_read %>%
      SpectraMapping:::mapVariables(mapping)
    
    # message("Available spectra variables:")
    # str(spectraVariables(converting_map))
    
    message("Applying extra processing steps")
    converting_process <- process_spectra_batch(converting_map, settings$processing)

    message("\nexporting")
    target_filename <- glue("[target_folder]/[filename_out]", .open = "[", .close = "]" )
    export(converting_process,
           MsBackendMapping(format = formats[[settings$format$output]](
             parallel = FALSE,
             progress = TRUE,
             mapping = mapping_out
           )),
           file = target_filename,
           progress = FALSE,
           append = TRUE)
    

    message(glue("Block {i} completed")) 
})


# merge the files

  
