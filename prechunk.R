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

source("functions.R")


parallel <- FALSE

if(!exists("settings_file"))
  settings_file <- "input/settings.yml"
settings <- yaml::read_yaml(settings_file)

mapping <- settings$mapping$input
mapping_out <- settings$mapping$output
filename_out <- settings$filename_out_schema
cache_folder <- settings$data$cache
block_size <- settings$files_per_block
target_folder <- settings$data$output

formats <- list(
  "MsFormatMassbank" = MsFormatMassbank,
  "MsFormatMsp" = MsFormatMsp
)
stopifnot(settings$formats$input %in% names(formats))
stopifnot(settings$formats$output %in% names(formats))
stopifnot(fs::file_exists(settings$mapping$input))
stopifnot(fs::file_exists(settings$mapping$output))

chunks_folder <- fs::path(cache_folder, "chunks")
fs::file_delete(chunks_folder)
fs::dir_create(chunks_folder, recurse = TRUE)
files_in <- fs::dir_ls(input_folder, glob = "*.txt")

iwalk(files_in, function(file_in, i) {
  message(glue("Splitting file {i}..."))
  preSplit(
    file_in,
    format = formats[[settings$format$input]],
    block_size = settings$spectra_per_file,
    out_dir = chunks_folder,
    out_file_schema = glue("file[i]-chunk{i}", .open = "[", .close = "]")
  )
  message(glue("Splitting file {i}: done"))
})

message(glue("Chunking completed"))