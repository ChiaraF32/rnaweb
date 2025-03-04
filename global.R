# Load libraries and data shared across the app
library(shiny)
library(ggplot2)
library(OUTRIDER)
library(ggrepel)
library(DT)
library(annotables)
library(DESeq2)
library(tidyverse)
library(data.table)
library(dplyr)
library(biomaRt)
library(org.Hs.eg.db)
library(VariantAnnotation)
library(VariantFiltering)

ods <- readRDS("data/muscle/ods.Rds")
ods_hgnc <- readRDS("data/muscle/ods-hgnc.Rds") 
sam_data <- read.table("data/muscle/samplesheet.tsv", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
fds <- readRDS("data/muscle/fds.Rds")
vds<-readRDS("./data/muscle/MUSCLE_v38_data_table.Rds")

# Define some shared global variables
appTitle <- "Multi-Page Shiny App"
