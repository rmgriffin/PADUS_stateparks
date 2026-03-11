# Setup -------------------------------------------------------------------
rm(list=ls()) # Clears workspace

# # Install/call libraries
# install.packages("renv") # Run if you have cloned repository and don't already have renv installed
# renv::restore() # Run once after cloning repository
# renv::install("package") # Run to install new packages
# renv::snapshot() # Run after installing new packages (need to be referenced in code to get written - run renv::snapshot(type = "all") to avoid this)
# renv::init() # Only run when the repository is first created, don't run on cloning an existing repository

# Data for this repository is at https://drive.google.com/file/d/1UiExxgfnedtRNp76G2G7-sPGcHTkZguD/view?usp=sharing

# Checks that required packages are installed, stops if not, loads them if they are
pkgs<-c("tidyverse","rvest","pdftools")
missing<-pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly=TRUE)]
if (length(missing)>0) {
  stop("Missing packages: ", paste(missing, collapse=", "),
       "\nRun renv::restore()")
}
invisible(lapply(pkgs, library, character.only=TRUE))
rm(pkgs, missing)

# Download ----------------------------------------------------------------
url<-"https://extension.unr.edu/neap/state-parks-visitor-survey.aspx"

download_dir<-"Data/nevada_state_park_pdfs"
dir.create(download_dir, showWarnings=FALSE)
page<-read_html(url)

pdf_links<-page |>
  html_elements("a") |>
  html_attr("href") |>
  unique()

pdf_links<-pdf_links[stringr::str_detect(pdf_links, "\\.pdf$")]
pdf_links<-pdf_links[!stringr::str_detect(pdf_links, "6707|6550")] # Removing pdfs that aren't data targets

download_pdf<-function(link){
  
  fname<-basename(link)
  dest<-file.path(download_dir, fname)
  
  if(!file.exists(dest)){
    try(download.file(link, dest, mode="wb", quiet=TRUE))
  }
  
  dest
}

pdf_files<-map_chr(pdf_links, download_pdf)


# Extract visitation data -------------------------------------------------
parse_pdf<-function(file){
  
  txt<-pdftools::pdf_text(file)
  
  # --- extract park name from first page ---
  lines<-stringr::str_split(txt[1], "\n")[[1]]
  lines<-stringr::str_trim(lines)
  lines<-lines[lines!=""]
  
  park<-lines[1]
  
  # combine text for regex extraction
  txt_all<-paste(txt, collapse=" ")
  
  # locate numbers after Visitors
  visitor_section<-stringr::str_match(
    txt_all,
    "Visitors\\s+([0-9,]+)\\s+([0-9,]+)\\s+([0-9,]+)\\s+([0-9,]+)\\s+([0-9,]+)"
  )
  
  if(all(is.na(visitor_section))){
    return(NULL)
  }
  
  visitors<-visitor_section[2:6] |>
    stringr::str_remove_all(",") |>
    as.numeric()
  
  tibble::tibble(
    park=park,
    year=2019:2023,
    visitors=visitors
  )
}

visitation_df<-map_dfr(pdf_files, parse_pdf)

if (dir.exists("Out")) unlink("Out", recursive = TRUE, force = TRUE) # Deletes output folder
dir.create("Out")
write.csv(visitation_df,"Out/NV.csv")
