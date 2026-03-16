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
pkgs<-c("tidyverse","jsonlite")
missing<-pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly=TRUE)]
if (length(missing)>0) {
  stop("Missing packages: ", paste(missing, collapse=", "),
       "\nRun renv::restore()")
}
invisible(lapply(pkgs, library, character.only=TRUE))
rm(pkgs, missing)


# Extraction --------------------------------------------------------------
raw <- fromJSON("Data/texas_parks_raw.json")

visits_wide<-as.data.frame(
  do.call(rbind, raw$rows$columns),
  stringsAsFactors=FALSE
)

names(visits_wide)<-c(
  "park",
  "region",
  as.character(2007:2025)
)

visits_wide$park<-gsub("<br>.*","",visits_wide$park)
visits_wide$park<-trimws(visits_wide$park)

visits_wide[,3:ncol(visits_wide)]<-lapply(
  visits_wide[,3:ncol(visits_wide)],
  as.numeric
)

visits_wide<-visits_wide[, !is.na(names(visits_wide))]

visits_panel<-visits_wide |> # Wide to long
  pivot_longer(
    cols=3:ncol(visits_wide),
    names_to="year",
    values_to="visits"
  ) |>
  mutate(year=as.integer(year)) |>
  arrange(park,year)

visits_panel<-visits_panel |>
  mutate(State = "TX",
         b = paste0("1/1/",year),
         e = paste0("12/31/",year)) |>
  dplyr::select(park, visits, State, b, e)

write.csv(visits_panel,"Out/TX.csv")
  