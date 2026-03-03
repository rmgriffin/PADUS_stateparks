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
pkgs<-c("tidyverse","sf")
missing<-pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly=TRUE)]
if (length(missing)>0) {
  stop("Missing packages: ", paste(missing, collapse=", "),
       "\nRun renv::restore()")
}
invisible(lapply(pkgs, library, character.only=TRUE))
rm(pkgs, missing)


# Download and load data --------------------------------------------------
# Download national PADUS database here https://www.sciencebase.gov/catalog/item/652d4fc5d34e44db0e2ee45e and unzip and put in Data folder

# st_layers("Data/PADUS4_1Geodatabase.gdb/") # Inspect layers
padus<-st_read( # Load combined layer
  "Data/PADUS4_1Geodatabase.gdb/",
  layer="PADUS4_1Combined_Proclamation_Marine_Fee_Designation_Easement",
  quiet=TRUE
)


# Subsetting PADUS to state jurisdictional areas --------------------------
padus_s<-padus %>% 
  filter(Des_Tp %in% c("SP","SW","SCA","SREC","SHCA","SRMA","SOTH"))

padus_s %>% 
  filter(State_Nm == "AL" & Des_Tp == "SP") %>% 
  select(Unit_Nm) %>% 
  distinct() %>% 
  print(n = Inf)
