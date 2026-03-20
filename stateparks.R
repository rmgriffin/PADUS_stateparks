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
pkgs<-c("tidyverse","sf","googlesheets4","fuzzyjoin")
missing<-pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly=TRUE)]
if (length(missing)>0) {
  stop("Missing packages: ", paste(missing, collapse=", "),
       "\nRun renv::restore()")
}
invisible(lapply(pkgs, library, character.only=TRUE))
rm(pkgs, missing)


# Download and load data --------------------------------------------------
gs4_deauth()
ann_visits<-read_sheet(
  "https://docs.google.com/spreadsheets/d/1Sjxj21f7JpXsawPMPXvXV51qR1KLzj8StdHDqBOoi-E",
  sheet="Annual")
mon_visits<-read_sheet(
  "https://docs.google.com/spreadsheets/d/1Sjxj21f7JpXsawPMPXvXV51qR1KLzj8StdHDqBOoi-E",
  sheet="Monthly")

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

padus_s %>% # Parks are often broken up into multiple pieces if they have distinct information associated with them
  filter(State_Nm == "AL" & Des_Tp == "SP") %>% 
  select(Unit_Nm) %>% 
  distinct() %>% 
  print(n = Inf)


# Automated matching site names -----------------------------------------------------
sites1<-ann_visits|>
  distinct(State, Park) %>% 
  rename(State_Nm = State, Unit_Nm = Park)

sites2<-padus_s|>
  as.data.frame()|>
  distinct(State_Nm, Unit_Nm)

candidates<-stringdist_inner_join( # Fuzzy matching on names
  sites1, sites2,
  by=c("State_Nm","Unit_Nm"),
  method="jw",
  max_dist=0.25,   # loose on purpose
  distance_col="dist"
)

match_strength<-candidates|> # Matching on characters
  group_by(State_Nm.x, Unit_Nm.x)|>
  arrange(Unit_Nm.dist, .by_group=TRUE)|>
  summarise(
    best_match=first(Unit_Nm.y),
    second_match=nth(Unit_Nm.y, 2, default=NA_character_),
    third_match=nth(Unit_Nm.y, 3, default=NA_character_),
    
    best_dist=first(Unit_Nm.dist),
    second_dist=nth(Unit_Nm.dist, 2, default=NA_real_),
    third_dist=nth(Unit_Nm.dist, 3, default=NA_real_),
    
    gap=second_dist-best_dist,
    .groups="drop"
  )

confident<-match_strength|> # Confident matches before first mismatch for either 
  filter(
    best_dist<0.066 | gap>0.092)

unresolved<-match_strength|>
  anti_join(confident, by=c("State_Nm.x","Unit_Nm.x"))

unresolved_candidates<-candidates|>
  semi_join(unresolved, by=c("State_Nm.x","Unit_Nm.x"))

token_overlap<-function(a, b){
  a_tokens<-a|>
    str_to_upper()|>
    str_split("\\s+")|>
    unlist()
  
  b_tokens<-b|>
    str_to_upper()|>
    str_split("\\s+")|>
    unlist()
  
  a_tokens<-a_tokens[a_tokens != ""]
  b_tokens<-b_tokens[b_tokens != ""]
  
  inter<-length(intersect(a_tokens, b_tokens))
  union<-length(union(a_tokens, b_tokens))
  
  if(union == 0) return(NA_real_)
  inter / union
}

unresolved_candidates2<-unresolved_candidates|>
  rowwise()|>
  mutate(
    token_score=token_overlap(Unit_Nm.x, Unit_Nm.y)
  )|>
  ungroup()

token_strength<-unresolved_candidates2|> # Matching on tokens
  group_by(State_Nm.x, Unit_Nm.x)|>
  arrange(desc(token_score), Unit_Nm.dist, .by_group=TRUE)|>
  summarise(
    best_token=first(token_score),
    second_token=nth(token_score, 2, default=NA_real_),
    token_gap=best_token-second_token,
    best_match_token=first(Unit_Nm.y),
    best_dist_for_token=first(Unit_Nm.dist),
    .groups="drop"
  )

confident_token<-token_strength|>
  filter(
    best_token >= 0.7 | token_gap >= 0.28)

confident_all_stage12<-bind_rows( # Confident results based on strings and token matching
  confident|>
    transmute(State_Nm.x, Unit_Nm.x, best_match),
  
  confident_token|>
    transmute(State_Nm.x, Unit_Nm.x, best_match=best_match_token)
)|>
  distinct()

unresolved_stage12<-match_strength|> # Unresolved
  anti_join(confident_all_stage12, by=c("State_Nm.x","Unit_Nm.x"))

# unresolved_sites<-unresolved_stage12|> # Cosine approach, barely helped
#   select(State_Nm.x, Unit_Nm.x)|>
#   distinct()|>
#   rename(State_Nm=State_Nm.x, Unit_Nm=Unit_Nm.x)
# 
# states_to_match<-intersect(unique(unresolved_sites$State_Nm), unique(sites2$State_Nm))
# 
# candidates_cos<-map_dfr(states_to_match, function(st){
#   
#   x<-unresolved_sites|>
#     filter(State_Nm == st)
#   
#   y<-sites2|>
#     filter(State_Nm == st)
#   
#   if(nrow(x) == 0 || nrow(y) == 0) return(NULL)
#   
#   stringdist_inner_join(
#     x, y,
#     by="Unit_Nm",
#     method="cosine",
#     q=2,
#     max_dist=0.4,
#     distance_col="cos_dist"
#   )
# })
# 
# match_strength_cos<-candidates_cos|>
#   group_by(State_Nm.x, Unit_Nm.x)|>
#   arrange(cos_dist, .by_group=TRUE)|>
#   summarise(
#     best_match=first(Unit_Nm.y),
#     second_match=nth(Unit_Nm.y, 2, default=NA_character_),
#     third_match=nth(Unit_Nm.y, 3, default=NA_character_),
#     best_dist=first(cos_dist),
#     second_dist=nth(cos_dist, 2, default=NA_real_),
#     third_dist=nth(cos_dist, 3, default=NA_real_),
#     gap=second_dist-best_dist,
#     .groups="drop"
#   )
# 
# confident_cos<-match_strength_cos|>
#   filter(best_dist<0.25 | gap>0.05)
# 
# confident_all<-bind_rows(
#   confident_all_stage12,
#   confident_cos|>
#     transmute(State_Nm.x, Unit_Nm.x, best_match)
# )|>
#   distinct()
# 
# unresolved_final<-match_strength|>
#   anti_join(confident_all, by=c("State_Nm.x","Unit_Nm.x"))


# Manual matching site names ----------------------------------------------
source("all_resolved_final.R")
# # The code below is used to assist in generating hardcoded table in "all_resolved_final.R"
# review_tbl<-unresolved_stage12|>
#   left_join(unresolved_candidates2, by=c("State_Nm.x","Unit_Nm.x"))|>
#   group_by(State_Nm.x, Unit_Nm.x)|>
#   arrange(Unit_Nm.dist, .by_group=TRUE)|>
#   slice_head(n=3)|>   # show top 3 candidates
#   summarise(
#     cand1=first(Unit_Nm.y),
#     cand2=nth(Unit_Nm.y, 2),
#     cand3=nth(Unit_Nm.y, 3),
#     dist1=first(Unit_Nm.dist),
#     dist2=nth(Unit_Nm.dist, 2),
#     dist3=nth(Unit_Nm.dist, 3),
#     .groups="drop"
#   )|>
#   mutate(final_match=NA_character_)
# 
# choices<-c(
#   1,1,1,1,1,1,1,1,1,1,
#   1,1,1,1,1,1,1,1,1,1,
#   1,1,1,1,1,1,1,1,1,1,
#   1,1,1,1,1,1,1,1,1,1,
#   1,1,1,1,1,1,1,1,1,1,
#   1,1,1,1,1,1,1,1,1,1,
#   1,1,1,1,1,1,1,1,1,1,
#   1,1,1,1,1,1,1,1,1,1,
#   1,1,1,1,1,2,1,1,1,1,
#   1,1,1,1,1,1,1,1,1,1,
#   1,1,1,1,1,1,1,1,1,1,
#   1,1,1,1,1,1,1,1,1,1,
#   1,1,1,1,1,1,1,1,1,1,
#   2,1,1,1,1,1,1,1,1,1,
#   1,1,1,1,1,1,1,1,1,1,
#   1,1,1,1,1,1,1,1,1,1,
#   1,1,1,1,1,1,1,1,1,1,
#   1,1,1,1,1,3,1,3,1,1,
#   1,1,1,1,1,1,1,1,1,1,
#   1,1,1,1,1,1,1,1,1,1,
#   1,1,1,1,1,1,1,1,2,1,
#   1,1,1,1,1,1,1,1,1,1,
#   1,1,1,1,1,1,1,3,1,1, 
#   1,1,1,1,1,1,1,1,1,1,
#   1,1,1,1,1,1,1,1,1,2,
#   1,1,1,1,1,1,1,1,1,1,
#   1,1,1,1,1,1,1,1,1,1,
#   1,1,1,1,1,1,1,1,1,1,
#   1,1,1,3,1,1,1,1,1,1, 
#   1,1,1,1,1,3,1,1,1,1, 
#   1,1,1,1,1,1,1,1,2,1,
#   1,1,1,1,1,1,1,1,1,1,
#   1,1,1,3,1,1,1,1,1,1,  
#   1,1,1,1,1,1,1,1,1,1,
#   1,1,1,1,1,2,1,1,1,1,
#   1,1,1,1,1,1,1,1,1,1,
#   1,1,1,1,1,1,1,1,1,1,
#   1,1,1,1,1,1,1,1,1,1,
#   1,1,1,1,1,1,1,1,1,1,
#   1,1,1,1,1,1,1,1,1,1,
#   1,1,1,1,1,1,1,1,1,1,
#   1,1,1,1,1,1,1,1,1,1,
#   1,1,1,1,1,1,1,1,1,1,
#   1,1,1,1,1,1,1,1,1,1,
#   1,1,1,1,1,1,1,1,1,1,
#   1,1,1,1,1,1,1,1,1,1,
#   1,1,1,1,1,1,1,1,1,1,
#   1,1,1,1,1,1,1,1,1,1,
#   3,1,3,3,1,1,1,1,1,1, 
#   1,1,1,1,1,1,1,1,1,1,
#   1,1,1,1,2,1,1,1,1,1,
#   1,1,1,1,1,1,1,1,1,1,
#   2,3,1,1,1,3,1,1,1,1, 
#   3,1,1,2,1,3,1,1,1,1, 
#   1,3,3,1,1,1,1,3,1,1, 
#   1,2,1,1,1,3,1,1,1,1,
#   1,1,1,1,1,1,1,1,3,1, 
#   1,1,1,1,3,3,3,3,1,1, 
#   1,1,1,1,2,1,1,1,1,1,
#   1,1,3,1,1,1,1,1,3,3, 
#   1,1,2,1,1,3,1,2,1,1, 
#   1,1,1,1,1,3,1,1,1,1, 
#   1,1,3,1,1,1,1,1,2,1, 
#   3,1,1,1,1,3,1,1,1,1, 
#   1,1,1,2,2,1,1,1,1,1,
#   2,1,1,1,1,1,1,1,1,1,
#   1,1,1,1,1,1,1,1,1,1,
#   1,1,1,1,1,1,1,1,2,1,
#   1,1,1,1,1,1,1,1,1,1,
#   1,1,1,3,3,1,1,1,1,1, 
#   1,1,1,1,1,1,1,1,1,1,
#   1,3,2,1,1,1,1,1,1,1, 
#   1,1,1,1,1,1,3,3,3,1, 
#   3,3,3,3,3,3,3,3,3,3, 
#   1,3,1,3,1,1,3,3,3,3, 
#   1,1,1,3,3,3,1,1,1,1,
#   1,3,1,3,3,3,3,3,3,3, 
#   1,1,1,3,3,1,3,3,1,1, 
#   1,1,3,3,3,3,2,1,3,3, 
#   1,3,3,1,3,3,3,3,3,3, 
#   3,3,3,3,3,3,3,3,3,1, 
#   1,1,3,1,1,1,1,3,3,3, 
#   1,1,3,1,1,3,3,3,3,1, 
#   1,1,3,1,3,1,1,3,1,1, 
#   3,1,1,3,3,1,1,1,2,1, 
#   1,1,1,3,1,1,1,3,3,1, 
#   1,2,3,1,1,1,1,1,1,1, 
#   1,1,1,2,1,1,1,1,1,1,
#   1,1,1,1,1,1,1,1,1,1,
#   1,1,1,1,3,1,1,1,1,1, 
#   1,1,1,1,1,1,1,1,1,1,
#   1,1,1,1,1,1,1,1,1,1,
#   1,1,1,1,1,1,1,1,2,1,
#   2,1,1,1,1,3,1,1,1,1, 
#   1,1,3,3,2,1,3,3,1,1, 
#   1,1,3,1,3,1,1,1,1,1, 
#   3,1,1,1,1,1,3,3,3,3, 
#   1,1,1,3,3,1,3,3,3,1, 
#   1,1,1,3,3,3,1,1,1,1, 
#   1,3,1,1,3,3,3,1,1,1,
#   1,1,1,3,1,1,1,1,1,3, 
#   3,1,1,1,1,1,1,1,1,1, 
#   1,1,1,1,1,1,1,1,1,1,
#   1,1,1,1,1,1,1,1,1,1,
#   1,1,1,1,1,1,1,1,1,1,
#   1,1,1,1,1,1,1,2,1,3, 
#   1,1,1,1,1,1,1,1,1,1,
#   1,1,1,1,1,1,1,1,1,1,
#   3,1,1,3,1,1,2,1,1,1, 
#   1,1,1,1,1,1,1,1,1,3, 
#   1,1,1,1,1,1,1,2,1,3,  
#   3,1,1,1,1,1,1,2,1,1,    
#   1,1,1,1,1,2,1,1,1,1,
#   2,1,3,1,1,3,3,3,1,1, 
#   1,3,3,1,1,2,1,1,1,1,
#   1,2,1,1,2,1,1,1,1,1,
#   1,3,1,1,1,3,3,3,1,3, 
#   1,3,1,3,3,1,1,1,1,1,   
#   3,1,1,3,3,1,3,3,1,1, 
#   1,1,3,1,1,1,1,2,1,1, 
#   3,1,1,1,1,1,1,1,1,1, 
#   1,1,1,1,1,1,1,1,1,1,
#   1,1,1,1,1,1,1,1,1,2,
#   1,1,1,1,2,1,1,3,3,2, 
#   1,2,1,2,1,1,1,1,2,1,
#   2,1,1,1,1,2,2,3,1,1, 
#   1,1,1,1,1,1,1,1,1,1,
#   1,1,1,3,1,1,1,1,3,1, 
#   1,1,1,3,3,3,3,3,3,3, 
#   3,1,1,1,3,3,3,3,3,3, 
#   3,1,1,1,3,2,1,1,1,1, 
#   1,1,1,1,1,1,1,1,1,1,
#   1,1,1,1,1,2,1,1,3,3, 
#   1,1,1,1,1,1,1,1,3,1, 
#   1,1,1,2,1,1,1,1,1,1,
#   1,1,1,3,3,1,1,3,3,1, 
#   3,1,3,3,1,1,1,3,3,1,  
#   1,1,1,1,1,1,1,1,1,1,
#   3,3,1,1,1,1,1,1,3,3, 
#   3,1,3,1,1,1,1,1,1,1, 
#   1,1,1,1,1,1,1,1,1,1,
#   3,3,1,1,1,1,2,1,1,1, 
#   1,1,1,1,1)
# 
# stopifnot(length(choices)==nrow(review_tbl))
# stopifnot(all(is.na(choices) | choices %in% 1:3))
# 
# cand_cols<-review_tbl[c("cand1","cand2","cand3")]
# 
# best_match<-vapply(
#   seq_len(nrow(review_tbl)),
#   function(i){
#     ch<-choices[i]
#     
#     if(is.na(ch)) return(NA_character_)
#     
#     val<-cand_cols[[ch]][i]
#     
#     if(is.na(val)) NA_character_ else as.character(val)
#   },
#   character(1)
# )
# 
# manual_resolved<-review_tbl|>
#   transmute(
#     State_Nm.x,
#     Unit_Nm.x,
#     best_match
#   )
# 
# all_resolved<-rbind(manual_resolved,confident_all_stage12)
# 
# to_str<-function(x){
#   ifelse(is.na(x), "NA", paste0('"', x, '"'))
# }
# 
# rows<-apply(all_resolved, 1, function(r){
#   paste0(
#     "  data.frame(State_Nm.x=", to_str(r["State_Nm.x"]),
#     ", Unit_Nm.x=", to_str(r["Unit_Nm.x"]),
#     ", best_match=", to_str(r["best_match"]),
#     ", stringsAsFactors=FALSE)"
#   )
# })
# 
# lines<-c(
#   "all_resolved_write<-do.call(rbind, list(",
#   paste(rows, collapse=",\n"),
#   "))"
# )
# 
# writeLines(lines, "all_resolved.R")


# Merges ------------------------------------------------------------------
all_resolved<-all_resolved %>% # Filtering non-matched park data
  filter(!is.na(best_match))

visits_mapped<-ann_visits %>%
  inner_join(
    all_resolved,
    by=c("State"="State_Nm.x", "Park"="Unit_Nm.x")
  )

final<-padus_s %>%
  inner_join(
    visits_mapped,
    by=c("State_Nm"="State", "Unit_Nm"="best_match"),
    relationship="many-to-many"
  )

st_write(final,"Out/annual_visits_PADUS_v1.gpkg")

