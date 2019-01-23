rm(list = ls())
source('~/git/sandbox/header.R')

## 00. Define parameters ----
## Wards
sw_wards <- c(3:25, 34)
## Start date - day after last municipal election -- SUBJECT TO CHANGE -- 
start_date <- "2015-02-26"
## Sunshine Illinois API link
api_link <- "http://illinoissunshine.org/api/receipts/"

## 01. Pull in committee IDs ----
# tdl_url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vRIKBkKCDVZOiflft-Qc5VCRlXCnVzZUsuvx6YzanMhKQl1mf-wWHiUVXtRY3YHVhTDvP_LgAEk0i9e/pubhtml#"
# gs_url(tdl_url)
# extract_key_from_url(tdl_url)

sheet_list <- gs_ls("TDL - Aldermanic Spreadsheet")
tdl_link <- gs_title("TDL - Aldermanic Spreadsheet")
raw_ids <- gs_read(ss = tdl_link)
raw_ids <- raw_ids %>% 
  rename_all(tolower) %>% 
  rename_all(function(x){gsub("[()]", "", x)}) %>% 
  rename_all(function(x){gsub(" |-", "_", x)}) %>% 
  mutate(ward_no = x1 %>% gsub("[a-z]", "", .) %>% as.numeric())

sw_ids_full <- raw_ids %>% 
  filter(ward_no %in% sw_wards,
         !grepl("withdrew|Removed|Withdrawn", petition_objection_status))

sw_ids <- sw_ids_full$committee_id[!is.na(sw_ids_full$committee_id)]

## 02. pull data from Sunshine Illinois' API ----
receipts_raw <- lapply(sw_ids, 
                      function(curr_id){
                        # curr_id = sw_ids[10]
                        phrase <- paste0(api_link, 
                                         "?committee_id=", curr_id, 
                                         "&received_date__ge=", start_date,
                                         "&datatype=csv")
                        if (try(read.csv(phrase), silent = T) %>% class != "try-error") {
                          read_csv(phrase, 
                                   col_types = "nnnncccnnnccccccccccccccccccccc")
                        } else {
                          data.frame(committee_id = curr_id)
                        }
                      }) %>% 
  bind_rows() %>% 
  left_join(sw_ids_full)

## 03. Clean up donor names ----

# ---- ON HOLD -----

# receipts_raw <- receipts_raw %>% 
#   mutate(last_name_new = gsub(",|-", " ", last_name %>% tolower) %>% 
#            sub("&", "and", .) %>%
#            sub("street", "st", .) %>%
#            gsub("\\.|#| inc| llc| pac|-?political action committee|companies|ltd", "", .) %>% 
#            sub("  ", " ", .) %>% 
#            trimws())
# 
# ## individuals to map 
# ## note - some duplicate names due to occupation/employer
# ## unduped ~2,700 individuals
# individuals <- receipts_raw %>% 
#   filter(!is.na(first_name)) %>% 
#   distinct(first_name, last_name, occupation, employer, address1) %>% 
#   arrange(last_name, first_name)
# 
# ## corporations to investigate
# 
# # w/ address 
# corporations_addr <- receipts_raw %>% 
#   filter(is.na(first_name)) %>% 
#   distinct(last_name, last_name_new, address1, zipcode) %>% 
#   arrange(last_name) %>% 
#   distinct(last_name_new, address1, zipcode, .keep_all = T)
# 
# write.csv(corporations_addr, 
#           "~/data/sandbox/01_build_alderman_data/corporations_addr.csv")
# 
# # w/o address
# corporations <- receipts_raw %>% 
#   filter(is.na(first_name)) %>% 
#   distinct(last_name, last_name_new) %>% 
#   arrange(last_name) %>% 
#   distinct(last_name_new, .keep_all = T)
# 
# write.csv(corporations_addr, 
#           "~/data/sandbox/01_build_alderman_data/corporations.csv")

## 04. Assign industry to corporate donors ----
## first-pass using regex, then fill in by hand ##

## clean up names
clean_names <- receipts_raw %>% 
  # filter to corperate donors
  filter(is.na(first_name)) %>% 
  ## remove duplicates
  distinct(last_name) %>% 
  # remove useless terms
  mutate(last_name_clean = str_remove_all(last_name %>% tolower, 
                                      "&| inc\\.?$| llc$| co\\.| co$|,|\\.| ltd| llp|^la |'") %>% 
           str_replace_all(" e\\.? | s\\.? | w\\.? | n\\.? | and |-", " ") %>% 
           str_squish() %>% 
           str_trim()) %>% 
  arrange(last_name)

## look at common terms 
common_terms <- clean_names %>% 
  distinct(last_name_clean) %>% 
  unlist() %>% 
  str_split(" ") %>% 
  unlist() %>% 
  table %>% 
  data.frame %>% 
  arrange(desc(Freq))

## flags for common terms
clean_names <- clean_names %>% 
  ## developers
  mutate(flag_developer = grepl("construction|development|property|properties|contractors|building|design|architects?|studio gang|contracting|demolition|realty|real estate", last_name_clean),
         flag_union = grepl("union|local| lu( |$)|seiu", last_name_clean),
         flag_pol = grepl("citizens?|committee|for congress|for mayor|rahm|berrios|pac|political|friends?|democratic|ipo", last_name_clean),
         flag_legal = grepl("associates|attorneys?|atty|law", last_name_clean))

## 05. summary stats

## corp vs individual amounts, by committee
ss_corp_ind <- receipts_raw %>% 
  group_by(committee_id, ward_no, candidate_website) %>% 
  summarise(total_receipts = sum(amount, na.rm = T), 
            corp_receipts = sum(ifelse(is.na(first_name), amount, 0), na.rm = T),
            ind_receipts = sum(ifelse(is.na(first_name), 0, amount), na.rm = T),
            total_count = sum(!is.na(amount)), 
            corp_count = sum(is.na(first_name) & !is.na(amount)),
            ind_count = sum(!is.na(first_name) & !is.na(amount))) %>% 
  mutate(corp_share = corp_receipts / total_receipts,
           ind_share = ind_receipts / total_receipts) %>% 
  arrange(ward_no)

## top # donors, by committee
ss_top_donors <- receipts_raw %>% 
  group_by(committee_id)
