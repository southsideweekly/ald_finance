rm(list = ls())
source('~/git/ald_finance/header.R')

## 00. Define parameters ----
## Wards
sw_wards <- c(3:25, 34)
## Start date - day after last municipal election -- SUBJECT TO CHANGE -- 
start_date <- "2015-02-26"
## Sunshine Illinois API link
api_link <- "http://illinoissunshine.org/api/receipts/"

## 01. Pull in committee IDs ----
sheet_list <- gs_ls()

## pull in committee IDs from TDL table
tdl_link <- gs_title("TDL - Aldermanic Spreadsheet")
raw_ids <- gs_read(ss = tdl_link)
## clean up variable names
raw_ids <- raw_ids %>% 
  rename_all(tolower) %>% 
  rename_all(function(x){gsub("[()]", "", x)}) %>% 
  rename_all(function(x){gsub(" |-", "_", x)}) %>% 
  mutate(ward_no = x1 %>% gsub("[a-z]", "", .) %>% as.numeric())

## drop candidates who are removed or withdrawn
sw_ids_full <- raw_ids %>% 
  filter(ward_no %in% sw_wards,
         !grepl("withdrew|Removed|Withdrawn", petition_objection_status))

## unique list of committee ids
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
  left_join(sw_ids_full) %>% 
  ## drop donations after 1/27/2019
  filter(received_date <= as.Date("2019-01-27"))

## 03. Clean up donor names ----

## From Jasmine -- will need to fold in dedup ##
clean_org_names <- gs_read(ss = gs_title("corporate reference"))
clean_org_names_unique <- clean_org_names %>% 
  select(cluster_id, last_name_new) %>% 
  distinct(cluster_id, .keep_all = T)

## merge clean org names onto data
receipts_id <- receipts_raw %>% 
  ## remove double-spacing
  mutate(last_name = last_name %>% str_squish(),
         address1 = address1 %>% str_squish()) %>% 
  ## merge on cluster_id
  left_join(clean_org_names %>% distinct(cluster_id, last_name, address1)) %>% 
  ## use cluster_id to merge on clean names
  left_join(clean_org_names_unique)

## pull out distinct names and afddresses for industry lookup
## NOTE - THERE APPEARS TO BE SOME JOIN ISSUES WITH THE CLEAN ORG NAMES ##
ind_lookup_exp <- receipts_id %>% 
  filter(is.na(first_name)) %>% 
  distinct(last_name, last_name_new, address1, zipcode)

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
  distinct(last_name, address1) %>% 
  # remove useless terms
  mutate(last_name_clean = str_remove_all(last_name %>% tolower, 
                                      "&| inc\\.?$| llc$| co\\.| co$|,|\\.| ltd| llp|^la |'") %>% 
           str_replace_all(" e\\.? | s\\.? | w\\.? | n\\.? | and |-|#", " ") %>% 
           str_replace_all("political action committee", "pac") %>% 
           str_remove(" company$| co$") %>% 
           str_squish() %>% 
           str_trim(),
         address1_new = str_remove_all(address1 %>% tolower,
                                       "\\.|,| (suite|ste|apt|fl|floor|\\#|unit|no)\\.? ?[0-9]*[a-z]*| [0-9]*[a-z]* (floor|fl|flr)") %>%
           str_trim() %>% 
           str_remove("street$| st$|drive$| dr$|road$| rd$|lane$| ln$|court$| ct$|avenue$| ave$| hwy$|boulevard| blvd$| ste$|place$| pl$|square$| sq$") %>% 
           str_remove(" se$| - ?$") %>% 
           str_replace(" e | s | w | n | east | south | west | north ", " ") %>% 
           str_replace("p o ", "po ") %>% 
           str_replace("pobox", "po box") %>% 
           str_replace("(dr)? martin luther king( jr)?", " mlk") %>% 
           str_squish()) %>% 
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
  mutate(flag_construction = grepl("construction|contractors|building|design|architects?|studio gang|contracting|demolition|roofing|masonry|builders?", last_name_clean),
         flag_realty = grepl("development|property|properties|realty|real estate", last_name_clean),
         flag_union = grepl("union|local| lu( |$)|seiu", last_name_clean),
         flag_pol = grepl("citizens?|committee|for congress|for mayor|rahm|berrios|pac|political|friends?|democratic| dem |ipo", last_name_clean),
         flag_legal = grepl("associates|attorneys?|atty|law", last_name_clean),
         flag_service = grepl("restaurant| pub|pizza|hotel|motel| spa | spa$|food|liquor|grocery|coffee|grill|funeral|bakery|salon|beauty|beverage", last_name_clean))

clean_names_out <- clean_names %>% 
  select(last_name_clean, address1_new, starts_with("flag_")) %>% 
  distinct()

write_csv(clean_names_out, "~/data/ald_finance/01_build_alderman_data/corporations_addr_flags.csv")

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
