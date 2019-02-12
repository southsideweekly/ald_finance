rm(list = ls())
source('~/git/ald_finance/header.R')

## 00. Define parameters ----
## Wards
sw_wards <- c(20, 25,     ## first tier
              12, 15, 16, ## second tier
              14, 5, 22)  ## third tier
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
  mutate(ward_no = ward %>% gsub("[a-z]", "", .) %>% as.numeric()) %>% 
  rename(candidate = candidate_website)

## 20th Ward
ward_20_candidates <- raw_ids %>% filter(ward_no == 20) %>% 
  ## merge in "other committees"
  bind_rows(data.frame(ward_no = 20, 
                       candidate = "Kevin Bailey",
                       candidate_committee = "20th Ward Democratic Organization",
                       committee_id = 34349))
ward_20_ids <- ward_20_candidates %>% 
  filter(!is.na(committee_id)) %>% 
  distinct(committee_id) %>% 
  unlist()

## 02. Pull in donation data from Illinois Sunshine API ----
receipts_raw <- lapply(ward_20_ids, 
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
  left_join(ward_20_candidates)

## clean up receipts

### i. remove "other committee" receipts from main committee 
### (to prevent double-counting)
receipts_raw <- receipts_raw %>% 
  mutate(other_committees = paste(other_committees_1,
                                  other_committees_2, 
                                  other_committees_3,
                                  sep = ";") %>% 
           tolower() %>% str_replace_all(";na", "") %>% str_replace_all(";", "|") %>% str_remove("^na$"))
receipts_raw$remove_other <- lapply(1:nrow(receipts_raw),
                                    function(n){
                                      (receipts_raw$other_committees[n] != "" & 
                                         grepl(receipts_raw$other_committees[n],
                                               receipts_raw$last_name[n] %>% tolower))
                                    }) %>% unlist()

### ii. flag individual vs organization donors
receipts_raw$donor_type <- ifelse(is.na(receipts_raw$first_name), 
                                  "Organization", 
                                  "Individual")

### iii. light cleaning of names
receipts_raw <- receipts_raw %>% 
  mutate(last_name = ifelse(donor_type == "Individual", 
                            last_name, 
                            last_name %>% 
                              toupper() %>% 
                              str_remove_all("\\.|,|INC|LLC") %>% 
                              str_trim() %>% str_squish()))

## 03. Summary Stats ----

### 3ia. total donations by donor type ----
total_donations <- receipts_raw %>% 
  filter(remove_other == FALSE) %>% 
  group_by(candidate, donor_type) %>% 
  summarise(type_amount = sum(amount)) %>% 
  ungroup %>% group_by(candidate) %>% 
  mutate(total_amount = sum(type_amount, na.rm = T),
         total_amount = ifelse(is.na(total_amount), 0, total_amount),
         type_share = type_amount/total_amount*100,
         label_share = paste0(sprintf("%.0f", type_share), "%")) %>% 
  arrange(desc(total_amount))

total_donations_plot <- total_donations %>% 
  ggplot(aes(x = reorder(candidate, total_amount),
             y = type_amount,
             fill = factor(donor_type,
                           levels = c("Organization", "Individual")))) + 
  geom_bar(stat = "identity") + 
  labs(
    title = "Figure 1a: Total Receipts by Donor Type",
    y = "Receipts",
    x = "Candidate",
    fill = "Donor Type") +
  theme_classic() +
  scale_y_continuous(labels = dollar) +
  theme(plot.title = element_text(size = 18, face = "bold")) +
  geom_text(aes(label = label_share), position = position_stack(vjust = 0.5), size = 4) +
  # geom_text(aes(label = total_amount), size = 4) +
  coord_flip()

plot(total_donations_plot)

### 3ib. total donations by donor type ----

pct_donations_plot <- total_donations %>% 
  ggplot(aes(x = reorder(candidate, -total_amount),
             y = type_share,
             fill = donor_type)) + 
  geom_bar(stat = "identity") + 
  labs(
    title = "Figure 1b: Share of Receipts by Donor Type",
    y = "Receipts",
    x = "Candidate",
    fill = "Donor Type") +
  theme_classic() +
  scale_y_continuous(labels = percent) +
  theme(plot.title = element_text(size = 18, face = "bold")) +
  geom_text(aes(label = label_share), position = position_stack(vjust = 0.5), size = 4)


plot(pct_donations_plot)

### 3ii. larges donors ----
notable_donors <- receipts_raw %>% 
  filter(remove_other == F,
         !is.na(amount)) %>% 
  ungroup %>% 
  group_by(candidate, first_name, last_name, donor_type) %>% 
  summarise(amount= sum(amount)) %>% 
  ungroup %>% 
  group_by(candidate) %>% 
  arrange(candidate, desc(amount)) %>% 
  mutate(order = 1:n()) %>% 
  filter(order <= 10) %>%
  left_join(total_donations %>% distinct(candidate, total_amount)) %>% 
  mutate(share = amount/total_amount,
         name = ifelse(donor_type == "Individual", 
                       paste(first_name, last_name),
                       last_name)) %>% 
  ungroup %>% group_by(candidate) %>% 
  select(candidate, name, amount, share, order)

## write out names, add in description and load back
write.csv(notable_donors,
          "~/data/ald_finance/02_ward_demo/notable_donors.csv",
          row.names = F, na = "")

notable_donors %>% 
  filter(order <= 3) %>% 
  select(-order) %>% 
  ## arrange in descending order of total receipts
  left_join(total_donations %>% distinct(candidate, total_amount)) %>% 
  arrange(desc(total_amount)) %>% select(-total_amount) %>% 
  ## create table
  gt() %>% 
  tab_header(
    title = md("**Figure 3: Top Donors**"),
    subtitle = md("*25th Ward*")
  ) %>% 
  fmt_currency(
    columns = vars(amount), 
    currency = "USD",
    decimals = 0
  ) %>% 
  fmt_percent(
    columns = vars(share),
    decimals = 1
  ) %>% 
  cols_label(
    name = "",
    amount = "Amount",
    share = "Share of Total Receipts"
  ) %>% 
  tab_source_note(
    source_note = "Source: Illinois Sunshine"
  ) %>% 
  tab_options(
    stub_group.font.weight = "bold"
  )

### 3iii. donor stats, by type ----
donor_stats <- receipts_raw %>% 
  filter(remove_other == F, 
         !is.na(amount)) %>% 
  ungroup %>%
  group_by(candidate, donor_type, first_name, last_name) %>%
  summarise(amount = sum(amount)) %>%
  ungroup %>% 
  group_by(candidate, donor_type) %>% 
  summarise(
    count = n(),
    median = median(amount),
    total_amt = sum(amount))
# min = min(amount),
# max = max(amount),
# mean = mean(amount))

donor_stats %>% 
  ## arrange in descending order of total receipts
  left_join(total_donations %>% distinct(candidate, total_amount)) %>% 
  arrange(desc(total_amount)) %>% select(-total_amount) %>%
  ## create table
  gt() %>% 
  tab_header(
    title = md("**Figure 2: Summary of Receipts**"),
    subtitle = md("*25th Ward*")
  ) %>% 
  fmt_currency(
    columns = vars(total_amt, median), 
    currency = "USD",
    decimals = 0
  ) %>% 
  cols_label(
    donor_type = "",
    count = html("Number of<br>Donors"),
    median = html("Median<br>Donation"),
    total_amt = "Total Amount"
  ) %>% 
  tab_source_note(
    source_note = "Source: Illinois Sunshine"
  ) %>% 
  tab_options(
    stub_group.font.weight = "bold"
  )

