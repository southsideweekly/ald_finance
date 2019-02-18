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

## 14th Ward
ward_14_candidates <- raw_ids %>% filter(ward_no == 14) %>% 
  ## merge in "other committees"
  bind_rows(data.frame(ward_no = c(14, 14),
                       candidate = c("Ed Burke", "Ed Burke"),
                       candidate_committee = c("14th Ward Regular Democratic Org","Burnham Committee"),
                       committee_id = c(499, 16891),
                       other_committees_1 = c("Burnham Committee", "14th Ward Regular Democratic Org"),
                       other_committees_2 = c("Friends of Edward M. Burke", "Friends of Edward M. Burke")))
ward_14_ids <- ward_14_candidates %>% 
  filter(!is.na(committee_id)) %>% 
  distinct(committee_id) %>% 
  unlist()

## 02. Pull in donation data from Illinois Sunshine API ----
## --- ISSUE WITH ILLINOIS SUNSHINE API - UPDATE WHEN API ONLINE ----
# receipts_raw <- lapply(ward_5_ids, 
#                        function(curr_id){
#                          # curr_id = ward_5_ids[1]
#                          phrase <- paste0(api_link, 
#                                           "?committee_id=", curr_id, 
#                                           "&received_date__ge=", start_date,
#                                           "&datatype=csv")
#                          if (try(read.csv(phrase), silent = T) %>% class != "try-error") {
#                            read_csv(phrase, 
#                                     col_types = "nnnncccnnnccccccccccccccccccccc")
#                          } else {
#                            data.frame(committee_id = curr_id)
#                          }
#                        }) %>% 
#   bind_rows() %>% 
#   left_join(ward_5_candidates)

## import and clean .csv files from Illinois Sunshine ----
file_names <- list.files("~/data/ald_finance/02_ward_demo/14th Ward/import/")
receipts_raw <- lapply(file_names, function(file){
  read_csv(glue("~/data/ald_finance/02_ward_demo/14th Ward/import/{file}"),
           col_types = "nnnncccnnnccccccccccccccccccccc")
}) %>% 
  bind_rows() %>% 
  filter(as.Date(received_date) >= start_date) %>% 
  left_join(ward_14_candidates)


## clean up receipts

### i. remove "other committee" receipts from main committee 
### (to prevent double-counting)
receipts_raw <- receipts_raw %>% 
  mutate(other_committees = paste(other_committees_1,
                                  other_committees_2, 
                                  other_committees_3,
                                  sep = ";") %>% 
           tolower() %>% str_replace_all(";na", "") %>% str_replace_all(";", "|") %>% str_remove("^na$") %>% 
           str_remove("the "))
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
  mutate(candidate_clean = candidate %>% 
           str_replace_all(" ", "\n") %>% 
           str_replace("\nG\\.", " G\\.")) %>% 
  mutate(total_amount = sum(type_amount, na.rm = T),
         total_amount = ifelse(is.na(total_amount), 0, total_amount),
         type_share = type_amount/total_amount*100,
         label_share = paste0(sprintf("%.0f", type_share), "%"),
         label_total = ifelse(donor_type == "Organization" & total_amount > 0, "", dollar(round(total_amount)))) %>% 
  arrange(desc(total_amount))

## set note/source to use for all exhibits
note_source <- "Note: Total receipts for Ed Burke include contributions to the 14th Ward Regular Democratic Org
and The Burnham Committee, both of which are chaired by Ed Burke.
Source: illinoissunshine.org"

total_donations_plot <- total_donations %>% 
  ggplot(aes(x = reorder(candidate_clean, total_amount),
             y = type_amount,
             fill = factor(donor_type,
                           levels = c("Organization", "Individual")))) + 
  geom_bar(stat = "identity") + 
  labs(
    title = "Total Receipts by Donor Type",
    subtitle = "14th Ward",
    y = "",
    x = "",
    fill = "",
    caption = note_source) +
  theme_classic() +
  scale_y_continuous(labels = dollar, expand = c(0,10),
                     limit = c(0, 5700000), 
                     breaks = NULL) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 22),
        legend.position = "bottom",
        legend.text = element_text(size = 18),
        plot.caption = element_text(hjust = 0, size = 12),
        plot.subtitle = element_text(hjust = 0.5, size = 18),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_text(size = 18),
        axis.text = element_text(color = "black", face = "bold", size = 18)) +
  # geom_label(aes(label = label_share),
  #            color = "black",
  #            fill = "white",
  #            position = position_stack(vjust = 0.5), size = 8) +
  coord_flip() +
  scale_fill_grey() + 
  geom_text(aes(x = reorder(candidate_clean, total_amount),
                y = total_amount,
                label = label_total,
                fill = NULL),
            data = total_donations,
            hjust = -0.25,
            size = 6, 
            fontface = "bold") + 
  guides(fill = guide_legend(reverse = TRUE))

plot(total_donations_plot)

### 3ib. total donations by donor type ----
# 
# pct_donations_plot <- total_donations %>% 
#   ggplot(aes(x = reorder(candidate, -total_amount),
#              y = type_share,
#              fill = donor_type)) + 
#   geom_bar(stat = "identity") + 
#   labs(
#     title = "Figure 1b: Share of Receipts by Donor Type",
#     y = "Receipts",
#     x = "Candidate",
#     fill = "Donor Type") +
#   theme_classic() +
#   scale_y_continuous(labels = percent) +
#   theme(plot.title = element_text(size = 18, face = "bold")) +
#   geom_text(aes(label = label_share), position = position_stack(vjust = 0.5), size = 4)
# 
# 
# plot(pct_donations_plot)

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
  left_join(total_donations %>% distinct(candidate, total_amount)) %>% 
  mutate(share = amount/total_amount,
         name = ifelse(donor_type == "Individual", 
                       paste(first_name, last_name),
                       last_name)) %>% 
  ungroup %>% group_by(candidate) %>% 
  select(candidate, name, amount, share, order, donor_type)

# notable_donors %>% 
#   filter(order <= 3) %>% 
#   select(-order, -donor_type) %>% 
#   ## arrange in descending order of total receipts
#   left_join(total_donations %>% distinct(candidate, total_amount)) %>% 
#   arrange(desc(total_amount)) %>% select(-total_amount) %>% 
#   ## create table
#   gt() %>% 
#   tab_header(
#     title = md("**Figure 3: Top Donors**"),
#     subtitle = md("14th Ward")
#   ) %>% 
#   fmt_currency(
#     columns = vars(amount), 
#     currency = "USD",
#     decimals = 0
#   ) %>% 
#   fmt_percent(
#     columns = vars(share),
#     decimals = 1
#   ) %>% 
#   cols_label(
#     name = "",
#     amount = "Amount",
#     share = "Share of Total Receipts"
#   ) %>% 
#   tab_source_note(
#     source_note = "Source: illinoissunshine.org"
#   ) %>% 
#   tab_options(
#     stub_group.font.weight = "bold"
#   )

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

candidates <- unique(receipts_raw$candidate[which(receipts_raw$amount > 0)])

donor_stats %>% 
  ## create null rows where donor type is missing
  right_join(expand.grid(candidate = candidates, 
                         donor_type = c("Individual", "Organization"))) %>% 
  mutate(count = ifelse(is.na(count), 0, count)) %>% 
  ## arrange in descending order of total receipts
  left_join(total_donations %>% distinct(candidate, total_amount)) %>% 
  arrange(desc(total_amount)) %>% select(-total_amount) %>%
  ## create table
  gt() %>% 
  tab_header(
    title = md("**Summary of Receipts**"),
    subtitle = md("14th Ward")
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
    source_note = html("Note: Total receipts for Ed Burke include contributions to the 14th Ward Regular Democratic Org
    and The Burnham Committee, both of which are chaired by Ed Burke.<br>
    Source: illinoissunshine.org")
  ) %>% 
  tab_options(
    stub_group.font.weight = "bold"
  )

## 4. Interactive polygon chart ----

walk(candidates, function(c){
  
  ### 4i. generate cirle coordinates ----
  # curr_candidate <- candidates[1]
  curr_candidate <- c
  curr_donors <- notable_donors %>% filter(candidate == curr_candidate)
  packing <- circleProgressiveLayout(curr_donors$amount, sizetype = "area")
  df_circle <- bind_cols(curr_donors, packing) %>% 
    mutate(text = glue("Donor: {name}\nAmount: {dollar(amount)}") %>% 
             str_remove_all("'"))
  df_gg <- circleLayoutVertices(packing, npoints = 50)
  
  ### 4ii. ---- 
  circle_plot <- ggplot() +
    geom_polygon_interactive(
      data = df_gg, 
      aes(x, y, group = id, 
          fill = factor(df_circle$donor_type[id],
                        levels = c("Organization", "Individual")), 
          tooltip = df_circle$text[id], 
          data_id = id),
      color = "black") +
    theme_void() +
    theme(plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
          legend.position = "bottom",
          legend.text = element_text(size = 18),
          plot.caption = element_text(hjust = 0, size = 12),
          plot.subtitle = element_text(size = 18, hjust = 0.5)) + 
    coord_equal() +
    labs(
      title = "Receipts by Donor",
      subtitle = glue("14th Ward - {curr_candidate}"),
      fill = "")
  
  # plot(circle_plot)
  circle_widget <- ggiraph(ggobj = circle_plot, width_svg = 10, height_svg = 10)
  htmlwidgets::saveWidget(widget = circle_widget, 
                          file = glue("~/data/ald_finance/02_ward_demo/14th Ward/Figure 4 Circle Widget - {curr_candidate}.html"))
})

## 4iii. one big circle plot!
packing <- circleProgressiveLayout(notable_donors$amount, sizetype = "area")
df_circle <- bind_cols(notable_donors, packing) %>% 
  mutate(text = glue("Candidate: {candidate}\nDonor: {name}\nAmount: {dollar(amount)}") %>% 
           str_remove_all("'"))
df_gg <- circleLayoutVertices(packing, npoints = 50)
circle_plot <- ggplot() +
  geom_polygon_interactive(
    data = df_gg, 
    aes(x, y, group = id, 
        fill = factor(df_circle$candidate[id],
                      levels = unique(df_circle$candidate)), 
        tooltip = df_circle$text[id], 
        data_id = id),
    color = "black") +
  theme_void() +
  theme(legend.position = "right",
        plot.title = element_text(size = 18, face = "bold")) + 
  coord_equal() +
  labs(
    title = "Figure 4: Receipts by Donor",
    subtitle = glue("14th Ward"),
    fill = "Donor Type",
    caption = "Source: illinoissunshine.org")

# plot(circle_plot)
circle_widget <- ggiraph(ggobj = circle_plot, width_svg = 10, height_svg = 10)
htmlwidgets::saveWidget(widget = circle_widget, 
                        file = glue("~/data/ald_finance/02_ward_demo/14th Ward/Figure 4 Circle Widget - all candidates.html"))
