# 1. Installing Packages ####

install.packages("dplyr")
install.packages("devtools")
install.packages("readr")
install.packages("moments")
install.packages("extrafont")
install.packages("tidyr")
install.packages("stringr")
install.packages("logistf")
install.packages("rstanarm")
install.packages("geepack")
install.packages("BayesFactor")
install.packages("sf")
install.packages("rnaturalearth")
install.packages("rnaturalearthdata")
install.packages("reshape2")
install.packages("ggplot2")
install.packages("scales")
install.packages("networkD3")
install.packages("tibble")
install.packages("ggthemes")

library(dplyr)
library(readr)
library(moments)
library(extrafont)
font_import()
loadfonts(device = "win")
library(tidyr)
library(stringr)
library(logistf)
library(rstanarm)
library(geepack)
library(BayesFactor)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(devtools)
devtools::install_github("ropensci/rnaturalearthhires")
library(reshape2)
library(ggplot2)
library(scales)
library(networkD3)
library(tibble)
library(ggthemes)
windowsFonts(`Times New Roman` = windowsFont("Times New Roman"))


## 2. Transforming Constituency-level Dataset into State-level Dataset ####

#Loading TCPD Dataset
#setwd()
tcpd_df <- read_csv("All_States_AE.csv") #Base TCPD Lok Dhaba Dataset

# Checking for any NA values in the Poll_No column
na_poll_no <- sum(is.na(tcpd_df$Poll_No))
if (na_poll_no > 0) {
  cat("There are", na_poll_no, "NA values in the Poll_No column.\n")
} else {
  # Filter the dataset where Poll_No is not equal to 0 and Position is 1
  tcpd_df_const <- tcpd_df %>%
    filter(Poll_No == 0, Position == 1) %>%
    select(-Poll_No, -Position) # Drop the redundant columns
}


### 3. Data Cleaning ####

#(i). Checking for duplicates and ties in the original dataset
tied_candidates <- tcpd_df %>%
  group_by(State_Name, Year, Constituency_No) %>%
  mutate(max_votes = max(Votes)) %>% # Calculate the maximum votes in each group
  filter(Votes == max_votes) %>% # Keep only the rows where the candidate's votes equal the maximum votes
  ungroup() %>%
  group_by(State_Name, Year, Constituency_No, Votes) %>%
  filter(n() > 1) %>% # Filter for groups where there's more than one candidate with the same number of votes
  ungroup() %>%
  select(-max_votes) # Remove the max_votes column as it's not needed anymore
# Displaying tied candidates
tied_candidates
# There are a few duplicates at Poll_No = 0. This either means that there was a tie in the votes or a row repeated as an error. 


#(ii). Addressing the duplicate rows
# Meghalaya, 1988, KHERAPARA:
# Two candidates have the same votes: CHAMBERUN MARAK (IND) and ROSTER M. SANGMA (INC) both with 2591 votes.
# According to the ECI, Chamberun Marak (IND) was ultimately elected from this constituency.
# Since his position is already mentioned as 1 in the original dataset, there should not have been any problems with the transformation.

# Maharashtra, 1962, MAHAD:
# Two candidates have the same votes: SAKHARAM VITHOBA SALUNKE (PSP) and SHANKAR BABAJI SAWANT (INC) both with 12664 votes.
# According to the ECI, Shankar Babaji Sawant (INC) was ultimately elected from this constituency. 
tied_maharashtra <- tcpd_df %>%
  filter(State_Name == "Maharashtra", 
         Year == 1962, Constituency_Name =="MAHAD")
tied_maharashtra
# In this case, the original dataset lists Sawant's position as 2 and Salunke's position as 1. Given the ECI results, I need to manually switch the information for the winning candidate.
# Resolution:
# Identifying the row to be replaced
row_to_replace <- which(tcpd_df_const$State_Name == "Maharashtra" &
                          tcpd_df_const$Year == 1962 &
                          tcpd_df_const$Constituency_Name == "MAHAD" &
                          tcpd_df_const$Candidate == "SAKHARAM VITHOBA SALUNKE")
# Identifying the row to use for replacement from df
replacement_row <- tcpd_df %>%
  filter(State_Name == "Maharashtra" &
           Year == 1962 &
           Constituency_Name == "MAHAD" &
           Candidate == "SHANKAR BABAJI SAWANT") %>%
  select(-c(Poll_No, Position)) # Removing the extra columns
# Replacing the row in df_transformed with the identified row from df
tcpd_df_const[row_to_replace, ] <- replacement_row
# Checking if the resolution was successful.
tied_maharashtra_resolved <- tcpd_df_const %>%
  filter(State_Name == "Maharashtra", 
         Year == 1962, Constituency_Name =="MAHAD")
tied_maharashtra_resolved
# Sawant has been updated as the winning candidate for the constituency.

# Assam, 2001
# Duplicate entry of the same candidate; already resolved by selecting only the first position.

# Arunachal Pradesh, 2019
# Multiple entries with NOTA and a candidate. 
# Three-member of the Bharatiya Janata Party, Phurpa Tsering from Dirang, Taba Tedir from Yachuli and Kento Jini from Along East were elected unopposed after others' candidature was rejected or the candidates withdrew themselves.
# No changes needed here since all these unoppossed candidates were already listed in the number 1 position while extracting from the original dataset.

# Karnataka, 2018
# There is a duplicate entry for the same candidate in the number 1 and 2 position; already resolved by selecting only the first position.

# Final check for duplicate entries for a constituency in a given state and election
duplicates <- tcpd_df_const %>%
  group_by(Constituency_No, State_Name, Assembly_No) %>%
  filter(n() > 1) %>%
  arrange(State_Name, Assembly_No, Constituency_No)
print(duplicates)
# There are no duplicate entries in the transformed dataset.


#(iii). Standardizing Data
# Ensuring that there are no impossible values (e.g., negative votes, votes greater than the population of a constituency).
# Identifying numeric and integer columns
numeric_vars <- names(tcpd_df_const)[sapply(tcpd_df_const, function(x) is.numeric(x) | is.integer(x))]
numeric_vars
# Check each numeric column for negative values
negative_values <- list()
for (var in numeric_vars) {
  neg_rows <- which(tcpd_df_const[[var]] < 0)
  if (length(neg_rows) > 0) {
    negative_values[[var]] <- neg_rows
  }
}
# Print out columns with negative values and their respective row numbers
if (length(negative_values) > 0) {
  cat("Columns with negative values and respective row numbers:\n")
  print(negative_values)
} else {
  cat("No negative values found in the dataset's numeric columns.")
}

#Checking if Votes, Valid_Votes greater than Electors
excessive_votes <- tcpd_df_const %>%
  filter(Votes > Electors | Valid_Votes > Electors)
# If this returns any rows, it indicates votes exceeding the population in those rows.
print(excessive_votes)
# No such entries found

# For categorical variables, checking for inconsistent categories or typos.
# List of columns to check for unique values
columns_to_check <- c(
  'Sex', 'Party', 'Candidate_Type', 'Constituency_Type', 'Deposit_Lost',
  'Party_Type_TCPD', 'last_poll', 'Same_Constituency', 'Same_Party',
  'Turncoat', 'Recontest', 'MyNeta_education', 'TCPD_Prof_Main',
  'TCPD_Prof_Second'
)
get_unique_values <- function(column_name, data) {
  return(unique(data[[column_name]]))
}
# Apply the function to each column and store the result in a list
unique_values_list <- lapply(columns_to_check, get_unique_values, data = tcpd_df_const)
# Print the unique values for each column
for (i in seq_along(columns_to_check)) {
  cat('Unique values for', columns_to_check[i], ':', unique_values_list[[i]], '\n\n')
}

# PROBLEM: Inconsistencies in Sex column, Candidate_Type column, Constituency_Type column
# RESOLUTION:
# Standardizing 'Sex' column to have only "Male" and "Female" categories
tcpd_df_const$Sex <- ifelse(tcpd_df_const$Sex == "MALE", "M", 
                             ifelse(tcpd_df_const$Sex == "FEMALE", "F", tcpd_df_const$Sex))
# Now check the unique values to ensure changes have been made
unique(tcpd_df_const$Sex)
# Replacing 'GENERAL' with 'GEN' in the 'Candidate_Type' column
tcpd_df_const$Candidate_Type[tcpd_df_const$Candidate_Type == "GENERAL"] <- "GEN"
# Check the unique values to ensure the change has been made
unique(tcpd_df_const$Candidate_Type)

# Removing redundant patterns like " (SC)", " (ST)", or " (BL)" from the 'Constituency_Name' column
# To locate these entries I use grep() to get the indices or value=TRUE to get the values
indices <- grep(" \\(SC\\)| \\(ST\\)| \\(BL\\)$", tcpd_df_const$Constituency_Name)
entries_with_patterns <- tcpd_df_const$Constituency_Name[indices]
print(entries_with_patterns)
tcpd_df_const$Constituency_Name <- gsub(" \\(SC\\)| \\(ST\\)| \\(BL\\)$", "", tcpd_df_const$Constituency_Name)
# Checking some of the entries to ensure the pattern was removed
head(tcpd_df_const$Constituency_Name)

#Final dataset after cleaning
View(tcpd_df_const)
# Manually checking for a few random rows
sample_rows <- tcpd_df_const %>% sample_n(5)
print(sample_rows)
# One can manually cross-check these rows with data from the Election Commission of India.


#### 4. Data Aggregation ####

#(i). Adding a column for total number of constituencies (Total_Constituencies) and majority constituencies (Majority_Constituencies).
calculate_majority <- function(x) {
  if (x %% 2 == 0) {
    return((x / 2) + 1)
  } else {
    return(ceiling(x / 2))
  }
}
tcpd_df_const <- tcpd_df_const %>%
  group_by(State_Name, Year, Assembly_No) %>%
  mutate(Total_Constituencies = n_distinct(Constituency_No)) %>%
  mutate(Majority_Constituencies = sapply(Total_Constituencies, calculate_majority)) %>%
  ungroup()
# Checking a few samples to ensure the calculations are correct
sample_rows <- tcpd_df_const %>% sample_n(5)
print(sample_rows)

#(ii). Adding a column for the BJP vote share in each constituency (BJP_Vote_Share_Percentage)
bjp_votes_summary <- tcpd_df %>%
  filter(Poll_No == 0) %>%
  group_by(State_Name, Constituency_No, Assembly_No, Year) %>%
  summarise(
    BJP_Votes_Sum = sum(Votes[Party == "BJP"], na.rm = TRUE),
    Total_Votes_Sum = sum(Votes, na.rm = TRUE),
    Reported_Valid_Votes = first(Valid_Votes),
    .groups = "drop"
  ) %>%
  mutate(
    BJP_Vote_Share_Percentage = ifelse(Total_Votes_Sum > 0, (BJP_Votes_Sum / Total_Votes_Sum) * 100, 0),
    Discrepancy = Reported_Valid_Votes - Total_Votes_Sum
  )
# Checking for non-zero discrepancies and printing them out
non_zero_discrepancies <- filter(bjp_votes_summary, Discrepancy != 0)
if(nrow(non_zero_discrepancies) > 0) {
  print("There are non-zero discrepancies in the following rows:")
  print(non_zero_discrepancies)
} else {
  print("There are no non-zero discrepancies.")
}
# Joining the BJP vote share from votes_summary to tcpd_df_const
tcpd_df_const <- tcpd_df_const %>%
  left_join(bjp_votes_summary %>% select(State_Name, Constituency_No, Assembly_No, Year, BJP_Vote_Share_Percentage), 
            by = c("State_Name", "Constituency_No", "Assembly_No", "Year"))

#(iii). Adding a column for INC vote share (INC_Vote_Share_Percentage)
inc_votes_summary <- tcpd_df %>%
  filter(Poll_No == 0) %>%
  group_by(State_Name, Constituency_No, Assembly_No, Year) %>%
  summarise(
    INC_Votes_Sum = sum(Votes[Party == "INC"], na.rm = TRUE),
    Total_Votes_Sum = sum(Votes, na.rm = TRUE),
    Valid_Votes = first(Valid_Votes), # Assumes the number of valid votes is the same for each row of the group
    Discrepancy = Valid_Votes - Total_Votes_Sum,
    .groups = 'drop' # This drops the grouping structure after summarisation
  ) %>%
  mutate(INC_Vote_Share_Percentage = (INC_Votes_Sum / Total_Votes_Sum) * 100)
non_zero_discrepancies <- filter(inc_votes_summary, Discrepancy != 0)
if(nrow(non_zero_discrepancies) > 0) {
  print("There are non-zero discrepancies in the following rows:")
  print(non_zero_discrepancies)
} else {
  print("There are no non-zero discrepancies.")
}
tcpd_df_const <- tcpd_df_const %>%
  left_join(inc_votes_summary %>% select(State_Name, Constituency_No, Assembly_No, Year, INC_Vote_Share_Percentage), 
            by = c("State_Name", "Constituency_No", "Assembly_No", "Year"))

#(iv). Discrepancy Check for the Vote Share Columns
# First, filtering the dataset for rows where the winning party is BJP or INC
winning_bjp_inc <- tcpd_df %>%
  filter(Party %in% c("BJP", "INC") & Position == 1 & Poll_No ==0) %>%
  select(State_Name, Constituency_No, Assembly_No, Year, Party, Vote_Share_Percentage)
# Now, joining this with tcpd_df_const to get the calculated vote share percentages
comparison_df <- tcpd_df_const %>%
  left_join(winning_bjp_inc, by = c("State_Name", "Constituency_No", "Assembly_No", "Year"))
tolerance <- 0.01  # set the tolerance level to 0.01%
comparison_df <- comparison_df %>%
  mutate(
    Discrepancy_BJP = if_else(Party.x == "BJP", 
                              if_else(abs(Vote_Share_Percentage.y - BJP_Vote_Share_Percentage) > tolerance, 
                                      Vote_Share_Percentage.y - BJP_Vote_Share_Percentage, 0), 0),
    Discrepancy_INC = if_else(Party.x == "INC", 
                              if_else(abs(Vote_Share_Percentage.y - INC_Vote_Share_Percentage) > tolerance, 
                                      Vote_Share_Percentage.y - INC_Vote_Share_Percentage, 0), 0)
  )
significant_discrepancies <- comparison_df %>%
  filter(abs(Discrepancy_BJP) > tolerance | abs(Discrepancy_INC) > tolerance)
if (nrow(significant_discrepancies) > 0) {
  print("Significant discrepancies found:")
  print(significant_discrepancies)
}
#There are five discrepancies in the data for INC vote share. I will address each of them:
#Kerala, 8: 30, 1982, K. K. Ramchandran, INC. He got 31858 votes out of a total of 65871 votes. TCPD reports his vote share to be 48.36. The magnitude of our discrepancy is -0.3959017.I update the INC_Vote_Share_Percentage to the correct calculations.
tcpd_df_const <- tcpd_df_const%>%
  mutate(
    INC_Vote_Share_Percentage = case_when(
      State_Name == "Kerala" & Year == 1982 & Assembly_No == 8 &  Constituency_No == 30 ~ Votes / Valid_Votes * 100,
      TRUE ~ INC_Vote_Share_Percentage
    )
  )
#West_Bengal, 9: 28, 1982, Choudhary Md. Abdulkarim, INC. He got 33508 votes out of 68959. TCPD reports vote share as 48.59. The magnitude of my discrepancy is high: -14.0457691. This might be due to incorrect reporting of the votes of other candidates. I update this entry to correct calculations.
tcpd_df_const <- tcpd_df_const %>%
  mutate(
    INC_Vote_Share_Percentage = case_when(
      State_Name == "West_Bengal" & Year == 1982 & Assembly_No == 9 & Constituency_No == 28 ~ Votes / Valid_Votes * 100,
      TRUE ~ INC_Vote_Share_Percentage
    )
  )
#Maharashtra, 3: 166, 1972, S. Iqbal Hussain, INC. He received 18308 votes out of 36820. TCPD reports his vote share as 49.72 compared to my calculations at 59.19337. I update the rows based on new calculations.
tcpd_df_const <- tcpd_df_const %>%
  mutate(
    INC_Vote_Share_Percentage = case_when(
      State_Name == "Maharashtra" & Year == 1972 & Assembly_No == 3 & Constituency_No == 166 ~ Votes / Valid_Votes * 100,
      TRUE ~ INC_Vote_Share_Percentage
    )
  )
#Haryana, 3: 79, 1972, Harkishan Lal, INC. He received 26581 votes out of 51295. TCPD reports his vote share as 51.82 compared to my 52.76343. I adjust my calculations.
tcpd_df_const <- tcpd_df_const %>%
  mutate(
    INC_Vote_Share_Percentage = case_when(
      State_Name == "Haryana" & Year == 1972 & Assembly_No == 3 & Constituency_No == 79 ~ Votes / Valid_Votes * 100,
      TRUE ~ INC_Vote_Share_Percentage
    )
  )
#Odisha, 4: 70, 1967, M.nayak, INC. He received 7523 votes out of 18747. TCPD reports his vote share as 40.13 whereas my calculations say 49.93332. 
tcpd_df_const <- tcpd_df_const %>%
  mutate(
    INC_Vote_Share_Percentage = case_when(
      State_Name == "Odisha" & Year == 1967 & Assembly_No == 4 & Constituency_No == 70 ~ Votes / Valid_Votes * 100,
      TRUE ~ INC_Vote_Share_Percentage
    )
  )
# Performing the same discrepancy test, we can check that there are no more discrepancies between the reported and calculate vote share for the BJP and the INC.


#(v). Adding a column for the party of the runner-up candidate in terms of vote share (Runner_Up_Party)
runner_ups <- tcpd_df %>%
  filter(Poll_No == 0) %>%
  group_by(State_Name, Constituency_No, Assembly_No, Year) %>%
  arrange(desc(Votes)) %>%
  slice(2) %>%
  ungroup() %>%
  select(State_Name, Constituency_No, Assembly_No, Year, Party) %>%
  rename(Runner_Up_Party = Party)
# Joining the runner-up information
tcpd_df_const <- tcpd_df_const %>%
  left_join(runner_ups, by = c("State_Name", "Constituency_No", "Assembly_No", "Year"))
# Updating runner-up information for uncontested constituencies
tcpd_df_const <- tcpd_df_const %>%
  mutate(Runner_Up_Party = ifelse(N_Cand == 1, "Uncontested", Runner_Up_Party))
# Double checking
single_candidate_constituencies <- tcpd_df_const %>%
  filter(N_Cand == 1)
View(single_candidate_constituencies)


#(vi). Adding Columns for the Largest Party by Vote Share (Largest_Party, Largest_Party_Vote_Share)
#Calculating the total votes by party for each state, assembly, and year
party_votes <- tcpd_df %>%
  filter(Poll_No==0) %>%
  group_by(State_Name, Assembly_No, Year, Party) %>%
  summarise(Total_Party_Votes = sum(Votes, na.rm = TRUE), .groups = 'drop')
#Calculating the total valid votes in each state, assembly, and year
total_valid_votes <- tcpd_df %>%
  filter(Poll_No == 0) %>%
  group_by(State_Name, Year, Assembly_No) %>%
  distinct(State_Name, Year, Assembly_No, Constituency_No, .keep_all = TRUE) %>% # Ensuring one entry per constituency
  summarise(Total_Valid_Votes = sum(Valid_Votes, na.rm = TRUE)) %>%
  ungroup()
#Joining the party votes with the total valid votes to calculate vote shares
party_vote_shares <- party_votes %>%
  left_join(total_valid_votes, by = c("State_Name", "Assembly_No", "Year")) %>%
  mutate(Vote_Share = Total_Party_Votes / Total_Valid_Votes * 100)
#Checking if the vote shares sum up to 100% for each group
vote_share_check <- party_vote_shares %>%
  group_by(State_Name, Year, Assembly_No) %>%
  summarise(Sum_Vote_Share = sum(Vote_Share, na.rm = TRUE)) %>%
  ungroup()
#Identifying the largest party by vote share
largest_party_vote_shares <- party_vote_shares %>%
  arrange(desc(Vote_Share)) %>%
  group_by(State_Name, Assembly_No, Year) %>%
  slice(1) %>%
  ungroup() %>%
  select(State_Name, Assembly_No, Year, Largest_Party = Party, Largest_Party_Vote_Share = Vote_Share)
#Joining the largest party vote shares with tcpd_df_const
tcpd_df_const <- tcpd_df_const %>%
  left_join(largest_party_vote_shares, by = c("State_Name", "Assembly_No", "Year"))


#(vii). Adding Columns for Largest Party by Seat Share (Largest_Party_By_Seats, Seats_Won)
#Counting the number of constituencies won by each party in each state, assembly, and year
party_seats <- tcpd_df %>%
  filter(Poll_No==0) %>%
  count(State_Name, Assembly_No, Year, Party, wt = Position == 1, name = "Seats_Won")
#Identifying the party with the most seats in each state, assembly, and year
largest_party_by_seats <- party_seats %>%
  arrange(desc(Seats_Won)) %>%
  group_by(State_Name, Assembly_No, Year) %>%
  slice(1) %>%
  ungroup() %>%
  select(State_Name, Assembly_No, Year, Largest_Party_By_Seats = Party, Seats_Won)
#Joining the largest party by seat share with tcpd_df_const
tcpd_df_const <- tcpd_df_const %>%
  left_join(largest_party_by_seats, by = c("State_Name", "Assembly_No", "Year"))


#(viii). Adding Columns for the Largest Party at the Centre (Largest_Party_Centre), their seats (LParty_Seats), whether or not there is a coalition at the centre (Coalition_Centre), name of coalition at the centre (Coalition_Name)
#Uploading the central government information (Source: ECI)
central_gov_info <- data.frame(
  Year = c(1951, 1957, 1962, 1967, 1971, 1977, 1980, 1984, 1989, 1991, 1996, 1998, 1999, 2004, 2009, 2014, 2019),
  Largest_Party_Centre = c("INC", "INC", "INC", "INC", "INC", "JP", "INC", "INC", "INC", "INC", "BJP", "BJP", "BJP", "INC", "INC", "BJP", "BJP"),
  LParty_Seats = c(364, 371, 361, 283, 352, 295, 353, 404, 197, 232, 161, 182, 182, 145, 206, 282, 303),
  Coalition_Centre = c(0, 0, 0, 0, 0, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0),
  Coalition_Name = c("No Coalition", "No Coalition", "No Coalition", "No Coalition", "No Coalition", "Janata Party + allies", "No Coalition", "No Coalition", "National Front + allies", "Congress + allies", "United Front", "NDA", "NDA", "UPA", "UPA", "No Coalition", "No Coalition")
)
#Creating a vector of election years for reference
election_years <- central_gov_info$Year
year_map <- data.frame(
  Year = min(tcpd_df_const$Year):max(tcpd_df_const$Year),
  Election_Year = sapply(min(tcpd_df_const$Year):max(tcpd_df_const$Year), function(x) max(election_years[election_years <= x]))
)
tcpd_df_const <- tcpd_df_const %>%
  left_join(year_map, by = "Year")
tcpd_df_const <- tcpd_df_const %>%
  left_join(central_gov_info, by = c("Election_Year" = "Year"))
tcpd_df_const <- tcpd_df_const %>%
  select(-Election_Year)


#(ix). Adding a Column to Check if Majority was Obtained in a State (Majority_Party) or if a Coalition Was Needed (Coalition_State)
tcpd_df_const <- tcpd_df_const %>%
  mutate(Majority_Party = ifelse(Seats_Won >= Majority_Constituencies, Largest_Party_By_Seats, "No Majority"))
tcpd_df_const <- tcpd_df_const %>%
  mutate(Coalition_State = ifelse(Majority_Party == "No Majority", 1, 0))


##### 5. Calculating Index Components ####

#(i). Organizing a New State-wise Dataset 
df_state <- tcpd_df_const %>%
  group_by(State_Name, Assembly_No, Year) %>%
  slice(1) %>%
  select(State_Name, Assembly_No, Year, Total_Constituencies, Majority_Constituencies, Largest_Party, Largest_Party_Vote_Share, Largest_Party_By_Seats, Seats_Won, Largest_Party_Centre, LParty_Seats, Coalition_Centre, Coalition_Name, Majority_Party, Coalition_State) %>%
  ungroup()

#(ii). Integrating a new dataset to identify the coalition leader party in cases of coalition governments (Coalition_Leader)
#Reading the new dataset with the Coalition_Leader column, which manually updates instances where there is no single majority party and instead a coalition.
coalition_data <- read_csv("potential_coalitions.csv")
#Selecting only the necessary columns (State_Name, Year, Assembly_No, Coalition_Leader)
coalition_data <- coalition_data %>%
  select(State_Name, Year, Assembly_No, Coalition_Leader)
#Merging the Coalition_Leader data into df_state
df_state <- df_state %>%
  left_join(coalition_data, by = c("State_Name", "Year", "Assembly_No"))
str(df_state)


#(iii). Adding a Column Calculating Turnover (Turnover)
calc_turnover <- function(state_data) {
  # Initialize the Turnover column to 1 by default
  state_data$Turnover <- rep(1, nrow(state_data))
  # Traverse through the dataframe from the second row onwards
  consec_reelections <- 0  # Initialize a counter for consecutive reelections
  for (i in 2:nrow(state_data)) {
    # Determine which party to use for comparison
    party_current <- ifelse(is.na(state_data$Coalition_Leader[i]), 
                            ifelse(state_data$Majority_Party[i] == "No Majority", state_data$Largest_Party_By_Seats[i], state_data$Majority_Party[i]), 
                            state_data$Coalition_Leader[i])
    party_previous <- ifelse(is.na(state_data$Coalition_Leader[i-1]), 
                             ifelse(state_data$Majority_Party[i-1] == "No Majority", state_data$Largest_Party_By_Seats[i-1], state_data$Majority_Party[i-1]), 
                             state_data$Coalition_Leader[i-1])
    if (party_current == party_previous) {
      consec_reelections <- consec_reelections + 1
      # If more than two consecutive reelections
      if (consec_reelections >= 2) {
        state_data$Turnover[i] <- 0
      }
    } else {
      consec_reelections <- 0 # Reset counter on party change
    }
  }
  # Set the first row to NA
  state_data$Turnover[1] <- NA
  return(state_data)
}
df_state <- df_state %>% group_by(State_Name) %>% group_modify(~ calc_turnover(.x)) %>% ungroup()


#(iv). Adding Columns Calculating ENOP (ENOP_Mean, ENOP_Median) and SOP (SOP)
#ENOP 
df_enop <- tcpd_df_const %>% group_by(State_Name, Assembly_No, Year) %>%
  summarize(
    ENOP_Mean = mean(ENOP, na.rm = TRUE),
    ENOP_Median = median(ENOP, na.rm = TRUE)
  ) %>% ungroup()
df_state <- df_state %>% left_join(df_enop, by = c("State_Name", "Assembly_No", "Year"))

#SOP 
df_state <- df_state %>%
  mutate(
    SOP = (Total_Constituencies - Seats_Won) / Total_Constituencies
  )


#(v). Adding a Column Calculating Participation (State_Turnout) 
df_turnout <- tcpd_df_const %>% 
  group_by(State_Name, Assembly_No, Year) %>%
  summarize(
    State_Turnout = sum(Valid_Votes, na.rm = TRUE) / sum(Electors, na.rm = TRUE)
  ) %>% ungroup()
df_state <- df_state %>% left_join(df_turnout, by = c("State_Name", "Assembly_No", "Year"))
df_state$State_Turnout[df_state$State_Turnout == Inf] <- NA

#Manually adding Turnout data for Mizoram (2018)
df_state[df_state$State_Name == "Mizoram" & df_state$Year == 2018, "State_Turnout"] <- 0.8161


###### 6. Creating a Subnational Democracy Index ####

#(i). Normalizing Component Measures
#Normalization function (min-max)
normalize <- function(x) {
  return((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
}
#Normalizing SOP (SOP_Normalized), State_Turnout (State_Turnout_Normalized), ENOP_Median (ENOP_Median_Normalized) 
df_state <- df_state %>%
  mutate(
    SOP_Normalized = normalize(SOP),
    State_Turnout_Normalized = normalize(State_Turnout),
    ENOP_Median_Normalized = normalize(ENOP_Median)
  )


#(ii). Calculating Contestation Components (Contestation_Normalized)
df_state <- df_state %>%
  mutate(
    #Contestation_Normalized: Average of normalized ENOP_Median and SOP
    Contestation_Normalized = (ENOP_Median_Normalized + SOP_Normalized) / 2
  )


#(iii). Calculating Unweighted Index (DemIndex) and Weighted Index (W_DemIndex)
df_state <- df_state %>%
  mutate(
    DemIndex = Turnover + (State_Turnout_Normalized * Contestation_Normalized)
  )
df_state <- df_state %>%
  mutate(
    W_DemIndex = 0.3 * Turnover + 0.7 * (State_Turnout_Normalized * Contestation_Normalized)
  )
df_state <- df_state %>%
  mutate(
    DemIndex_NoTurnover = (State_Turnout_Normalized * Contestation_Normalized)
  )
# Final check
summary(df_state)

######## 7. Backsliding Classifications ####

#Classification Based on Weighted Democracy Index

#(i). Filtering the dataset from the earliest available year and removing NA values
backsliding_data <- df_state %>%
  filter(!is.na(W_DemIndex)) %>%
  select(State_Name, Year, W_DemIndex) %>%
  arrange(State_Name, Year)


#(ii). Calculating the change in democracy score between consecutive election years for each state
backsliding_data <- backsliding_data %>%
  group_by(State_Name) %>%
  mutate(democracy_change = W_DemIndex - lag(W_DemIndex)) %>%
  ungroup()


#(iii). Initializing the classification and cumulative gain columns
backsliding_data <- backsliding_data %>%
  group_by(State_Name) %>%
  mutate(classification = "Non-Backsliding",  # Default classification
         cumulative_gain = 0,                 # Initialize cumulative gain tracker for recovery
         backsliding_start = NA_real_,        # Initialize backsliding start
         cumulative_consolidation_gain = 0,   # Initialize cumulative gain for consolidation
         consolidating_flag = FALSE)          # Flag to track if we are in a consolidating phase


#(iv). Looping through each state's data to classify backsliding, recovery, and consolidation
for(i in 2:nrow(backsliding_data)) {
  if(backsliding_data$State_Name[i] == backsliding_data$State_Name[i-1]) {
    # Handle Backsliding
    if(backsliding_data$classification[i-1] == "Backsliding") {
      # Update cumulative gain for recovery
      cumulative_gain <- backsliding_data$W_DemIndex[i] - backsliding_data$backsliding_start[i-1]
      backsliding_data$cumulative_gain[i] <- cumulative_gain
      # Check for Recovery
      if(cumulative_gain >= 0.1) {
        backsliding_data$classification[i] <- "Recovery"
        backsliding_data$consolidating_flag[i] <- FALSE  # Reset consolidation flag after recovery
      } else {
        backsliding_data$classification[i] <- "Backsliding"
        backsliding_data$backsliding_start[i] <- backsliding_data$backsliding_start[i-1]
      }
    }
    # Handle Recovery
    else if(backsliding_data$classification[i-1] == "Recovery") {
      backsliding_data$classification[i] <- "Non-Backsliding"
      backsliding_data$consolidating_flag[i] <- FALSE
    }
    # Handle New Backsliding
    if(backsliding_data$democracy_change[i] <= -0.1) {
      backsliding_data$classification[i] <- "Backsliding"
      backsliding_data$backsliding_start[i] <- backsliding_data$W_DemIndex[i]
      backsliding_data$consolidating_flag[i] <- FALSE  # Reset consolidation flag if backsliding starts
    }
    # Handle Consolidating
    else if (backsliding_data$classification[i-1] == "Non-Backsliding" || backsliding_data$classification[i-1] == "Recovery" || backsliding_data$classification[i-1] == "Consolidating") {
      # Check if currently consolidating
      if (backsliding_data$consolidating_flag[i-1] == TRUE) {
        if (backsliding_data$democracy_change[i] > 0) {  # Any positive gain keeps consolidation
          backsliding_data$classification[i] <- "Consolidating"
          backsliding_data$cumulative_consolidation_gain[i] <- backsliding_data$cumulative_consolidation_gain[i-1] + backsliding_data$democracy_change[i]
        } else {
          backsliding_data$classification[i] <- "Non-Backsliding"  # Revert to Non-Backsliding on any decrease
          backsliding_data$consolidating_flag[i] <- FALSE
        }
      }
      # Handle potential new consolidation
      else if (backsliding_data$democracy_change[i] > 0) {
        backsliding_data$cumulative_consolidation_gain[i] <- backsliding_data$cumulative_consolidation_gain[i-1] + backsliding_data$democracy_change[i]
        if (backsliding_data$cumulative_consolidation_gain[i] >= 0.1) {
          backsliding_data$classification[i] <- "Consolidating"
          backsliding_data$consolidating_flag[i] <- TRUE  # Set consolidation flag
        }
      }
    }
  }
}
print(backsliding_data)


#(v). Creating a dummy column for state-level backsliding (1 if classification == 'Backsliding', 0 otherwise)
backsliding_data <- backsliding_data %>%
  mutate(State_Backsliding = ifelse(classification == "Backsliding", 1, 0))

#(vi). Creating a second dataset to only include observations since 2013 (to 2023)
recent_backsliding_data <- backsliding_data %>%
  filter(Year >= 2013)

######### 8. National Level Dataset and Index Construction ####

#(i). Creating the Foundation
election_years <- c(1951, 1957, 1962, 1967, 1971, 1977, 1980, 1984, 1989, 1991, 1996, 1998, 1999, 2004, 2009, 2014, 2019)
assembly_numbers <- 1:length(election_years)
df_national <- tibble(
  Assembly_No = assembly_numbers,      # Assembly number for each general election
  Year = election_years,               # Corresponding year of the general election
  Turnout = numeric(length(election_years)),             # Placeholder for national turnout percentage,   
  SOP = numeric(length(election_years)),                 # Placeholder for Strength of Opposition at the national level
  Largest_Party = character(length(election_years)),     # Placeholder for the name of the largest party by seats
  Largest_Party_Seats = numeric(length(election_years)), # Placeholder for the seats held by the largest party
  Total_Seats = numeric(length(election_years)), # Placeholder for the total number of seats in the Lok Sabha
  Coalition = integer(length(election_years)),           # Binary dummy (1 for coalition, 0 for no coalition)
  Coalition_Name = character(length(election_years)),    # Placeholder for the name of the winning coalition (if any)
  Coalition_Leader = character(length(election_years))   # Placeholder for the name of party leading the winning coalition (if any)
)


#(ii). Adding Manually Obtained Data
# Updating the Turnout values for elections from 1962 to 2019 (Obtained from TCPD)
turnout_values <- c(61.16, 45.44, 55.42, 61.33, 55.29, 60.49, 56.92, 63.56, 61.95, 56.93, 57.94, 61.97, 59.99, 57.65, 58.19, 66.4, 66.79)
# Update the Turnout column in the national-level dataset
df_national$Turnout[1:17] <- turnout_values
df_national$Turnout <- df_national$Turnout/100
# List of largest parties by seats in the Indian general elections
largest_parties <- c("INC", "INC", "INC", "INC", "INC(R)", "JP", "INC", "INC", "INC", "INC", "BJP", "BJP", "BJP", "INC", "INC", "BJP", "BJP")
# List of coalition dummy values
coalition_values <- c(0, 0, 0, 0, 0, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0)
# List of seats won by the largest party
largest_party_seats <- c(364, 371, 361, 283, 352, 295, 353, 414, 197, 244, 161, 182, 182, 145, 206, 282, 303)
# List of total seats in the Lok Sabha
total_seats <- c(489, 494, 494, 520, 518, 542, 529, 541, 529, 534, 543, 543, 543, 543, 543, 543, 543)
# List of coalition names
coalition_names <- c("No Coalition", "No Coalition", "No Coalition", "No Coalition", "No Coalition", "Janata Alliance", "No Coalition", 
                     "No Coalition", "Janata Dal+", "Congress+", "Janata Dal+", "NDA", "NDA", "Congress+", "Congress+", "No Coalition", "No Coalition")
# List of coalition leader names
coalition_leaders <- c(NA, NA, NA, NA, NA, "JP", NA, NA, "JD", "INC", "JD", "BJP", "BJP", "INC", "INC", NA, NA)
# Updating the dataset with the provided information
df_national$Largest_Party <- largest_parties
df_national$Coalition <- coalition_values
df_national$Largest_Party_Seats <- largest_party_seats
df_national$Total_Seats <- total_seats
df_national$Coalition_Name <- coalition_names
df_national$Coalition_Leader <- coalition_leaders


#(iii). Calculations for SOP and Turnover Using Manually Obtained Data
#SOP:
df_national$SOP <- (df_national$Total_Seats - df_national$Largest_Party_Seats)/(df_national$Total_Seats)

#Turnover:
calc_turnover_national <- function(national_data) {
  national_data$Turnover <- rep(1, nrow(national_data))
  consec_reelections <- 0 
  for (i in 2:nrow(national_data)) {
    party_current <- ifelse(is.na(national_data$Coalition_Leader[i]), 
                            national_data$Largest_Party[i], 
                            national_data$Coalition_Leader[i])
    party_previous <- ifelse(is.na(national_data$Coalition_Leader[i-1]), 
                             national_data$Largest_Party[i-1], 
                             national_data$Coalition_Leader[i-1])
    if (party_current == party_previous) {
      consec_reelections <- consec_reelections + 1
      if (consec_reelections >= 2) {
        national_data$Turnover[i] <- 0
      }
    } else {
      consec_reelections <- 0 
    }
  }
  national_data$Turnover[1] <- NA
  return(national_data)
}
df_national <- calc_turnover_national(df_national)
df_national[c(1, 5), "Turnover"] <- c(1, 0) #Changing NA and INC(R) is the same as INC


#(iv). Adding New Data for 2024 Indian Elections (Source of Data: ECI and IndiaVotes.com)
new_row <- tibble::tibble(
  Assembly_No = 18,
  Year = 2024,
  Turnout = 0.66,
  SOP = 0.559,
  Largest_Party = "BJP",
  Largest_Party_Seats = 240,
  Total_Seats = 543,
  Coalition = 1,
  Coalition_Name = "NDA",
  Coalition_Leader = "BJP",
  Turnover = 0
)
df_national <- dplyr::bind_rows(df_national, new_row)


#(v). Manually Calculating National ENOPs from IndiaVotes Datasets
election_years <- c("1951", "1957", "1962", "1967", "1971", "1977", "1980", "1984", 
                    "1989", "1991", "1996", "1998", "1999", "2004", "2009", "2014", "2019", "2024")
# Initializing an empty vector to store the ENOP results for each year
enop_results <- data.frame(Year = character(), ENOP = numeric(), stringsAsFactors = FALSE)
# Defining a function to read in a file, clean the data, and calculate ENOP
calculate_enop <- function(file_name) {
  # Read the dataset with only the relevant columns
  election_data <- read.csv(file_name, stringsAsFactors = FALSE)
  # Step 2: Clean the Party_Votes column - remove commas and convert to numeric
  election_data$Party_Votes <- as.numeric(gsub(",", "", election_data$Party_Votes))
  # Step 3: Calculate total valid votes and Party_Vote_Share for each party
  total_valid_votes <- sum(election_data$Party_Votes, na.rm = TRUE)
  election_data$Party_Vote_Share <- election_data$Party_Votes / total_valid_votes
  # Step 4: Calculate ENOP using the Laakso and Taagepera formula
  enop_value <- 1 / sum(election_data$Party_Vote_Share^2, na.rm = TRUE)
  return(enop_value)
}
enop_1951 <- calculate_enop("1951_ENOP.csv")
enop_1957 <- calculate_enop("1957_ENOP.csv")
enop_1962 <- calculate_enop("1962_ENOP.csv")
enop_1967 <- calculate_enop("1967_ENOP.csv")
enop_1971 <- calculate_enop("1971_ENOP.csv")
enop_1977 <- calculate_enop("1977_ENOP.csv")
enop_1980 <- calculate_enop("1980_ENOP.csv")
enop_1984 <- calculate_enop("1984_ENOP.csv")
enop_1989 <- calculate_enop("1989_ENOP.csv")
enop_1991 <- calculate_enop("1991_ENOP.csv")
enop_1996 <- calculate_enop("1996_ENOP.csv")
enop_1998 <- calculate_enop("1998_ENOP.csv")
enop_1999 <- calculate_enop("1999_ENOP.csv")
enop_2004 <- calculate_enop("2004_ENOP.csv")
enop_2009 <- calculate_enop("2009_ENOP.csv")
enop_2014 <- calculate_enop("2014_ENOP.csv")
enop_2019 <- calculate_enop("2019_ENOP.csv")
enop_2024 <- calculate_enop("2024_ENOP.csv")
# Creating a dataframe to store the ENOP values
national_enop_results <- data.frame(
  Year = c("1951", "1957", "1962", "1967", "1971", "1977", "1980", "1984", 
           "1989", "1991", "1996", "1998", "1999", "2004", "2009", "2014", "2019", "2024"),
  ENOP = c(enop_1951, enop_1957, enop_1962, enop_1967, enop_1971, enop_1977, 
           enop_1980, enop_1984, enop_1989, enop_1991, enop_1996, enop_1998, 
           enop_1999, enop_2004, enop_2009, enop_2014, enop_2019, enop_2024),
  stringsAsFactors = FALSE
)
# Converting Year in national_enop_results to numeric to match df_national
national_enop_results$Year <- as.numeric(national_enop_results$Year)
# Joining the ENOP results with df_national based on the Year column
df_national <- df_national %>%
  left_join(national_enop_results, by = "Year")


#(vi). Calculating National Democracy Index
# Normalizing SOP, Turnout, ENOP
df_national <- df_national %>%
  mutate(
    SOP_Normalized = normalize(SOP),
    Turnout_Normalized = normalize(Turnout),
    ENOP_Normalized = normalize(ENOP)
  )
# Calculating Contestation and Turncoat Components
# Adding the new columns to df_national
df_national <- df_national %>%
  mutate(
    Contestation_Normalized = (ENOP_Normalized + SOP_Normalized) / 2
  )
# Unweighted Index
df_national <- df_national %>%
  mutate(
    DemIndex = Turnover + (Turnout_Normalized * Contestation_Normalized)
  )
# Weighted Index
df_national <- df_national %>%
  mutate(
    W_DemIndex = 0.3 * Turnover + 0.7 * (Turnout_Normalized * Contestation_Normalized)
  )


#(vii). Classifying National Backsliding
#Filtering the dataset from the earliest available year and removing NA values
national_backsliding_data <- df_national %>%
  filter(!is.na(W_DemIndex)) %>%
  select(Year, W_DemIndex) %>%
  arrange(Year)
#Calculating the change in democracy score between consecutive election years
national_backsliding_data <- national_backsliding_data %>%
  mutate(democracy_change = W_DemIndex - lag(W_DemIndex))
#Initializing the classification and cumulative gain columns
national_backsliding_data <- national_backsliding_data %>%
  mutate(classification = "Non-Backsliding",  # Default classification
         cumulative_gain = 0,                 # Initialize cumulative gain for recovery
         backsliding_start = NA_real_,        # Initialize backsliding start point
         cumulative_consolidation_gain = 0,   # Initialize cumulative gain for consolidation
         consolidating_flag = FALSE)          # Flag to track consolidation phase
#Looping through the data to classify backsliding, recovery, and consolidation
for (i in 2:nrow(national_backsliding_data)) {
  # Handle Backsliding Continuation
  if (national_backsliding_data$classification[i - 1] == "Backsliding") {
    # Update cumulative gain for recovery
    cumulative_gain <- national_backsliding_data$W_DemIndex[i] - national_backsliding_data$backsliding_start[i - 1]
    national_backsliding_data$cumulative_gain[i] <- cumulative_gain
    # Check for Recovery
    if (cumulative_gain >= 0.1) {
      national_backsliding_data$classification[i] <- "Recovery"
      national_backsliding_data$consolidating_flag[i] <- FALSE  # Reset consolidation flag after recovery
    } else {
      national_backsliding_data$classification[i] <- "Backsliding"
      national_backsliding_data$backsliding_start[i] <- national_backsliding_data$backsliding_start[i - 1]
    }
  }
  # Handle Transition from Recovery to Non-Backsliding
  else if (national_backsliding_data$classification[i - 1] == "Recovery") {
    national_backsliding_data$classification[i] <- "Non-Backsliding"
    national_backsliding_data$consolidating_flag[i] <- FALSE
  }
  # Handle New Backsliding Event
  if (national_backsliding_data$democracy_change[i] <= -0.1) {
    national_backsliding_data$classification[i] <- "Backsliding"
    national_backsliding_data$backsliding_start[i] <- national_backsliding_data$W_DemIndex[i]
    national_backsliding_data$consolidating_flag[i] <- FALSE  # Reset consolidation flag
  }
  # Handle Consolidation
  else if (national_backsliding_data$classification[i - 1] %in% c("Non-Backsliding", "Recovery", "Consolidating")) {
    # Check if already in consolidation
    if (national_backsliding_data$consolidating_flag[i - 1]) {
      if (national_backsliding_data$democracy_change[i] > 0) {  # Positive gain continues consolidation
        national_backsliding_data$classification[i] <- "Consolidating"
        national_backsliding_data$cumulative_consolidation_gain[i] <- national_backsliding_data$cumulative_consolidation_gain[i - 1] + national_backsliding_data$democracy_change[i]
      } else {
        national_backsliding_data$classification[i] <- "Non-Backsliding"  # Any decrease ends consolidation
        national_backsliding_data$consolidating_flag[i] <- FALSE
        national_backsliding_data$cumulative_consolidation_gain[i] <- 0  # Reset cumulative gain
      }
    }
    # Potential New Consolidation
    else if (national_backsliding_data$democracy_change[i] > 0) {
      national_backsliding_data$cumulative_consolidation_gain[i] <- national_backsliding_data$democracy_change[i]
      if (national_backsliding_data$cumulative_consolidation_gain[i] >= 0.1) {
        national_backsliding_data$classification[i] <- "Consolidating"
        national_backsliding_data$consolidating_flag[i] <- TRUE
      } else {
        national_backsliding_data$classification[i] <- "Non-Backsliding"
        national_backsliding_data$consolidating_flag[i] <- FALSE
      }
    } else {
      # No change or decrease
      national_backsliding_data$classification[i] <- "Non-Backsliding"
      national_backsliding_data$consolidating_flag[i] <- FALSE
      national_backsliding_data$cumulative_consolidation_gain[i] <- 0  # Reset cumulative gain
    }
  }
}
# Merging classification back into df_national
df_national <- df_national %>%
  left_join(national_backsliding_data %>% select(Year, classification), by = "Year")

########## 9. National Level Visualizations for Chapter 3 ####

# (i). Line Plot of W_DemIndex Over Time
ggplot(df_national, aes(x = Year, y = W_DemIndex)) +
  geom_line(color = "steelblue", size = 1.5) +
  geom_point(size = 2, color = "darkblue") +
  labs(
    x = "Year",
    y = "Weighted Democracy Index"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.text = element_text(size = 12),
    axis.title.x = element_text(face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(face = "bold", margin = margin(r = 15))
  )

#(ii). Step Plot for National-Level Classification Changes Over Time
df_national$classification_ordered <- factor(df_national$classification,
                                             levels = c("Backsliding", "Recovery", "Non-Backsliding", "Consolidating"),
                                             ordered = TRUE)
ggplot(df_national, aes(x = Year, y = as.numeric(classification_ordered))) +
  geom_step(color = "steelblue", size = 1.5) +
  labs(title = "",
       x = "Year",
       y = "Classification Level") +
  scale_y_continuous(breaks = 1:4, labels = c("Backsliding", "Recovery", "Non-Backsliding", "Consolidating")) +
  theme_minimal(base_size = 15) +
  theme(
    text = element_text(family = "Times New Roman"),
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text = element_text(size = 12),
    axis.title.x = element_text(face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(face = "bold", margin = margin(r = 15))
  )


#(iii). Smoother Line Plot with Confidence Interval for W_DemIndex (Optional)
ggplot(df_national, aes(x = Year, y = W_DemIndex)) +
  geom_smooth(method = "loess", span = 0.5, color = "darkred", fill = "lightcoral", size = 1.5) +
  geom_point(size = 2, color = "darkred") +
  labs(title = "Trend in National-Level Democracy Score with Confidence Interval",
       subtitle = "Loess Smoothing for Weighted Democracy Index",
       x = "Year",
       y = "Weighted Democracy Index") +
  theme_minimal(base_size = 15) +
  theme(
    text = element_text(family = "Garamond"),
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text = element_text(size = 12),
    axis.title.x = element_text(face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(face = "bold", margin = margin(r = 15))
  )


#(iv). Box Plot for Democracy Index Across Classifications
ggplot(df_national, aes(x = classification, y = W_DemIndex, fill = classification)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Accent") +
  labs(title = "Boxplot of Weighted Democracy Index by Classification",
       x = "Classification",
       y = "Weighted Democracy Index") +
  theme_minimal(base_size = 15) +
  theme(
    text = element_text(family = "Garamond"),
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text = element_text(size = 12),
    axis.title = element_text(face = "bold"),
    legend.position = "none"
  )


#(v). Density Plot of Democracy Scores (Optional)
ggplot(df_national, aes(x = W_DemIndex)) +
  geom_density(fill = "blue", alpha = 0.3) +
  labs(title = "Distribution of National Democracy Index Scores",
       x = "Democracy Index Score",
       y = "Density") +
  theme_minimal(base_size = 15) +
  theme(
    text = element_text(family = "Garamond"),
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text = element_text(size = 12),
    axis.title.x = element_text(face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(face = "bold", margin = margin(r = 15))
  )


#(vi). Heatmap of Yearly Democracy Scores (Optional)
ggplot(df_national, aes(x = factor(Year), y = "Democracy Score")) +
  geom_tile(aes(fill = W_DemIndex), color = "white") +
  scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Democracy Score") +
  labs(
    title = "Heatmap of Democracy Scores Over Time",
    subtitle = "Reflecting Changes in National Democracy Scores",
    x = "Year",
    y = "",
    fill = "Democracy Score"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    text = element_text(family = "Garamond"),
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_blank(),  # Remove unnecessary axis title
    panel.grid.major = element_blank(),  # Remove grid for a cleaner look
    legend.position = "bottom"
  )


########### 10. Subnational Level Visualizations for Chapter 3 ####

#(i). Visualizing the classification over time for each state
ggplot(backsliding_data, aes(x = Year, y = State_Name, color = classification)) +
  geom_point(size = 3) +
  labs(title = "",
       x = "Year",
       y = "States",
       color = "Classification") +
  theme_minimal(base_size = 15) +
  theme(
    text = element_text(family = "Times New Roman")
  )

#(ii). Visualizing the classification over time for each state since 2014
ggplot(recent_backsliding_data, aes(x = Year, y = State_Name, color = classification)) +
  geom_point(size = 3) +
  labs(title = "",
       x = "Year",
       y = "States",
       color = "Classification") +
  scale_x_continuous(breaks = seq(min(recent_backsliding_data$Year, na.rm = TRUE),
                                  max(recent_backsliding_data$Year, na.rm = TRUE),
                                  by = 1)) +
  theme_minimal(base_size = 15) +
  theme(
    text = element_text(family = "Times New Roman")
  )


#(iii). Creating a frequency table with state names for each classification and year
frequency_table_states <- recent_backsliding_data %>%
  group_by(Year, classification) %>%
  summarise(State_Names = paste(State_Name, collapse = ", ")) %>%
  pivot_wider(names_from = classification, values_from = State_Names) %>%
  arrange(Year)
# Replacing NA with empty strings for better readability
frequency_table_states[is.na(frequency_table_states)] <- ""
print(frequency_table_states)


#(iv). Calculating the percentage of states in each classification
percentage_classification <- recent_backsliding_data %>%
  group_by(classification) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = (Count / sum(Count)) * 100)
print(percentage_classification)


#(v). Creating transition table
recent_backsliding_data <- recent_backsliding_data %>%
  arrange(State_Name, Year) %>%
  group_by(State_Name) %>%
  mutate(Next_Classification = lead(classification))
transition_table <- recent_backsliding_data %>%
  filter(!is.na(Next_Classification)) %>%
  count(classification, Next_Classification) %>%
  spread(key = Next_Classification, value = n, fill = 0)
print(transition_table)


#(vi). Sankey Plot for Transitions
sankey_data <- recent_backsliding_data %>%
  arrange(State_Name, Year) %>%
  group_by(State_Name) %>%
  mutate(Next_Classification = lead(classification)) %>%
  filter(!is.na(Next_Classification)) %>%
  count(classification, Next_Classification)
# Creating nodes and links for the Sankey plot
nodes <- data.frame(name = unique(c(sankey_data$classification, sankey_data$Next_Classification)))
sankey_data$source <- match(sankey_data$classification, nodes$name) - 1
sankey_data$target <- match(sankey_data$Next_Classification, nodes$name) - 1
links <- sankey_data %>%
  rename(value = n) %>%
  dplyr::select(source, target, value)
# Adding a color scale
colourScale <- '
d3.scaleOrdinal()
  .domain(["Backsliding", "Recovery", "Consolidating", "Non-Backsliding"])
  .range(["#f8766d", "#c77cff", "#7cae00", "#00bfc4"])
'
# Creating Sankey Plot with Color Mapping
sankeyNetwork(Links = links, Nodes = nodes,
              Source = "source", Target = "target",
              Value = "value", NodeID = "name",
              units = "Transitions", fontSize = 14, nodeWidth = 30,
              colourScale = colourScale)


#(vii). Choropleth Map of State Democracy Classification (by Most Recent Election Year)
# Extracting the most recent classification for each state
recent_classifications <- recent_backsliding_data %>%
  group_by(State_Name) %>%
  filter(Year == max(Year)) %>%
  ungroup() %>%
  dplyr::select(State_Name, classification)
print(recent_classifications)
# Standardizing State Names in recent_classifications
recent_classifications$State_Name <- recent_classifications$State_Name %>%
  str_replace_all("[-_]", " ") %>%              # Replace underscores or dashes with spaces
  str_replace("Jammu & Kashmir", "Jammu and Kashmir") %>%  # Replace with consistent name
  str_trim() %>%                                # Remove leading/trailing whitespace
  str_to_title()                                # Convert to title case for consistent capitalization
# Loading the shapefile data for Indian states (using rnaturalearth)
india_states <- ne_states(country = "India", returnclass = "sf")
# Standardizing State Names in india_states (trim and convert to title case)
india_states$name <- india_states$name %>%
  str_trim() %>%
  str_to_title()
# Merging the spatial data with the recent classifications
map_data <- india_states %>%
  left_join(recent_classifications, by = c("name" = "State_Name"))
# Plotting the Choropleth Map
ggplot(data = map_data) +
  geom_sf(aes(fill = classification), color = "black", lwd = 0.2) +
  scale_fill_manual(values = c(
    "Non-Backsliding" = "#00bfc4",
    "Backsliding" = "#f8766d",
    "Consolidating" = "#7cae00",
    "Recovery" = "#c77cff"
  )) +
  labs(title = "",
       fill = "Classification") +
  theme_minimal(base_size = 15) +
  theme(
    text = element_text(family = "Times New Roman"),
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )


#(viii). Box Plot: Democracy Index by Classification
ggplot(recent_backsliding_data, aes(x = classification, y = W_DemIndex, fill = classification)) +
  geom_boxplot(alpha = 0.8, outlier.size = 1.5, outlier.shape = 21, color = "black") +
  scale_fill_manual(values = c(
    "Backsliding" = "#f8766d", 
    "Recovery" = "#c77cff", 
    "Consolidating" = "#7cae00", 
    "Non-Backsliding" = "#00bfc4"
  )) +
  labs(title = "",
       x = "Classification",
       y = "Weighted Democracy Index") +
  theme_minimal(base_size = 15) +
  theme(
    text = element_text(family = "Times New Roman"),
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.x = element_text(vjust = -1.5),
    axis.title.y = element_text(vjust = 2),
    legend.position = "none"  # Removes the legend
  )

summary_stats <- recent_backsliding_data %>%
  group_by(classification) %>%
  summarise(
    count = n(),
    min = min(W_DemIndex, na.rm = TRUE),
    Q1 = quantile(W_DemIndex, 0.25, na.rm = TRUE),
    median = median(W_DemIndex, na.rm = TRUE),
    Q3 = quantile(W_DemIndex, 0.75, na.rm = TRUE),
    max = max(W_DemIndex, na.rm = TRUE),
    mean = mean(W_DemIndex, na.rm = TRUE),
    sd = sd(W_DemIndex, na.rm = TRUE)
  )

#(ix). Lists of Classifications
# One: List of states with no backsliding and one or more instances of consolidation since 2014
no_backsliding_yes_consolidation <- recent_backsliding_data %>%
  group_by(State_Name) %>%
  filter(all(classification != "Backsliding") & any(classification == "Consolidating")) %>%
  distinct(State_Name) %>%
  pull(State_Name)
print("States with no backsliding and one or more instances of consolidation since 2014:")
print(no_backsliding_yes_consolidation)
# Two-A: List of states with no backsliding, no consolidation, and no recovery since 2014
no_backsliding_no_consolidation_no_recovery <- recent_backsliding_data %>%
  group_by(State_Name) %>%
  filter(all(classification != "Backsliding") & 
           all(classification != "Consolidating") & 
           all(classification != "Recovery")) %>%
  distinct(State_Name) %>%
  pull(State_Name)
print("States with no backsliding, no consolidation, and no recovery since 2014:")
print(no_backsliding_no_consolidation_no_recovery)
# Two-B: List of states with no backsliding, no consolidation, but have a classification of recovery since 2014
no_backsliding_no_consolidation_but_recovery <- recent_backsliding_data %>%
  group_by(State_Name) %>%
  filter(all(classification != "Backsliding") & 
           all(classification != "Consolidating") & 
           any(classification == "Recovery")) %>%
  distinct(State_Name) %>%
  pull(State_Name)
print("States with no backsliding, no consolidation, but with recovery since 2014:")
print(no_backsliding_no_consolidation_but_recovery)
# Three: List of states with backsliding but have recovered and are not currently backsliding
backsliding_then_recovered <- recent_backsliding_data %>%
  group_by(State_Name) %>%
  filter(any(classification == "Backsliding") & 
           last(classification) == "Recovery") %>%
  distinct(State_Name) %>%
  pull(State_Name)
print("States that experienced backsliding but have recovered and not backslid again since 2014:")
print(backsliding_then_recovered)
# Four: List of states with backsliding and still in backsliding as their last classification
still_backsliding <- recent_backsliding_data %>%
  group_by(State_Name) %>%
  filter(any(classification == "Backsliding") & 
           last(classification) == "Backsliding") %>%
  distinct(State_Name) %>%
  pull(State_Name)
print("States that experienced backsliding and are still backsliding as of the last observation:")
print(still_backsliding)


############ 11.Tamil Nadu Visualizations for Chapter 5 ####

# Filter for Tamil Nadu
tn_data <- df_state %>% filter(State_Name == "Tamil_Nadu")

# Reshape data for plotting multiple indicators (normalized ENOP used)
tn_long <- tn_data %>%
  select(Year, W_DemIndex, ENOP_Median_Normalized, SOP, State_Turnout, Turnover) %>%
  pivot_longer(cols = c(W_DemIndex, ENOP_Median_Normalized, SOP, State_Turnout),
               names_to = "Indicator", values_to = "Value")


# Define custom linetypes for secondary indicators
line_types <- c("ENOP_Median_Normalized" = "dashed",
                "SOP" = "dotted",
                "State_Turnout" = "twodash")

# Plot: Democratic Indicators in Tamil Nadu with W_DemIndex highlighted
ggplot() +
  annotate("rect", xmin = 2011, xmax = 2021, ymin = -Inf, ymax = Inf,
           fill = "grey85", alpha = 0.3) +
  geom_line(data = filter(tn_long, Indicator != "W_DemIndex"),
            aes(x = Year, y = Value, linetype = Indicator),
            color = "grey50", size = 0.7, alpha = 0.6) +
  geom_line(data = filter(tn_long, Indicator == "W_DemIndex"),
            aes(x = Year, y = Value, color = Indicator),
            size = 1.5) +
  geom_vline(data = tn_data %>% filter(Turnover == 1),
             aes(xintercept = Year),
             linetype = "dashed", color = "grey40") +
  scale_linetype_manual(
    values = line_types,
    labels = c("ENOP_Median_Normalized" = "ENOP (Normalized)",
               "SOP" = "Strength of Opposition",
               "State_Turnout" = "Turnout")
  ) +
  scale_color_manual(
    values = c("W_DemIndex" = "darkblue"),
    labels = c("W_DemIndex" = "Democracy Index")
  ) +
  theme_minimal(base_family = "Times") +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    legend.title = element_blank()
  ) +
  labs(
    title = NULL,
    subtitle = "2011–2021 Period Highlighted; Turnover Years Marked By Dashed Lines",
    x = "Election Year", y = "Score / %"
  )


# Plot for Largest_Party_Vote_Share and Seats_Won, focusing on DMK and ADMK (including "ADK")
tn_party_dominance <- tn_data %>%
  filter(Largest_Party %in% c("DMK", "ADMK", "ADK")) %>%
  mutate(Largest_Party = ifelse(Largest_Party == "ADK", "ADMK", Largest_Party))

ggplot(tn_party_dominance, aes(x = Year)) +
  geom_line(aes(y = Largest_Party_Vote_Share, color = Largest_Party), size = 1.2) +
  geom_point(aes(y = Largest_Party_Vote_Share, color = Largest_Party), size = 2) +
  geom_bar(aes(y = Seats_Won, fill = Largest_Party), stat = "identity",
           alpha = 0.4, position = "identity") +
  theme_minimal(base_family = "Times") +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    legend.title = element_blank()
  ) +
  labs(
    title = NULL,
    subtitle = "Vote Share (lines) and Seats Won (bars)",
    x = "Election Year", y = "Vote Share (%) / Seats Won"
  ) +
  scale_color_manual(values = c("DMK" = "darkred", "ADMK" = "navyblue")) +
  scale_fill_manual(values = c("DMK" = "darkred", "ADMK" = "navyblue"))






















