# This scripts reads and tidies the electoral data for 1962
# First, we have to load the necessary packages
stopifnot(requireNamespace("readr", quietly = TRUE))
stopifnot(requireNamespace("stringr", quietly = TRUE))
stopifnot(requireNamespace("dplyr", quietly = TRUE))
library(dplyr)

# Load the raw data for the 1962 election
elections_1962 <- readr::read_csv("data-raw/raw_datasets/elecciones_1962_raw.csv",
                                  col_names = FALSE)
#View(elections_1962)

# Assign the correct names to the columns. Which as of now represent the names
# of the parties that participated in these elections
names(elections_1962) <- elections_1962[1:4, ] %>%
  lapply(paste, collapse = " ") %>%
  stringr::str_replace_all(pattern = "(NA NA|^NA )", replacement = "") %>%
  stringr::str_trim() %>%
  unique() %>%
  rep(each = 2) %>%
  tail(-1) %>%
  paste(elections_1962[5, ], sep = "_")

names(elections_1962)[1] <- "region"



# The first five rows of the dataset used the be the column names
# We do not need them anymore. Thus eliminate them
elections_1962 <- tail(elections_1962, n = -5)

# Save the elections_1962 data frame so that it can be seen in excel
readr::write_csv(elections_1962,
                 path = "data-raw/clean_datasets/elecciones_1962_clean.csv")
