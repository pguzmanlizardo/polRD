# This scripts reads and tidies the electoral data for 1962
# First, we have to load the necessary packages
rm(list = ls())
stopifnot(requireNamespace("readr", quietly = TRUE))
stopifnot(requireNamespace("stringr", quietly = TRUE))
stopifnot(requireNamespace("dplyr", quietly = TRUE))
stopifnot(requireNamespace("tidyr", quietly = TRUE))

# I want to use the pipe from dplyr in a comfortable manner
`%>%` <- dplyr::`%>%`


# Load the raw data for the 1962 election
elections_1962 <- readr::read_csv("data-raw/raw_datasets/elecciones_1962_raw.csv",
                                  col_names = FALSE)


# COLUMNS: Partidos -----------------------------------------------------------

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

# We do not need the TOTALES columns since they have redundant information
elections_1962 <- elections_1962 %>%
  tidyr::pivot_longer(cols = -region,
                      names_to = "partido",
                      values_to = "votos") %>%
  dplyr::filter(stringr::str_detect(string = partido,
                                    pattern = "TOTALES",
                                    negate = TRUE)) %>%
  tidyr::pivot_wider(names_from = partido,
                     values_from = votos)


# Now I want to separate the `otras` from the `nacionales` columns convert
# them into an extra column
elections_1962 <- elections_1962 %>%
  tidyr::pivot_longer(cols = -region,
                      names_to = "partido",
                      values_to = "votos") %>%
  tidyr::separate(col = partido,
                  into = c("partido", "nivel"),
                  sep = "_") %>%
  tidyr::pivot_wider(names_from = partido,
                     values_from = votos)


# ROWS: regiones --------------------------------------------------------------
# Eliminate the TOTAL and NA rows at the end of the dataset
elections_1962 <- elections_1962 %>%
  dplyr::filter(!(is.na(region) | region == "TOTAL"))

# Create a function that takes row totals (as in province or department totals)
# And creates a new column with the group to which the observation belongs
disgroup <- function(data, name, patron, where){
  # data is the dataset you are working with
  # name is the name of the new column you want to create
  # patron is the pattern that you want to identify in where
  # where is the variable where you will get the information

  stopifnot(is.data.frame(data))
  stopifnot(is.character(name))
  stopifnot(length(name) == 1L)
  stopifnot(is.character(patron))
  stopifnot(length(patron) == 1L)
  stopifnot(is.character(where))
  stopifnot(length(where) == 1L)
  stopifnot(requireNamespace("stringr", quietly = TRUE))
  stopifnot(requireNamespace("tidyr", quietly = TRUE))
  stopifnot(requireNamespace("dplyr", quietly = TRUE))

  data[[name]] <-
    stringr::str_detect(data[[where]],
                        pattern = paste0(patron, "|", "DISTRITO")) *
    (data$nivel == "NACIONAL")

  data[[name]] <- ifelse(data[[name]], data[[where]], NA)

  data <- tidyr::fill(data = data, all_of(name))

  data <- data %>%
    dplyr::filter(!stringr::str_detect(data[[where]], pattern = patron))

  return(data)
}

# Use the disgroup function (which I just created) to create a provincia
# and departamento variable, while at the same time eliminating all the
# departamento and provincia observations

elections_1962 <- disgroup(data = elections_1962, name = "departamento",
                           patron = "DEPARTAMENTO", where = "region")
elections_1962 <- disgroup(data = elections_1962, name = "provincia",
                           patron = "PROVINCIA", where = "region")

# Cambiar la variable region, por la variable municipio
names(elections_1962)[1] <- "municipio"


# TIDY: everything ------------------------------------------------------------
# Actualmente, los partidos son columnas. Para hacerlo tidy, prefiero
# prefiero convertirlos en una variable mas
elections_1962 <- elections_1962 %>%
  dplyr::select(departamento, provincia, municipio, nivel, everything()) %>%
  tidyr::pivot_longer(-departamento:-nivel,
                      names_to = "partido",
                      values_to = "votos")

# Las observaciones en la columna `DEPARTAMENTO` y `PROVINCIA` todas empiezan
# con la palabra "DEPARTAMENTO DE" o "PROVINCIA DE". Es claramente redundante,
# asi que voy a eliminar esa primera expresion
elections_1962 <- elections_1962 %>%
  dplyr::mutate(
    departamento = stringr::str_remove(departamento,
                                       pattern = "DEPARTAMENTO DE "),
    provincia = stringr::str_remove(provincia, pattern = "PROVINCIA DE "))

# Add a variable `year` that says 1962
elections_1962$`año` <- "1962"
elections_1962$vuelta <- NA
elections_1962$coalicion <- NA
elections_1962$partido_lider <- NA
elections_1962$siglas <- NA

# Select the dataset in the right order
elections_1962 <- elections_1962 %>%
  dplyr::select(`año`, departamento, provincia, municipio, nivel, vuelta,
                coalicion, partido_lider, partido, siglas)


# SAVE ------------------------------------------------------------------------
# Save the elections_1962 data frame so that it can be seen in excel
readr::write_csv(elections_1962,
                 path = "data-raw/clean_datasets/elecciones_1962_clean.csv")

