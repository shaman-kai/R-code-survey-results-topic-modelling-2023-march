# FUNCTION TO CREATE NEW DATAFRAMES WITH RESPONSES THAT CONTAIN KEYWORDS FROM A SPECIFIC TOPIC/DICTIONARY
topic_df <- function(topic, data) {
  data$x <- grepl(topic, data$text)
  output <- data[data$x == TRUE,][,1]
  return(output)}

# FUNCTION TO EXTRACT KEYWORDS-IN-CONTEXT (PIECES OF TEXT CONTAINING A KEYWORD FROM A TOPIC + 15 WORDS AROUND IT) FROM EACH DATAFRAME AND TURN THOSE PIECES OF TEXT INTO A DTM(DOCUMENT-TERM MATRIX) FOR EACH TOPIC
topic_dtm <- function(dict, data) {
  corp_raw <- corpus(data, text_field = 'text')
  kwic_con  <- kwic(tokens(corp_raw), dict, window = 15)
  df_con <- data.frame(matrix(ncol = 1, nrow = nrow(kwic_con), dimnames=list(NULL, c('text'))))
  corp_con <- corpus(df_con$text <- paste(kwic_con$pre, kwic_con$keyword, kwic_con$post, sep=" "))
  dtm <- corp_con |>
    tokens(remove_punct=T, remove_symbols=T) |> tokens_tolower() |> tokens_remove(stopwords('en')) |> tokens_remove(stopwords_askar) |> dfm()
  return(dtm)}
