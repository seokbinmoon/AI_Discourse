suppressPackageStartupMessages({
  library(pdftools)
  library(tidytext)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(SnowballC)
  library(stopwords)
  library(tibble)
  library(topicmodels)
  library(readr)
  library(ggplot2)
})

PDF_DIR        <- "Official_Doc_us"

FINAL_K        <- 5
SEED           <- 1227
MIN_DOC_FREQ   <- 20
MIN_WORDS_PAGE <- 100
MIN_TOKENS_PAGE<- 60
MIN_TOKEN_LEN  <- 3


parse_filename <- function(fname) {
  base <- tools::file_path_sans_ext(fname)
  list(country = "us", doc = base)
}

preprocess_text <- function(text) {
  text %>%
    str_replace_all("([a-zA-Z])-\\s+([a-zA-Z])", "\\1\\2") %>%
    str_to_lower() %>%
    str_replace_all("[^a-z\\s]", " ") %>%
    str_replace_all("\\s+", " ") %>%
    str_trim()
}

base_sw <- union(
  stopwords::stopwords("en", source = "snowball"),
  stopwords::stopwords("en", source = "stopwords-iso")
)

country_sw <- c(
  "australia","australian","austria","austrian","belgium","belgian",
  "benin","beninese","bulgaria","bulgarian","cambodia","cambodian",
  "chile","chilean","china","chinese","costa","rica","rican",
  "czech","czechia","bohemia","bohemian","denmark","danish","dane",
  "dominican","dominicana","hispaniola","egypt","egyptian",
  "estonia","estonian","finland","finnish","finn","france","french",
  "germany","german","ghana","ghanaian","greece","greek",
  "hungary","hungarian","indonesia","indonesian","ireland","irish",
  "israel","israeli","italy","italian","japan","japanese",
  "kenya","kenyan","korea","korean","lesotho","basotho",
  "lithuania","lithuanian","luxembourg","luxembourgish","luxemburg",
  "malta","maltese","mauritania","mauritanian","zealand",
  "nigeria","nigerian","norway","norwegian","peru","peruvian",
  "philippines","philippine","filipino","pilipinas","poland","polish",
  "portugal","portuguese","romania","romanian","rwanda","rwandan",
  "saudi","arabia","arabian","singapore","singaporean",
  "slovenia","slovenian","spain","spanish","switzerland","swiss",
  "thailand","thai","turkey","turkish",
  "uae","emirati","emirate","emirates",
  "britain","british","uk","england","english","wales","welsh",
  "scotland","scottish","america","american","usa",
  "uzbekistan","uzbek","vietnam","vietnamese","viet","zambia","zambian",
  "russia","russian","soviet","india","indian","brazil","brazilian",
  "canada","canadian","mexico","mexican","netherlands","dutch",
  "sweden","swedish","ukraine","ukrainian","iran","iranian","persia","persian",
  "pakistan","pakistani","bangladesh","bangladeshi","colombia","colombian",
  "malaysia","malaysian","myanmar","burmese","laos","lao","laotian",
  "mongolia","mongolian","nepal","nepali","sri","lanka","lankan",
  "iraq","iraqi","syria","syrian","jordan","jordanian","morocco","moroccan",
  "tunisia","tunisian","algeria","algerian","sudan","sudanese",
  "senegal","senegalese","tanzania","tanzanian","uganda","ugandan",
  "angola","angolan",
  "africa","african","europe","european","asia","asian","oceania","oceanian",
  "pacific","atlantic","mediterranean","arctic","caribbean",
  "nordic","scandinavian","balkan","balkans","mideast",
  "eastern","western","northern","southern","central",
  "southeast","southwestern","northeastern","northwestern",
  "oecd","asean","g7","g20","nato","wto","unesco","unicef","undp","unido",
  "unctad","imf","itu","iaea","wef","apec","adb","afdb","idb",
  "kingdom","republic","federation","union","nation","nations","states",
  "government","ministry","minister","ministerial","parliament",
  "parliamentary","legislation","legislative","president","presidential","prime"
)

doc_sw <- c(
  "page","figure","fig","table","tab","box","chart","graph",
  "appendix","annex","chapter","section","paragraph","clause",
  "introduction","conclusion","summary","overview","foreword","preface",
  "content","contents","index","reference","bibliography","citation",
  "footnote","endnote","source","adapted","copyright","note","notes",
  "ibid","idem","op","cit","http","https","www","pdf","html","htm",
  "com","org","gov","document","report","publication","press","release"
)

policy_sw <- c(
  "shall","hereby","herein","hereto","therein","thereby","whereas",
  "furthermore","therefore","moreover","nevertheless","per","cent","percent",
  "billion","million","thousand",
  "january","february","march","april","may","june","july","august",
  "september","october","november","december",
  "jan","feb","mar","apr","jun","jul","aug","sep","oct","nov","dec",
  "day","week","month","quarter","annual","yearly"
)

proper_noun_sw <- c(
  "mckinsey","deloitte","accenture","capgemini","pwc","bcg","kpmg","bain","booz",
  "gartner","forrester","idc","frost","google","microsoft","amazon","alibaba",
  "baidu","tencent","facebook","meta","apple","samsung","huawei","cisco",
  "oracle","intel","nvidia","openai","chatgpt","deepmind","anthropic","ibm",
  "salesforc","sap","ericsson","qualcomm","mptc","dti","sfi","nai","ict"
)

artifact_sw <- c("eur","eu","aiisrael")

domain_sw <- c("artificial","intelligence","strategy","strategic","strategies")

generic_verb_sw <- c(
  "develop","implement","ensure","promote","include","establish","achieve",
  "support","provide","create","increase","improve","build","enable","aim",
  "set","share","focus"
)

stem_artifacts <- c(
  "ing","ent","tion","ment","ation","ort","ector","nolog","olici","tec",
  "earc","tit","ism","ist","ali","aci","oci","eri","ori","ifi","ari","ivi",
  "ati","ioni","olog","ble","ful","nes","iti","ize","ise","ate","est","har",
  "edg","lab","ect","str","tur","duc","struct"
)

ALL_STOPWORDS <- unique(c(base_sw, country_sw, doc_sw, policy_sw,
                          proper_noun_sw, artifact_sw, domain_sw, generic_verb_sw))
STEMMED_SW    <- unique(c(ALL_STOPWORDS,
                          wordStem(ALL_STOPWORDS, language = "english"),
                          stem_artifacts))
ARTIFACT_REGEX <- paste0("^(",
  "ing|ent|tion|ment|ation|ort|ector|nolog|olici|tec|earc|tit|ism|ist|ble|",
  "ful|nes|iti|ize|ise|ate|ifi|ari|ivi|ati|elo|est|har|edg|lab|ect|str|tur|",
  "duc|ata|cation|anc|tran|entat|sion|ance|ence|enci|anci|eous|ious|less|able|ible",
  ")$")

pdf_files <- list.files(PDF_DIR, pattern = "\\.pdf$", full.names = TRUE)
cat(sprintf("PDF 파일 수: %d\n", length(pdf_files)))

all_pages <- list()
for (i in seq_along(pdf_files)) {
  fpath <- pdf_files[i]; fname <- basename(fpath)
  meta  <- parse_filename(fname)
  raw_pages <- tryCatch(pdf_text(fpath),
    error = function(e) { cat("  [오류]", conditionMessage(e), "\n"); character(0) })

  for (pg_num in seq_along(raw_pages)) {
    raw_text   <- raw_pages[pg_num]
    word_count <- length(strsplit(raw_text, "\\s+")[[1]])
    if (word_count < MIN_WORDS_PAGE) next

    tokens_df <- tibble(text = preprocess_text(raw_text)) %>%
      unnest_tokens(word, text, token = "words") %>%
      filter(!word %in% ALL_STOPWORDS, nchar(word) >= MIN_TOKEN_LEN,
             !str_detect(word, "^[0-9]+$")) %>%
      mutate(word = wordStem(word, language = "english")) %>%
      filter(!word %in% STEMMED_SW, nchar(word) >= MIN_TOKEN_LEN,
             str_detect(word, "^[a-z]+$"), !str_detect(word, ARTIFACT_REGEX))

    if (nrow(tokens_df) < MIN_TOKENS_PAGE) next

    all_pages[[length(all_pages) + 1]] <- list(
      doc_id      = tools::file_path_sans_ext(fname),
      country     = meta$country, page_num = pg_num,
      token_count = nrow(tokens_df),
      tokens      = paste(tokens_df$word, collapse = " ")
    )
  }
}
pages_df <- bind_rows(lapply(all_pages, as.data.frame, stringsAsFactors = FALSE)) %>%
  filter(nchar(tokens) > 10) %>%
  mutate(page_id = paste(doc_id, page_num, sep = "_p"))

cat(sprintf("페이지 수: %d / 전략(국가x연도): %d\n",
            nrow(pages_df), n_distinct(pages_df$doc_id)))



tok <- pages_df %>%
  select(page_id, tokens) %>%
  separate_rows(tokens, sep = " ") %>%
  filter(tokens != "") %>%
  rename(word = tokens) %>%
  count(page_id, word, name = "n")

doc_freq   <- tok %>% distinct(page_id, word) %>% count(word, name = "df")
keep_words <- doc_freq %>% filter(df > MIN_DOC_FREQ) %>% pull(word)
tok <- tok %>% filter(word %in% keep_words) %>%
  group_by(page_id) %>% filter(sum(n) > 0) %>% ungroup()
dtm <- tok %>% cast_dtm(document = page_id, term = word, value = n)

cat(sprintf("DTM — 문서(페이지): %d / 어휘: %d\n", nrow(dtm), ncol(dtm)))


lda_model <- LDA(dtm, k = FINAL_K, method = "Gibbs",
                 control = list(delta = 0.01, seed = SEED, burnin = 1000, iter = 2000, thin = 100))


beta_td   <- tidy(lda_model, matrix = "beta")
top_terms <- beta_td %>% group_by(topic) %>%
  slice_max(beta, n = 10, with_ties = FALSE) %>%
  arrange(topic, desc(beta)) %>% ungroup()
for (k in 1:FINAL_K) {
  cat(sprintf("=== Topic %d ===\n", k))
  print(top_terms %>% filter(topic == k) %>%
          transmute(rank = row_number(), term, beta = round(beta, 4)))
  cat("\n")
}
write_csv(top_terms, "LDA_Topic_Words_us.csv")


p_topics <- top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") + scale_y_reordered() +
  labs(title = sprintf("US AI strategies LDA topics (k=%d)", FINAL_K), x = "beta", y = NULL) +
  theme_minimal()
ggsave("LDA_Topic_Words_us.png", p_topics, width = 11, height = 7, dpi = 150)


gamma_td <- tidy(lda_model, matrix = "gamma") %>% rename(page_id = document)
page_topics <- gamma_td %>%
  pivot_wider(names_from = topic, values_from = gamma, names_prefix = "topic_") %>%
  left_join(pages_df %>% select(page_id, doc_id, country, page_num, token_count),
            by = "page_id")
page_topics$dominant_topic <-
  max.col(as.matrix(page_topics[, paste0("topic_", 1:FINAL_K)]))
write_csv(page_topics, "LDA_Topics_us.csv")


strat_topics <- page_topics %>%
  group_by(doc_id, country) %>%
  summarise(across(starts_with("topic_"), ~ weighted.mean(.x, token_count)),
            n_pages = n(), .groups = "drop")
write_csv(strat_topics, "Topic_Strategy_us.csv")
