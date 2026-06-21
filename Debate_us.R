suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(stringr)
  library(tidyr)
  library(tidytext)
  library(topicmodels)
  library(ggplot2)
  library(udpipe) 
  library(SnowballC)
})

# Settings
K        <- c("Government" = 4, "Non-Government" = 5)
GRP_EN   <- c("Government" = "Government", "Non-Government" = "Non-Government")
MIN_TOK  <- 5        # Minimum tokens per pseudo-doc
MIN_DF   <- 4        # Minimum doc appearances
TOP_N    <- 10
SEED     <- 1227

# Stopwords
stop_spoken <- c(
  "thing","things","stuff","way","ways","lot","lots","bit","kind","sort",
  "guy","guys","people","person","everybody","everyone","someone","somebody",
  "today","time","year","years","day","days","moment","point","points",
  "okay","yeah","yes","ok","sure","maybe","right","well","actually","really",
  "thank","thanks","sorry","hey","hi","hello"
)

stop_meta <- c(
  "senator","senators","chairman","chairwoman","chair","committee","subcommittee",
  "witness","witnesses","panel","panelist","hearing","testimony","statement",
  "question","questions","answer","answers","comment","comments","remark","remarks",
  "moderator","host","gentleman","gentlemen","colleague","colleagues",
  "minute","minutes","second","page","floor","applause","welcome","introduction",
  "session","debate","discussion","audience","mr","mrs","ms","dr"
)

stop_generic <- c(
  "issue","issues","part","parts","example","examples","case","cases","fact","facts",
  "number","numbers","sense","place","places","area","areas","side","sides",
  "matter","matters","reason","reasons","result","results","level","levels",
  "type","types","form","forms","feature","features","aspect","aspects",
  "term","terms","piece","pieces","set","sets","line","lines","end","start",
  "today","tomorrow","yesterday","week","month","percent","kind"
)

stop_theme <- c(
  "ai","intelligence","artificial","agi","asi"
)

stop_names <- c(
  "ryan","sean","adams","mo","gawdat","steven","kotler","peter","diamandis",
  "daniel","miessler","stephen","dunbar","johnson","meredith","levien",
  "ted","budd","josh","hawley","google","openai"
)

stopwords_en <- unique(c(
  tolower(stop_words$word), stop_spoken, stop_meta,
  stop_generic, stop_theme, stop_names
))


stem_artifacts <- c(
  "ing","ent","tion","ment","ation","ort","ector","nolog","olici","tec",
  "earc","tit","ism","ist","ali","aci","oci","eri","ori","ifi","ari","ivi",
  "ati","ioni","olog","ble","ful","nes","iti","ize","ise","ate","est","har",
  "edg","lab","ect","str","tur","duc","struct"
)
ARTIFACT_REGEX <- paste0("^(",
  "ing|ent|tion|ment|ation|ort|ector|nolog|olici|tec|earc|tit|ism|ist|ble|",
  "ful|nes|iti|ize|ise|ate|ifi|ari|ivi|ati|elo|est|har|edg|lab|ect|str|tur|",
  "duc|ata|cation|anc|tran|entat|sion|ance|ence|enci|anci|eous|ious|less|able|ible",
  ")$")
stopwords_en_stem <- unique(c(
  stopwords_en, wordStem(stopwords_en, language = "english"), stem_artifacts
))

# Data Loading
DATA_DIR <- "Debate_us"
files <- list.files(DATA_DIR, pattern = "\\.xlsx$", full.names = TRUE)
raw <- lapply(files, function(f)
  read_excel(f) %>% mutate(source_file = basename(f), ord = row_number())) %>%
  bind_rows()

# Gov. vs. Non-Gov. labeling
# Hong & Davison 2010: pseudo-document
dat <- raw %>%
  filter(!is.na(Content), Category != "Host") %>%
  mutate(group = if_else(Category == "Government", "Government", "Non-Government")) %>%
  arrange(source_file, ord) %>%
  group_by(source_file) %>%
  mutate(turn = cumsum(Speaker != lag(Speaker, default = first(Speaker)) |
                       group   != lag(group,   default = first(group)))) %>%
  ungroup() %>%
  group_by(source_file, turn) %>%
  summarise(group   = first(group),
            Speaker = first(Speaker),
            Content = paste(Content, collapse = " "),
            .groups = "drop") %>%
  mutate(doc_id = paste0("d", row_number()))

# udpipe English model (download once, then load from local file)
ud_file <- list.files(pattern = "^english-ewt.*\\.udpipe$")
if (length(ud_file) == 0) {
  ud_file <- udpipe_download_model(language = "english-ewt")$file_model
}
ud_model <- udpipe_load_model(ud_file)

# Extract nouns (udpipe: NOUN / PROPN) + Porter stemming
anno <- udpipe_annotate(ud_model, x = dat$Content, doc_id = dat$doc_id) %>%
  as.data.frame()

tokens <- anno %>%
  filter(upos %in% c("NOUN", "PROPN")) %>%
  transmute(doc_id, word = str_to_lower(token)) %>%
  filter(str_detect(word, "^[a-z]+$"),
         nchar(word) >= 3,
         !word %in% stopwords_en) %>%
  mutate(noun = wordStem(word, language = "english")) %>%       # stemming
  filter(!noun %in% stopwords_en_stem, nchar(noun) >= 3,
         str_detect(noun, "^[a-z]+$"), !str_detect(noun, ARTIFACT_REGEX)) %>%
  select(doc_id, noun) %>%
  left_join(dat %>% select(doc_id, group), by = "doc_id")

# Filtering
keep_docs  <- tokens %>% count(doc_id, name = "n") %>% filter(n >= MIN_TOK) %>% pull(doc_id)
tokens     <- tokens %>% filter(doc_id %in% keep_docs)
keep_terms <- tokens %>% distinct(doc_id, noun) %>% count(noun, name = "df") %>%
  filter(df >= MIN_DF) %>% pull(noun)
tokens     <- tokens %>% filter(noun %in% keep_terms)

build_dtm <- function(grp) {
  tokens %>% filter(group == grp) %>%
    count(doc_id, noun, name = "count") %>%
    cast_dtm(doc_id, noun, count)
}

# LDA
run_lda <- function(grp) {
  k   <- K[[grp]]
  dtm <- build_dtm(grp)
  lda <- LDA(dtm, k = k, method = "Gibbs",
             control = list(alpha = 0.1, delta = 0.01, seed = SEED, burnin = 1000, iter = 2000, thin = 100))
  top_terms <- tidy(lda, matrix = "beta") %>%
    group_by(topic) %>% slice_max(beta, n = TOP_N, with_ties = FALSE) %>%
    arrange(topic, desc(beta)) %>% ungroup()
  write.csv(top_terms, sprintf("lda_topterms_%s_us.csv", grp),
            row.names = FALSE, fileEncoding = "UTF-8")
  p <- top_terms %>%
    mutate(term = reorder_within(term, beta, topic)) %>%
    ggplot(aes(beta, term, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free_y") + scale_y_reordered() +
    labs(title = sprintf("%s Discourse LDA Topics (k=%d)", GRP_EN[[grp]], k), x = "beta", y = NULL) +
    theme_minimal()
  ggsave(sprintf("lda_topterms_%s_us.png", grp), p, width = 11, height = 7, dpi = 150)
  top_terms
}
tt_gov    <- run_lda("Government")
tt_nongov <- run_lda("Non-Government")

print_topics <- function(tt, grp) {
  for (tp in sort(unique(tt$topic)))
    cat(sprintf("  Topic %d: %s\n", tp,
                paste(tt %>% filter(topic == tp) %>% pull(term), collapse = ", ")))
}
print_topics(tt_gov,    "Government")
print_topics(tt_nongov, "Non-Government")


# Weighted log-odds
weighted_log_odds <- function(df) {
  V <- n_distinct(df$word); a_w <- 1; a0 <- V * a_w
  wtot  <- df %>% group_by(word)  %>% summarise(wt = sum(n), .groups = "drop")
  gtot  <- df %>% group_by(group) %>% summarise(gt = sum(n), .groups = "drop")
  grand <- sum(df$n)
  df %>%
    left_join(wtot, by = "word") %>% left_join(gtot, by = "group") %>%
    mutate(n_jw = wt - n, n_j = grand - gt,
           l_i  = log((n    + a_w) / (gt  + a0 - n    - a_w)),
           l_j  = log((n_jw + a_w) / (n_j + a0 - n_jw - a_w)),
           log_odds = l_i - l_j,
           z = log_odds / sqrt(1/(n + a_w) + 1/(n_jw + a_w))) %>%
    select(group, word, n, log_odds, z)
}

lo_debate <- tokens %>% count(group, noun, name = "n") %>% rename(word = noun) %>%
  weighted_log_odds()
write.csv(lo_debate, "logodds_gov_vs_nongov_us.csv",
          row.names = FALSE, fileEncoding = "UTF-8")

cat("\n[Log-Odds] Government distinctive words Top 10 (z):\n")
print(lo_debate %>% filter(group == "Government")     %>% slice_max(z, n = 10) %>%
        transmute(word, z = round(z, 2), n))
cat("\n[Log-Odds] Non-Government distinctive words Top 10 (z):\n")
print(lo_debate %>% filter(group == "Non-Government") %>% slice_max(z, n = 10) %>%
        transmute(word, z = round(z, 2), n))

LO_TOP <- 10
lo_axis <- lo_debate %>% filter(group == "Non-Government")
lo_plot_df <- bind_rows(
  lo_axis %>% slice_max(z, n = LO_TOP, with_ties = FALSE),
  lo_axis %>% slice_min(z, n = LO_TOP, with_ties = FALSE) 
) %>%
  mutate(side = if_else(z > 0, "Non-Government", "Government"),
         word = reorder(word, z))
p_lo <- ggplot(lo_plot_df, aes(z, word, fill = side)) +
  geom_col(show.legend = TRUE) +
  geom_vline(xintercept = 0, linewidth = 0.3, color = "grey40") +
  scale_fill_manual(values = c("Government" = "#C0392B", "Non-Government" = "#2C6FBB")) +
  labs(title = "Government vs Non-Government Distinctive Words (Weighted Log-Odds)",
       subtitle = sprintf("Top %d by z per group · z>0 Non-Government / z<0 Government", LO_TOP),
       x = "weighted log-odds z", y = NULL, fill = "Group") +
  theme_minimal()
ggsave("logodds_gov_vs_nongov_us.png", p_lo, width = 9, height = 7, dpi = 150)
