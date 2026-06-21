suppressPackageStartupMessages({
  library(tidytext)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(tibble)
  library(topicmodels)
  library(readr)
  library(ggplot2)
})

CORPUS_CSV     <- "Official_Doc_kor/corpus_kor.csv"

FINAL_K        <- 4
SEED           <- 1227
MIN_DOC_FREQ   <- 20
MIN_TOKENS_PAGE<- 30
MIN_TOKEN_LEN  <- 2


# Stopword
generic_sw <- c(
  "등","및","통해","위해","관련","대한","경우","가지","정도","부분","수준",
  "이상","이하","이내","대비","대상","측면","차원","기준","결과","내용","사항",
  "여부","과정","상황","문제","필요","가능","활용","제고","개선","강화","확대",
  "구축","마련","추진","지원","제공","수행","운영","관리","실시","적용","반영",
  "기반","중심","주요","다양","효과","효율","적극","우리","해당","현재","향후"
)

doc_sw <- c(
  "페이지","그림","표","장","절","항","목","요약","개요","서론","결론","배경",
  "참고","출처","주석","부록","목차","페이지수","페이지번호","페이지수준"
)

domain_sw <- c(
  "인공지능","지능","전략","정책","방안","계획","사업","국가","정부","산업"
)

ALL_STOPWORDS <- unique(c(generic_sw, doc_sw, domain_sw))


raw <- read_csv(CORPUS_CSV, show_col_types = FALSE) %>%
  rename_with(tolower) %>%
  mutate(row_id = as.character(doc_id))
cat(sprintf("입력 행(페이지) 수: %d / 전략(plan) 수: %d\n",
            nrow(raw), n_distinct(raw$plan)))


clean_tokens <- function(txt) {
  tk <- str_split(txt, "\\s+")[[1]]
  tk <- tk[nchar(tk) >= MIN_TOKEN_LEN]
  tk <- tk[!tk %in% ALL_STOPWORDS]
  tk <- tk[str_detect(tk, "^[가-힣]+$")]
  tk
}

all_pages <- list()
for (i in seq_len(nrow(raw))) {
  tokens <- clean_tokens(raw$proc_text[i])
  if (length(tokens) < MIN_TOKENS_PAGE) next
  all_pages[[length(all_pages) + 1]] <- list(
    page_id     = raw$row_id[i],
    plan        = raw$plan[i],
    page        = raw$page[i],
    token_count = length(tokens),
    tokens      = paste(tokens, collapse = " ")
  )
}
pages_df <- bind_rows(lapply(all_pages, as.data.frame, stringsAsFactors = FALSE)) %>%
  filter(nchar(tokens) > 5)

cat(sprintf("페이지 수: %d / 전략(plan): %d\n",
            nrow(pages_df), n_distinct(pages_df$plan)))


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
write_csv(top_terms, "LDA_Topic_Words_kor.csv")


p_topics <- top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") + scale_y_reordered() +
  labs(title = sprintf("Government Official Documents LDA Topics (k=%d)", FINAL_K), x = "beta", y = NULL) +
  theme_minimal(base_family = "AppleGothic")
ggsave("LDA_Topic_Words_kor.png", p_topics, width = 11, height = 7, dpi = 150)


gamma_td <- tidy(lda_model, matrix = "gamma") %>% rename(page_id = document)
page_topics <- gamma_td %>%
  pivot_wider(names_from = topic, values_from = gamma, names_prefix = "topic_") %>%
  left_join(pages_df %>% select(page_id, plan, page, token_count),
            by = "page_id")
page_topics$dominant_topic <-
  max.col(as.matrix(page_topics[, paste0("topic_", 1:FINAL_K)]))
write_csv(page_topics, "LDA_Topics_kor.csv")


strat_topics <- page_topics %>%
  group_by(plan) %>%
  summarise(across(starts_with("topic_"), ~ weighted.mean(.x, token_count)),
            n_pages = n(), .groups = "drop")
write_csv(strat_topics, "Topic_Strategy_kor.csv")
