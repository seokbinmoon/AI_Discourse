suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(stringr)
  library(tidyr)
  library(tidytext)
  library(topicmodels)
  library(ggplot2)
  library(reticulate)
})

# Settings
K        <- c("정부" = 4, "비정부" = 5)
GRP_EN   <- c("정부" = "Government", "비정부" = "Non-Government")
MIN_TOK  <- 5
MIN_DF   <- 4
TOP_N    <- 10
SEED     <- 1227

use_python("/usr/bin/python3", required = TRUE)
kiwi <- import("kiwipiepy")$Kiwi()

# Stopwords
stop_spoken <- c(
  "생각","말씀","얘기","이야기","말","사실","정말","진짜","약간","조금",
  "그냥","무슨","무엇","누구","이거","그거","저거","이런","그런","저런",
  "여기","거기","저기","이때","요즘","처음","마지막","오늘","지금","이제",
  "이번","다음","우리","저희","여러분","사람","나라","모두","다들","자신",
  "정도","경우","자체","상황","내용","결국","고민","느낌","생각들"
)

stop_meta <- c(
  "위원","위원장","위원회","의원","교수","변호사","원장","진술","진술인",
  "사회자","패널","토론","토론회","질문","답변","발언","의견","논의","공청회",
  "페이지","작년","시간","질의","순서","박수","소개","말씀드","감사","안녕",
  "진행","간사","이견", "이후", "고려"
)

stop_generic <- c(
  "중요","필요","가능","부분","입장","측면","방식","설명","기준점",
  "관련","대한","위함","대해","때문","통해","위해","가지","자료","경우들",
  "문제","형태","기능","방법","최근","시작","기존","이상","표현",
  "과정","일반","우리나라","포함","차이","초기","효과","적용", "발의", "제정",
  "이해", "자동차", "국회", "정책", "오픈", "의료"
)

stop_theme <- c(
  "ai","인공","지능","intelligence","artificial","인공지능","인텔리전스"
)

stop_names <- c(
  "유승","최경진","구본권","이상희","조인철","헬렌","켈러","젠슨","고한",
  "방통위","과기","페이크", "네이버"
)
stopwords_ko <- unique(c(stop_spoken, stop_meta, stop_generic, stop_theme, stop_names))

# Data Loading (preserve in-file utterance order)
files <- list.files(pattern = "\\.xlsx$")
raw <- lapply(files, function(f)
  read_excel(f) %>% mutate(source_file = f, ord = row_number())) %>%
  bind_rows()

# Gov. vs. Non-Gov. labeling
# Hong & Davison 2010: pseudo-document
dat <- raw %>%
  filter(!is.na(내용), 구분 != "진행") %>%
  mutate(group = if_else(구분 == "정부", "정부", "비정부")) %>%
  arrange(source_file, ord) %>%
  group_by(source_file) %>%
  mutate(turn = cumsum(행위자 != lag(행위자, default = first(행위자)) |
                       group   != lag(group,   default = first(group)))) %>%
  ungroup() %>%
  group_by(source_file, turn) %>%
  summarise(group  = first(group),
            행위자 = first(행위자),
            내용   = paste(내용, collapse = " "),
            .groups = "drop") %>%
  mutate(doc_id = paste0("d", row_number()))

# Extract noun
extract_nouns <- function(text) {
  toks  <- kiwi$tokenize(text)
  keep  <- Filter(function(t) t$tag %in% c("NNG", "NNP", "SL"), toks)
  forms <- str_to_lower(vapply(keep, function(t) t$form, character(1)))
  forms <- forms[nchar(forms) >= 2]
  forms[!forms %in% stopwords_ko]
}

tokens <- dat %>%
  rowwise() %>% mutate(noun = list(extract_nouns(내용))) %>% ungroup() %>%
  select(doc_id, group, noun) %>% unnest(noun)

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
             control = list(alpha = 0.1, delta = 0.01, seed = SEED, burnin = 500, iter = 2000))
  top_terms <- tidy(lda, matrix = "beta") %>%
    group_by(topic) %>% slice_max(beta, n = TOP_N, with_ties = FALSE) %>%
    arrange(topic, desc(beta)) %>% ungroup()
  write.csv(top_terms, sprintf("lda_topterms_%s_kor.csv", grp),
            row.names = FALSE, fileEncoding = "UTF-8")
  p <- top_terms %>%
    mutate(term = reorder_within(term, beta, topic)) %>%
    ggplot(aes(beta, term, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free_y") + scale_y_reordered() +
    labs(title = sprintf("%s Discourse LDA Topics (k=%d)", GRP_EN[[grp]], k), x = "beta", y = NULL) +
    theme_minimal(base_family = "AppleGothic")
  ggsave(sprintf("lda_topterms_%s_kor.png", grp), p, width = 11, height = 7, dpi = 150)
  top_terms
}
tt_gov    <- run_lda("정부")
tt_nongov <- run_lda("비정부")

print_topics <- function(tt, grp) {
  for (tp in sort(unique(tt$topic)))
    cat(sprintf("  토픽 %d: %s\n", tp,
                paste(tt %>% filter(topic == tp) %>% pull(term), collapse = ", ")))
}
print_topics(tt_gov,    "정부")
print_topics(tt_nongov, "비정부")


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
write.csv(lo_debate, "logodds_gov_vs_nongov_kor.csv",
          row.names = FALSE, fileEncoding = "UTF-8")

cat("\n[오즈비] 정부 변별 어휘 Top 10 (z):\n")
print(lo_debate %>% filter(group == "정부")   %>% slice_max(z, n = 10) %>%
        transmute(word, z = round(z, 2), n))
cat("\n[오즈비] 비정부 변별 어휘 Top 10 (z):\n")
print(lo_debate %>% filter(group == "비정부") %>% slice_max(z, n = 10) %>%
        transmute(word, z = round(z, 2), n))

LO_TOP <- 10
lo_axis <- lo_debate %>% filter(group == "비정부")
lo_plot_df <- bind_rows(
  lo_axis %>% slice_max(z, n = LO_TOP, with_ties = FALSE),   # 비정부 변별어
  lo_axis %>% slice_min(z, n = LO_TOP, with_ties = FALSE)    # 정부 변별어
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
  theme_minimal(base_family = "AppleGothic")
ggsave("logodds_gov_vs_nongov_kor.png", p_lo, width = 9, height = 7, dpi = 150)
