# Copyright (c) 2017 YA-androidapp(https://github.com/YA-androidapp) All rights reserved.

# パッケージを読み込む
# install.packages('jsonlite')
# install.packages("quanteda") #テキスト分析
# # または install.packages("devtools"); devtools::install_github("kbenoit/quanteda")
# install.packages('igraph') # 共起ネットワーク分析
# install.packages('RCurl')
library(jsonlite)
library(quanteda)
library(RCurl)
library(igraph)

# JSONファイル群を読み込む
readJsons <- function() {
  # 初期化する
  result <- NULL

  # JSONファイル一覧を取得する
  files <- list.files()
  files.json <- grep('[0-9]{,4}_[0-9]{,2}\\.js$', files)
  files.jsontxt <- grep('[0-9]{,4}_[0-9]{,2}\\.js.txt$', files)
  file.remove(files[files.jsontxt])

  # JSONファイルごとに処理する
  for (fi in 1:length(files.json)) {
    orifile = files[files.json[fi]]
    tmpfile = sub('.js', ".js.txt", orifile) # paste('_', orifile, sep = '')

    f = file(orifile, 'r')

    # 各ファイル内の文字列処理を行う
    i = 0
    while (TRUE) {
      line = suppressWarnings(readLines(f, 1))
      if (length(line) == 0)
        break
      if (i == 0) {
        # ファイル先頭行を置換する
        i = i + 1
      } else {
        cat(line, '\n', sep = '', file = tmpfile, append = TRUE)
      }
    }

    # ファイル開放
    close(f)

    # JSONファイルをリストへ格納する
    # result <- jsonlite::read_json(tmpfile)
    result <- c( result, jsonlite::read_json(tmpfile) )
    file.remove(tmpfile)
  }

  return( result )
}
readdata <- readJsons()


# JSONファイルの内容をDataFrameへ格納する
getStatuses <- function(readdata) {
  # 初期化する
  f_created_at  <- as.POSIXct(as.character(NULL))
  f_id_str      <- as.character(NULL)
  f_screen_name <- as.character(NULL)
  f_source      <- as.character(NULL)
  f_text        <- as.character(NULL)
  f_username    <- as.character(NULL)

  for (item in readdata) {
    f_created_at   <- c(f_created_at,  as.POSIXct(item$created_at, format = "%Y-%m-%d %H:%M:%S +0000", tz = "UTC"))
    f_id_str       <- c(f_id_str,      item$id_str)
    f_screen_name  <- c(f_screen_name, item$user$screen_name)
    f_source       <- c(f_source,      item$source)
    f_text         <- c(f_text,        item$text)
    f_username     <- c(f_username,    item$user$name)
  }

  result <- data.frame(
    created_at = f_created_at,
    id_str = f_id_str,
    screen_name = f_screen_name,
    source = f_source,
    text = f_text,
    username = f_username,
    stringsAsFactors=FALSE
  )
  return( result )
}
statuses <- getStatuses(readdata)

head(statuses) # データフレームの確認
str(statuses)  # 文字型が因子型として扱われていないことを確認する



# quantedaによる文書解析

# 特徴語抽出

# 制約条件を加える
getTopFeatures <- function(dfm, len = 2, cnt = 2) {
  # 最低文字長
  result <- dfm_select(dfm, min_nchar = len)

  # 文字種
  # result <- dfm_remove(result, '^[ぁ-ん]+$', valuetype = 'regex')

  # 頻度
  result <- dfm_trim(result, min_count = cnt)
  return( result )
}

getDfm <- function(statuses) {
  # コーパスオブジェクトを生成
  quanteda.corpus <- corpus(statuses, text_field = 'text')
  ndoc(quanteda.corpus)
  head(quanteda.corpus)
  head(docvars(quanteda.corpus))

  texts(quanteda.corpus)[[1]]

  table(docvars(quanteda.corpus, 'source'))

  quanteda.corpus.web <- corpus_subset(quanteda.corpus, grepl('Twitter Web Client', source))
  ndoc(quanteda.corpus.web)

  table(weekdays(docvars(quanteda.corpus, 'created_at')))

  quanteda.corpus.ssv <- corpus_segment(quanteda.corpus, what = "other", delimiter = " ")
  ndoc(quanteda.corpus.ssv)
  texts(quanteda.corpus.ssv)



  # トークンオブジェクトを生成
  quanteda.tokens <- tokens(quanteda.corpus)
  # quanteda.tokens <- tokens(quanteda.corpus, what = "fastestword", remove_punct = FALSE) # 元データが半角スペースで分かち書きされている場合

  ntoken(quanteda.tokens)
  head(quanteda.tokens)

  # 文字種を制限する
  quanteda.tokens.selected <- tokens_select(quanteda.tokens, '^[ぁ-んァ-ヶーa-zA-Z0-9一-龠0-9、。]+$', valuetype = 'regex', padding = TRUE)
  quanteda.tokens.selected <- tokens_remove(quanteda.tokens, '^[0-9a-zA-Z]+$', valuetype = 'regex', padding = TRUE)

  quanteda.tokens.kwic <- kwic(quanteda.tokens, "RT") # KWIC索引の生成
  head(quanteda.tokens.kwic)
  quanteda.tokens.ngram <- tokens_ngrams(quanteda.tokens, n = 2) # トークンからn-gramを作成
  head(quanteda.tokens.ngram[[1]])

  # 共起語を結合
  quanteda.tokens.collocations <- textstat_collocations(
    quanteda.tokens,
    'bj',
    features = '^[０-９ァ-ヶー一-龠]+$',
    valuetype = 'regex',
    min_count = 1,
    nested = FALSE
    )
  # quanteda.tokens.compound <- tokens_compound(
  #   quanteda.tokens,
  #   quanteda.tokens.collocations[quanteda.tokens.collocations$p < 0.01,],
  #   valuetype = 'fixed',
  #   concatenator = '',
  #   join = TRUE
  #   )



  # document-feature matrixを生成
  quanteda.dfm <- dfm(quanteda.tokens)
  ndoc(quanteda.dfm)
  nfeature(quanteda.dfm)
  topfeatures(quanteda.dfm)
  head(quanteda.dfm)

  # 特長語の選択
  getTopFeatures(quanteda.dfm, 2, 2)

<<<<<<< HEAD

  return( list(tokens=quanteda.tokens, dfm=quanteda.dfm) )
}
quanteda.tokens <- getDfm(statuses)$tokens
quanteda.dfm <- getDfm(statuses)$dfm



# ワードクラウド
textplot_wordcloud(quanteda.dfm, comparison = FALSE)
textplot_wordcloud(quanteda.dfm, random.color = TRUE, rot.per = .25, colors = sample(colors()[2:128], 5))



# 相対頻度分析
quanteda.dfm.relative <- textstat_keyness(
  quanteda.dfm,
  docvars(quanteda.dfm, 'source') == 'Web' | docvars(quanteda.dfm, 'source') == 'Web'
  )
head(keys)



# 辞書分析
words <- c('RT', 'QT')

# 感情分析辞書（Higashiyama et al. 2008) の読み込み
dict <- dictionary(file = 'extra/higashiyama_sentiment.yml')

# 日時で集計する
names(quanteda.tokens) <- docvars(quanteda.tokens, 'created_at')
wordage <- rowSums(dfm_compress(dfm(quanteda.tokens)))

quanteda.tokens.1 <- as.tokens(kwic(quanteda.tokens, words[1]))
quanteda.dfm.1 <- dfm(quanteda.tokens.1, dictionary = dict)
quanteda.dfm.1 <- dfm_compress(quanteda.dfm.1)
quanteda.dfm.1 <- dfm_select(quanteda.dfm.1, documents = unique(names(quanteda.tokens)), valuetype = 'fixed', padding = TRUE) # 日付を補完
quanteda.dfm.1 <- quanteda.dfm.1[order(docnames(quanteda.dfm.1)),] # 日にちで並べ替え
plot((quanteda.dfm.1[,'positive'] - quanteda.dfm.1[,'negative']) / wordage, type = 'l')

quanteda.tokens.2 <- as.tokens(kwic(quanteda.tokens, words[2]))
quanteda.dfm.2 <- dfm(quanteda.tokens.2, dictionary = dict)
quanteda.dfm.2 <- dfm_compress(quanteda.dfm.2) # 日ごとに集計
quanteda.dfm.2 <- dfm_select(quanteda.dfm.2, documents = unique(names(quanteda.tokens)), valuetype = 'fixed', padding = TRUE) # 日付を補完
quanteda.dfm.2 <- mx_clinton[order(docnames(quanteda.dfm.2)),] # 日にちで並べ替え
plot((quanteda.dfm.2[,'positive'] - quanteda.dfm.2[,'negative']) / wordage , type = 'l')

plot((quanteda.dfm.1[,'positive'] - quanteda.dfm.1[,'negative']) / wordage, type = 'l',
    xaxt = 'n', ylab = 'ポジティブ・ネガティブ比', xlab = '時間')
lines((quanteda.dfm.2[,'positive'] - quanteda.dfm.2[,'negative']) / wordage, col = 'red')
axis(1, at = 1:366, seq.Date(as.Date('2016-01-01'), as.Date('2016-12-31'), 'days'))
grid()
legend('topleft', col = c('black', 'red'), legend = words, lty = 1)



# トピックモデル
require(topicmodels)

quanteda.dfm.lda <- LDA(convert(quanteda.dfm, to = "topicmodels"), k = 20)
get_terms(quanteda.dfm.lda, 10)



# 共起ネットワーク分析
require(igraph)

word <- 'Microsoft' # 単語
top <- 10 # 出力件数

key_jp <- rownames(
  textstat_keyness(
    quanteda.dfm,
    which(rowSums(dfm_select(quanteda.dfm, word)) > 0)
    )
  )

quanteda.dfm.top <- quanteda.dfm[,head(key_jp, top)]
quanteda.dfm.fcm <- fcm(quanteda.dfm.top, tri = FALSE) # 共起行列
diag(quanteda.dfm.fcm) <- 0 # 自己共起を無視
quanteda.dfm.fcm[quanteda.dfm.fcm < 1000] <- 0 # 低頻度の共起を無視

gr <- graph_from_adjacency_matrix(
  quanteda.dfm.fcm,
  weighted = TRUE,
  diag = FALSE,
  mode = 'undirected'
  )
igraph.options(
  plot.layout=layout_with_fr,
  vertex.label.family = 'sanserif',
  vertex.label.cex = 0.8
  )
plot(
  gr,
  edge.width = E(gr)$weight / 1000,
  edge.color = adjustcolor('sky blue', 0.5),
  vertex.color = adjustcolor('white', 1.0),
  vertex.frame.color = adjustcolor('sky blue', 1.0),
  vertex.label.color = 'black',
  edge.curved = 0.3,
  edge.arrow.size = 0.3,
  vertex.size = 20
  )



# 相対頻度分析

# 2つの文書の間で特に出現頻度が違う語を抽出する
quanteda.dfm.relative <- textstat_keyness(
  quanteda.dfm,
  docvars(quanteda.dfm, 'source') == '<a href="http://twitter.com/download/android" rel="nofollow">Twitter for Android</a>' |
  docvars(quanteda.dfm, 'source') == '<a href="https://about.twitter.com/products/tweetdeck" rel="nofollow">TweetDeck</a>'
  )
head(quanteda.dfm.relative)
