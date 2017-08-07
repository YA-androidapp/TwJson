# Copyright (c) 2017 YA-androidapp(https://github.com/YA-androidapp) All rights reserved.

# パッケージを読み込む
# install.packages('jsonlite')
# install.packages("quanteda") #テキスト分析
# # または install.packages("devtools"); devtools::install_github("kbenoit/quanteda")

library(jsonlite)
library(quanteda)

# JSONファイル群を読み込む
readJsons <- function() {
  # 初期化する
  result <- NULL

  # JSONファイル一覧を取得する
  files <- list.files()
  files.json <- grep('[0-9]{,4}_[0-9]{,2}\\.js$', files)

  # JSONファイルごと
  for (fi in 1:length(files.json)) {
    orifile = files[files.json[fi]]
    tmpfile = sub('.js', ".js.txt", orifile) # paste('_', orifile, sep = '')

    f = file(orifile, 'r')
    file.remove(tmpfile)

    # 各ファイル内の文字列処理を行う
    i = 0
    while (TRUE) {
      line = suppressWarnings(readLines(f, 1))
      if (length(line) == 0)
        break
      if (i == 0) {
        # ファイル先頭行の置換
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



getStatuses <- function(readdata) {
  # 初期化する
  result <- data.frame(
                       created_at = NULL,
                       id_str = item$id_str,
                       screen_name = item$user$screen_name,
                       source = item$source,
                       text = item$text,
                       username = item$user$name
  )

  for (item in readdata) {
    current <- data.frame(
      created_at = as.POSIXlt(item$created_at, "JST"),
      id_str = item$id_str,
      screen_name = item$user$screen_name,
      source = item$source,
      text = item$text,
      username = item$user$name
    )
    result <- merge(
      result,
      transform(current, name=levels(name)[name]),
      all = T
      )
  }

  return( result )
}
statuses <- getStatuses(readdata)






#as.POSIXct(tweet_df$created_at, format = "%a %b %d %H:%M:%S +0000 %Y") + (9 * 3600),

statuses <- merge(
  data.frame(
    created_at = Sys.time() - 600,
    id_str = 'id_str',
    screen_name = 'screen_name',
    source = 'source',
    text = 'text',
    username = 'username'
    ),
  data.frame(
    created_at = Sys.time(),
    id_str = 'id_str2',
    screen_name = 'screen_name',
    source = 'source2',
    text = 'text2',
    username = 'username2'
  ),
  all = T
  )
statuses <- transform(statuses, text=levels(text)[text])

statuses<-lapply(statuses[2:ncol(statuses)],as.character)
str(statuses)

getTopFeatures <- function(dfm, len = 2, cnt = 2) {
  # 最低文字長
  result <- dfm_select(dfm, min_nchar = len)

  # 文字種
  result <- dfm_remove(result, '^[ぁ-ん]+$', valuetype = 'regex')

  # 頻度
  result <- dfm_trim(result, min_count = cnt)
  return( result )
}

getDfm <- function(statuses) {
  # コーパスオブジェクトを生成
  quanteda.corpus <- corpus(statuses, text_field = 'text')
  ndoc(quanteda.corpus)
  summary(quanteda.corpus, n = 1)
  head(corp)
  head(docvars(corp))

  #

  texts(quanteda.corpus)[[1]]

  table(docvars(quanteda.corpus, 'source'))

  quanteda.corpus.web <- corpus_subset(quanteda.corpus, source == 'Web')
  ndoc(quanteda.corpus.web)

  table(weekdays(docvars(quanteda.corpus, 'created_at')))

  quanteda.corpus.ssv <- corpus_segment(quanteda.corpus, what = "other", delimiter = " ")
  ndoc(quanteda.corpus.ssv)
  head(texts(quanteda.corpus.ssv))



  # トークンオブジェクトを生成
  quanteda.tokens <- tokens(quanteda.corpus)
  # quanteda.tokens <- tokens(quanteda.corpus, what = "fastestword", remove_punct = FALSE) # 元データが半角スペースで分かち書きされている場合

  ntoken(quanteda.tokens)
  head(quanteda.tokens)

  # 文字種を制限する
  quanteda.tokens.selected <- tokens_select(quanteda.tokens, '^[ぁ-んァ-ヶーa-zA-Z0-9一-龠0-9、。]+$', valuetype = 'regex', padding = TRUE)
  quanteda.tokens.selected <- tokens_remove(quanteda.tokens, '^[0-9a-zA-Z]+$', valuetype = 'regex', padding = TRUE)

  quanteda.tokens.kwic <- kwic(quanteda.tokens, "RT") # KWIC索引の生成
  quanteda.tokens.ngram <- tokens_ngrams(quanteda.tokens, n = 2)
  head(quanteda.tokens.kwic)
  head(quanteda.tokens.ngram[[1]])

  # 共起語を結合
  quanteda.tokens.collocations <- textstat_collocations(
    quanteda.tokens,
    'bj',
    features = '^[０-９ァ-ヶー一-龠]+$',
    valuetype = 'regex',
    min_count = 10,
    nested = FALSE
    )
  quanteda.tokens.compound <- tokens_compound(
    quanteda.tokens,
    quanteda.tokens.collocations[quanteda.tokens.collocations$p < 0.01,],
    valuetype = 'fixed',
    concatenator = '',
    join = TRUE
    )



  # document-feature matrixを生成
  quanteda.dfm <- dfm(quanteda.tokens)
  ndoc(quanteda.dfm)
  nfeature(quanteda.dfm)
  topfeatures(quanteda.dfm)
  head(quanteda.dfm)

  # 特長語の選択
  getTopFeatures(quanteda.dfm, 2, 2)


  return( result )
}
dfm <- getDfm(statuses)























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





































