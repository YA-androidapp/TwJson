# Copyright (c) 2017 YA-androidapp(https://github.com/YA-androidapp) All rights reserved.

# 初期化する
indata <- NULL

# パッケージを読み込む
# install.packages('jsonlite')
library(jsonlite)

# JSONファイル群を読み込む
readJsons <- function() {
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
        cat('[{', '\n', sep = '', file = tmpfile, append = TRUE)
        i = i + 1
      } else {
        cat(line, '\n', sep = '', file = tmpfile, append = TRUE)
      }
    }

    # ファイル開放
    close(f)

    # JSONファイルをリストへ格納する
    # indata <- jsonlite::read_json(tmpfile)
    indata <<- c( indata, jsonlite::read_json(tmpfile) )
    file.remove(tmpfile)
  }
}
readJsons()
