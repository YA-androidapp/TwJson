# Copyright (c) 2017 YA-androidapp(https://github.com/YA-androidapp) All rights reserved.

# パッケージを読み込む
# install.packages('jsonlite')
library(jsonlite)

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
indata <- readJsons()



getDates <- function(indata) {
  # 初期化する
  result <- data.frame(text=NULL,created_at=NULL)
  
  for (item in indata) {
    result <- rbind(
      result,
      data.frame(
        created_at=as.POSIXlt(item$created_at, "JST"),
        # id_str=item$id_str,
        # screen_name=item$user$screen_name,
        # source=item$source,
        text=item$text,
        # username=item$user$name
        )
      )
  }
  
  return( result )
}
dates <- getDates(indata)

dates[1,]
indata[[1]]


