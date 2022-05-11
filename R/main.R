############ 測試文字 Big5 #####################

##################### -cm- ##############################
#' 清除console
#'
#' @return
#' @export
#'
#' @examples
tool.clear_console <-function(){
  cat("\014")
}


ccc <-function(){
  cat("\014")
}

#' 檢查是否載入這些套件，如果沒有，就載入。
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#' cm.library_load(magrittr,tidyr,png,jpeg)
cm.library_load <-function(...){
  args <- rlang::ensyms(...)
  #paste(purrr::map(args, rlang::as_string), collapse = " ")
  for (i in args){
    #print(i)
    if (rlang::as_string(i) %in% (.packages())){
      #print("yes")
    }else{
      #print("no")
      do.call("library", list(i))
    }
  }
}

#' 計算真值表
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
#'
#' # "%^%" <- xor # define unary xor operator
#'
#' truth_table("!A") # not
#' ##       A    !A
#' ## 1 FALSE  TRUE
#' ## 2  TRUE FALSE
#'
#' truth_table("A | B") # or
#' ##       A     B A | B
#' ## 1 FALSE FALSE FALSE
#' ## 2  TRUE FALSE  TRUE
#' ## 3 FALSE  TRUE  TRUE
#' ## 4  TRUE  TRUE  TRUE
#'
#' truth_table("A & B") # and
#' ##       A     B A & B
#' ## 1 FALSE FALSE FALSE
#' ## 2  TRUE FALSE FALSE
#' ## 3 FALSE  TRUE FALSE
#' ## 4  TRUE  TRUE  TRUE
#'
#' truth_table("A %^% B") # xor
#' ##       A     B A %^% B
#' ## 1 FALSE FALSE   FALSE
#' ## 2  TRUE FALSE    TRUE
#' ## 3 FALSE  TRUE    TRUE
#' ## 4  TRUE  TRUE   FALSE
#'
#' truth_table("S | (T %^% U)") # 3 variables with brackets
#' ##       S     T     U S | (T %^% U)
#' ## 1 FALSE FALSE FALSE         FALSE
#' ## 2  TRUE FALSE FALSE          TRUE
#' ## 3 FALSE  TRUE FALSE          TRUE
#' ## 4  TRUE  TRUE FALSE          TRUE
#' ## 5 FALSE FALSE  TRUE          TRUE
#' ## 6  TRUE FALSE  TRUE          TRUE
#' ## 7 FALSE  TRUE  TRUE         FALSE
#' ## 8  TRUE  TRUE  TRUE          TRUE
#'
#' truth_table("A %^% (B %^% (C %^% D))") # 4 variables with nested brackets
#' ##        A     B     C     D A %^% (B %^% (C %^% D))
#' ## 1  FALSE FALSE FALSE FALSE                   FALSE
#' ## 2   TRUE FALSE FALSE FALSE                    TRUE
#' ## 3  FALSE  TRUE FALSE FALSE                    TRUE
#' ## 4   TRUE  TRUE FALSE FALSE                   FALSE
#' ## 5  FALSE FALSE  TRUE FALSE                    TRUE
#' ## 6   TRUE FALSE  TRUE FALSE                   FALSE
#' ## 7  FALSE  TRUE  TRUE FALSE                   FALSE
#' ## 8   TRUE  TRUE  TRUE FALSE                    TRUE
#' ## 9  FALSE FALSE FALSE  TRUE                    TRUE
#' ## 10  TRUE FALSE FALSE  TRUE                   FALSE
#' ## 11 FALSE  TRUE FALSE  TRUE                   FALSE
#' ## 12  TRUE  TRUE FALSE  TRUE                    TRUE
#' ## 13 FALSE FALSE  TRUE  TRUE                   FALSE
#' ## 14  TRUE FALSE  TRUE  TRUE                    TRUE
#' ## 15 FALSE  TRUE  TRUE  TRUE                    TRUE
#' ## 16  TRUE  TRUE  TRUE  TRUE                   FALSE
cm.truth_table <- function(x) {
  vars <- unique(unlist(strsplit(x, "[^a-zA-Z]+")))
  vars <- vars[vars != ""]
  perm <- expand.grid(rep(list(c(FALSE, TRUE)), length(vars)))
  names(perm) <- vars
  perm[ , x] <- with(perm, eval(parse(text = x)))
  perm
  perm %<>%
    mutate_all(~ifelse(.=="TRUE",1,0))
  return(perm)
}

#' 逆排序
#'
#' @param df
#' @param col
#'
#' @return
#' @export
#'
#' @examples
darrange<-function(df,col){
  col = enquo(col)
  df %<>%
    arrange(desc(!!col))
  return(df)
}


#' print縮寫
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
pp<-function(...){
  print(...)
}


#' render Rmarkdown
#' 因為有中文在knitr編繹時，會出問題。
#' @param rmd.file
#'
#' @return
#' @export
#'
#' @examples
rdr<-function(rmd.file){
  rmarkdown::render(
    rmd.file
  )
  # 升級版
  # render_fun <- function(penguin){
  #   rmarkdown::render(
  #     input = "penguin.rmd"
  #     ,
  #     params = list(species = penguin),
  #     output_file = glue::glue(
  #       "{penguin}-report.html"
  #     )
  #   )
  # }
  # distinct(penguins, as.character(species)) %>%
  #   pull() %>%
  #   purrr::walk(render_fun)
  #




}



read.csv.big5<-function(file){
  df <- read.csv(file,
                 header = T,
                 sep = ",",
                 fileEncoding = "BIG5")
  return(df)
}

#' create selenium chrome
#' https://cran.r-project.org/web/packages/RSelenium/vignettes/basics.html
#' @return
#' @export
#'
#' @examples
#' chrome.create()
#' chrome$open()
#' chrome$navigate("http://www.pchome.com.tw/")
chrome.create<-function(){
  library(RSelenium)
  system('java -jar D:\\R_notebook\\env\\selenium-server-4.1.2.jar')
  chrome <<- remoteDriver(remoteServerAddr = "localhost", port = 4444, browserName = "chrome")
}

#' 尋找「下載」資料夾中，最新下載的檔案。
#'
#' @return
#' @export
#'
#' @examples
find.file.newest<-function(){
  file<-dir(dir.download) %>%
    data.frame() %>%
    `colnames<-`("file") %>%
    filter(grepl("pdf|PDF",file)) %>%
    mutate(info = (file.info(dir.download %+% file))$ctime) %>%
    arrange(desc(info)) %>%
    pull(file) %>%
    .[1]
  return(file)
}

#' system sleeping
#'
#' @param min
#' @param max
#'
#' @return
#' @export
#'
#' @examples
sleeping<-function(min,max){
  sleeptime = runif(1,min,max)
  #print(sleeptime)
  Sys.sleep(sleeptime)
}


kof.file.full<-function(keyword,dir=dir.r.bin){
  list.files(dir,full.names = T) %>%
    data.frame() %>%
    `colnames<-`("file") %>%
    filter(grepl(keyword,file)) %>%
    print()
}

#' Title
#'
#' @param keyword
#' @param dir
#'
#' @return
#' @export
#'
#' @examples
kof.file.short<-function(keyword,dir=dir.r.bin){
  list.files(dir,full.names = F) %>%
    data.frame() %>%
    `colnames<-`("file") %>%
    filter(grepl(keyword,file)) %>%
    print()
}

#' open file
#'
#' @param filepath full filepath
#'
#' @return
#' @export
#'
#' @examples
op<-function(filepath){
  system(paste0('open "', filepath, '"'))
}

#' 抓取TWSE單檔股票價格，但僅能抓單月
#' 係參考此文，以R改寫而得 https://medium.com/%E5%B7%A5%E7%A8%8B%E9%9A%A8%E5%AF%AB%E7%AD%86%E8%A8%98/5%E7%A8%AE%E6%8A%93%E5%8F%96%E5%8F%B0%E8%82%A1%E6%AD%B7%E5%8F%B2%E8%82%A1%E5%83%B9%E7%9A%84%E6%96%B9%E6%B3%95-766bf2ed9d6
#' @param stockcode
#' @param date_num
#'
#' @return
#' @export
#'
#' @examples
query.twsestock<-function(stockcode,date_num){

  date     = as.character(date_num)
  stock_no = as.character(stockcode)

  html = (glue::glue('https://www.twse.com.tw/exchangeReport/STOCK_DAY?response=json&date={date}&stockNo={stock_no}'))
  content = fromJSON(html)
  stock_data = content$data
  col_name   = content$fields

  df = data.frame(stock_data) %>%
    `colnames<-`(col_name)
  sleeping(2,3)
  return(df)
}

#' 基於query.twsestock，抓取多月份
#'
#' @param stockcode
#' @param vec_years
#'
#' @return
#' @export
#'
#' @examples
purrr.query.twsestock<-function(stockcode,vec_years){
  loops = purrr.loops2map(vec_years,1:12)

  loops = 10000*loops$rows + 100*loops$cols  +1
  loops = loops[loops<=tidy.list.date.date_to_num(today())]

  df = map_dfr(loops,~query.twsestock(as.character(stockcode),.x))
  return(df)
}


#' pdf2ggpage
#'
#' @param path_file
#' @param keyword_grepl_format
#'
#' @return
#' @export
#'
#' @examples
#' pdf2ggpage('D:\\FOMCpresconf20220316.pdf',"inflation|Inflation")
#' oppdf %>% op()
pdf2ggpage<-function(path_file,keyword_grepl_format){
  library(ggpage)
  #path_file = 'D:\\FOMCpresconf20220316.pdf'
  oppdf<<-path_file # for op use
  py_run_file(dir.r.bin %+%"pdf2paragraph.py")
  paragraph=py$pdf2paragraph(path_file)
  paragraph %>%
    strsplit(.,split="\n") %>%
    data.frame() %>%
    `colnames<-`("text") %>%
    ggpage_build() %>%
    mutate(long_word = stringr::str_detect(word,keyword_grepl_format)) %>%
    ggpage_plot(aes(fill = long_word))+
    labs(title = "keywords throughout paragraph",
         subtitle = keyword_grepl_format) +
    scale_fill_manual(values = c("grey70", "blue"),
                      labels = c("non", "keywords"),
                      name = "keyword")
}



#' 轉換watchdog慣用的Date和Name為中文名稱
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples
wd.trans.name_and_date<-function(df){

  if ("Date" %in% colnames(df) ){
    df %<>%
      dplyr::rename(日期=Date)
  }

  if ("Name" %in% colnames(df) ){
    df %<>%
      dplyr::rename(公司=Name)
  }

  return(df)
}


ggplotly.rangeslider<-function(p){
  ggplotly(p, dynamicTicks = TRUE) %>%
    rangeslider() %>%
    layout(hovermode = "x unified")
}


ggplotly.y.percentage<-function(p.ly){
  p.ly %>%
    layout(yaxis = list(tickformat = "%"))
}



#' Initially render RMD file to Rpubs
#'
#' Step1: Need to add surrfix to url of Rpubs
#' Step2: assign variable by ID of result
#' @param filename
#'
#' @return
#' @export
#'
#' @examples
knitr.render2rpubs.first <- function(filename){
  rmarkdown::render(paste0("D:\\R_notebook\\bin\\",filename,".Rmd"),"html_document")

  result <- rsconnect::rpubsUpload(
    title=filename,
    contentFile = paste0("D:\\R_notebook\\bin\\",filename,".html"),
    originalDoc = paste0("D:\\R_notebook\\bin\\",filename,".Rmd")
  )
  if (!is.null(result$continueUrl)){
    browseURL(result$continueUrl)
  }else{
    stop(result$error)
  }
  print(result$id)
  return(result)
}


#' Renew render RMD file to Rpubs
#'
#' @param filename
#' @param rpubs_id
#'
#' @return
#' @export
#'
#' @examples
knitr.render2rpubs.renew<-function(filename,rpubs_id){
  rmarkdown::render(paste0("D:\\R_notebook\\bin\\",filename,".Rmd"),"html_document")

  rsconnect::rpubsUpload(title = filename,
                         contentFile = paste0("D:\\R_notebook\\bin\\",filename,".html"),
                         originalDoc = paste0("D:\\R_notebook\\bin\\",filename,".Rmd"),
                         #id="https://api.rpubs.com/api/v1/document/896042/d9b6198bc3bc4cf2945047e12d872450"
                         id = rpubs_id
  )



}



##################### -ft- ##############################

#' flextable 的Tii預設模版
#'
#' @param ft
#'
#' @return
#' @export
#'
#' @examples
#' Data %>%
#'filter(Date==11103) %>%
#'  mutate(Date = as.character(Date)) %>%
#'  select(Date,Name,稅前損益,資金來源總計) %>%
#'  add.name.order() %>%
#'  arrange(Name) %>%
#'  wd.trans.name_and_date() %>%
#'  flextable() %>%
#'  ft.theme_tii() %>%
#'  save_as_image(path = dir.desktop %+%"test.png", webshot = "webshot2")
#'
ft.theme_tii<-function(ft){
  nrows = nrow_part(ft)
  fontname <- "Microsoft JhengHei"
  ft %<>%
    span_header() %>%
    border_remove() %>%
    hline(part="body"  ,border = fp_border(color="gray")) %>%
    hline(part="header",border = fp_border(color="black")) %>%
    hline(i = nrows    ,border = fp_border(color="white")) %>%
    vline(              border = fp_border(color="white",width=5) ) %>%
    align_text_col(    align = "left" , header = TRUE, footer = TRUE) %>%
    align_nottext_col( align = "right", header = TRUE, footer = TRUE) %>%
    font(fontname = fontname, part = "all") %>%
    fontsize(size =14, part = "header") %>%
    bold(bold=T,part="header") %>%
    autofit()

  return(ft)
}

ft.theme_tii.add_dollar_unit<-function(ft){
  ft %<>%
    add_header_lines(values = "單位:新台幣億元") %>%
    autofit()

  return(ft)
}



##################### -Index- ##############################
tidy.df.colname.from_rows<-function(df,x,y){
  if (missing(y)){
    cols   = df[x,]
    df = df[-c(x),]
  }else{
    cols   = df[x,] %+% "_" %+% df[y,]

    df = df[-c(x,y),]
  }
  df %<>%
    `colnames<-`(cols)

  return(df)
}


tidy.df.colname.row_fix_blank<-function(df,row.num){
  #處理標題列合併儲存格問題，將之補齊
  for (i in (1:ncol(df))){
    if (!is.na(df[row.num,i])){
      temp = df[row.num,i]
    }else{
      df[row.num,i]=temp
    }
  }

  return(df)
}

tidy.df.colname.remove_na<-function(df){
  cols<-colnames(df)
  cols = str_replace_all(cols,"_NA","")
  df %<>%
    `colnames<-`(cols)
  return(df)
}

tidy.df.colname.remove_rn<-function(df){
  cols<-colnames(df)
  cols = str_replace_all(cols,"\r\n"," ")
  cols = str_replace_all(cols,"\n"," ")
  cols = str_replace_all(cols,"\r"," ")
  cols = str_replace_all(cols,"  "," ")
  df %<>%
    `colnames<-`(cols)
  return(df)
}


tidy.df.firstrow_to_colnames <-function(dataset){

  colnames(dataset)<-dataset[1,]
  dataset<-dataset[2:nrow(dataset),]
}

tidy.df.first_row_as_title = tidy.df.firstrow_to_colnames


tidy.df.firstcol_to_rownames<-function(dataset){
  rownames(dataset) <- dataset[,1]
  dataset[,1] <- NULL
  return(dataset)
}

tidy.df.col_to_rownames<-function(datasets,name.col.select){
  datasets %<>%
    remove_rownames %>%
    column_to_rownames(var=name.col.select)

  return(datasets)
}

#' 分隔欄位，但不確定要分幾欄，也不指定分後欄位名稱。
#'
#' 方法來源:https://stackoverflow.com/questions/4350440/split-data-frame-string-column-into-multiple-columns
#' @param df  data.frame
#' @param col 指定column
#' @param sep 分隔符號
#'
#' @return data.frame
#'
#' @export
#'
#' @examples
#' path = "D:\\USERS\\400201\\Downloads\\tifrs-2021Q3"
#'list.files(path) %>%
#'  data.frame() %>%
#'  `colnames<-`("file") %>%
#'  tidy.df.col.seperate_to_unlimit_col(col=file,symbol="-")
tidy.df.col.seperate_to_unlimit_col<-function(df,col,sep){
  col = enquo(col)
  out <- strsplit(as.character(df %>% pull(!!col)),sep)
  res <- do.call(rbind, out) %>%
    data.frame()
  return(res)
}


tidy.df.char_to_separate_cols <- function(dataset){
  colnames(dataset)<-"x"
  nmax <- max(stringr::str_count(dataset$x, ",")) + 1
  dataset<-tidyr::separate(dataset, x, paste0("col", seq_len(nmax)), sep = ",")

}

tidy.df.allcol_to_char <-function(dataset){
  dataset %<>%
    mutate_if(~!is.character(.),funs(as.character(.)))

}

tidy.df.value_and_ratio <- function(df,base,exclude){
  base = enquo(base)
  exclude = enquo(exclude)

  df.new<- df %>%
    mutate_each(~./!!base,-(!!(exclude))) %>%
    mutate(type = "ratio") %>%
    select(type,everything())

  df %<>%
    mutate(type = "value") %>%
    select(type,everything())

  df.new <- rbind(df,df.new) %>%
    data.frame() %>%
    tidy.df.firstcol_to_rownames() %>%
    t() %>%
    data.frame()


  return(df.new)

}


tidy.df.T <- function(dataset){
  dataset %<>%
    t() %>%
    data.frame()
}

tidy.df.encoding<-function(df,from,to){
  for (col in colnames(df)){
    df[[col]] <- iconv( df[[col]] , from=from, to=to)
  }
  return(df)
}


tidy.df.big5_to_utf8<-function(df){
  df <- as.data.frame(lapply(df, function(x) iconv(x, "BIG-5", "UTF-8")))
}

tidy.df.POSIXct_to_date<-function(df,col){
  #POSIXct format to date
  col = enquo(col)
  df %<>%
    mutate(!!col := as.Date(as.POSIXct(!!col, origin="1970-01-01")))
  return(df)
}

tidy.df.date.from_excel = function(df,col){
  col = enquo(col)
  df %<>%
    mutate(!!col := as.Date(as.numeric(!!col), origin = "1899-12-30"))
}


#' 篩選yqmw的最後一天
#'
#' @param df
#' @param col  column
#' @param yqmw "y","q","m","w"
#'
#' @return
#' @export
#'
#' @examples
#' df %>%
#'  tidy.df.date.num_to_date(col=date) %>%
#'  filter.df.date.yqmw_lastday(.,col=date,yqmw="w")

filter.df.date.yqmw_lastday<-function(df,col,yqmw){
  col = enquo(col)
  if (yqmw=="y"){
    res<-df %>%
      group_by(year(!!col)) %>%
      filter(!!col==max(!!col))

  }else if (yqmw=="q"){
    res<-df %>%
      group_by(quarter(!!col)) %>%
      filter(!!col==max(!!col))

  }else if (yqmw=="m"){
    res<-df %>%
      group_by(month(!!col)) %>%
      filter(!!col==max(!!col))

  }else if (yqmw=="w"){
    res<-df %>%
      group_by(week(!!col)) %>%
      filter(!!col==max(!!col))
  }
  return(res)
}



#' ymd數字向量轉換成日期(date)向量，因為數字是十進位，但日期不是，
#' 所以此轉換會排除不合規的數字轉日期格式。
#'
#' @param list
#' @param num.output 控制是要輸出數字格式，還是日期格式。
#'
#' @return
#' @export
#'
#' @examples
#' tidy.list.date.num_to_date(c(20200101:20220101))
tidy.list.date.num_to_date<-function(list){
  res<-data.frame(num.ymd=list) %>%
    tidy.df.date.num2char(col = num.ymd) %>%
    mutate(date= as.Date(date)) %>%
    drop_na() %>%
    pull(date)
  return(res)
}

#' Title 日期格式轉成數字格式
#'
#' @param list
#'
#' @return
#' @export
#'
#' @examples
#' stock.tool.date.num_to_date(c(20210511:20220111)) %>% tidy.list.date.date_to_num()
tidy.list.date.date_to_num<-function(list){
  res<-list %>%
    as.character() %>%
    gsub("-","",.) %>%
    as.numeric()
  return(res)
}


#tidy.date.date2timestamp(1503678519)
date.date2timestamp<-function(value){
  output = as.POSIXct(value, origin="1970-01-01")
  return(output)
}



tidy.df.drop.na_cols<-function(datasets){
  #old name: drop.na.cols
  datasets %<>%
    data.frame() %>%
    select_if(~all(!is.na(.)))

}





#' 計算是那當月的那一週
#'
#' @param dates
#'
#' @return
#' @export
#'
#' @examples
#' dates<- seq(ymd(20210710), today(), by='day')
#' tool.list.date.weekofmonth(dates)
tool.list.date.weekofmonth<-function(dates){
  begin_week_on = "Monday"
  # day_names and day_index are for beginning the week on a day other than Sunday
  # (this vector ordering matters, so careful about changing it)
  day_names<- c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")

  # index integer of first match
  day_index<- pmatch(tolower(begin_week_on),
                     tolower(day_names))[1]
  n_week<- (5 +
              lubridate::day(dates) +
              lubridate::wday(floor_date(dates, 'month'),
                              week_start = day_index)
  ) %/% 7

  return(n_week)
}



#General
#as.numeric.factor: factor->character->numeric
#tt:transform
#vv:alias View
#num2char
#char2num
#ssa:Single+Size+Agg

#Time
#change_date
#date2zoo
#month_lastday
#date2time
#Unit
#bank
#dollar2million
#dollar2hundredmillion
#dollar2trillion

#get_data
#patched_order
#nf

#Ratio
#financial_ratio

#Check
#check.ratio.moneyuse:check 資金運用收益率

kbl_fixedheader<-function(df){
  df %>%
    scroll_box(width = "1920px", height = "1080px")
}

kbl_commas <-function(df){
  df %>%
    knitr::kable(digits = 0, format.args = list(big.mark = ",",
                                                scientific = FALSE), table.attr='class="table-fixed-header"') %>%
    kable_paper("hover",full_width=F,html_font = "Arial")
}


kbl_tii<-function(df){
  df %>%
    kbl() %>%
    kable_paper("hover",full_width=F,html_font = "Arial")
}


kbl_tii_html<-function(df){
  df %>%
    kable("html",escape = F,table.attr = "style = \"color: black;\"") %>%
    kable_paper("hover",full_width=F,html_font = "Arial")
}



kbl.to.png<-function(kbl.object,png.name,zoom){
  #kbl to png
  if (missing(zoom)){
    zoom = 1
  }
  kbl.object %>%
    save_kable(file = paste0(png.name,".png"),
               zoom = zoom)
}

#################ICS #################

f.ics.tidy.name_past<-function(df){
  df %<>%
    mutate(temp = str_extract(Name.temp,"\\d{3,5}(\\w{2,4})人壽|220中華郵政"),.before=Name.temp) %>%
    mutate(code = str_extract(temp,"\\d{3,5}"),.before=temp) %>%
    mutate(Name = str_extract(temp,"(?<=\\d{3,5})\\w{2,4}(?=人壽)|中華郵政"),.before=temp) %>%
    mutate(Name = ifelse(Name=="01全球","全球非區隔",ifelse(Name=="02全球","全球區隔",Name))) %>%
    mutate(Name = ifelse(Name=="合作金庫","合庫",Name)) %>%
    select(-Name.temp,-temp)
  return(df)
}


f.ics.tidy.name<-function(df){
  df %<>%
    mutate(Year = str_extract(Name.temp,"(\\d{4})"),.before=Name.temp) %>%
    mutate(Name = str_extract(Name.temp,"_\\w{2,5}"),.before=Name.temp) %>%
    mutate(Name = str_remove(Name,"_")) %>%
    select(-Name.temp)
  return(df)
}


f.ics.combine<-function(file,sheet){
  df.temp = read_excel(dir.r.ics  %+% file,sheet=sheet,col_names = F)
  df.temp %>%
    `colnames<-`(paste0("X",(1:ncol(.)))) %>%
    mutate(row = (1:nrow(.))) %>%
    mutate(Name.temp = file,.before=row) %>%
    select(Name.temp,row,everything())
}


f.ics.addyear<-function(df,year){
  df %<>%
    mutate(year  = year,.before=everything())
  return(df)
}


f.ics.T.bining <-function(df,Table){
  fields<-Hash.ics.T %>%
    filter(T.Year==df$Year[1]) %>%
    filter(`TT` == Table)

  if (is.na(fields$End[1])){
    result<-df %>%
      filter(row>=fields$Start[1])
  }else{
    result<-df %>%
      filter(row>=fields$Start[1],row<=fields$End[1])
  }

  return(result)
}


f.ics.check.T.available<-function(){
  #確認變數是否存在
  df = data.frame()
  for (year in c(2021,2020)){
    df.temp = data.frame()
    for (i in c(1:74)){
      df.temp[i,1] = "Y" %+% as.character(year)
      df.temp[i,2] = "T" %+% as.character(i)

      if (exists(paste0("df.ics.T",as.character(i),".",as.character(year)))){
        df.temp[i,3]="v"
      }
    }
    df = rbind(df,df.temp)
  }

  df %>%
    select(Year=V1,TT=V2,exists=V3) %>%
    pivot_wider(names_from = Year,values_from = exists)
}


f.ics.look.T.info<-function(table){
  #查看表的名稱與細節
  if (!exists("Hash.ics.T")){
    Hash.ics.T = read_excel(dir.r.ics %+% "ICS.T.xlsx")
  }

  if (missing(table)){
    df = (Hash.ics.T)
  }else{
    print(1)
    df<-Hash.ics.T %>%
      filter(TT %in% table)
  }
  return(df)
}


f.ics.setvar<-function(df,Table){
  assign("df.ics." %+% Table %+% "." %+% df$Year[1],df, envir = .GlobalEnv)
}

#################RBC #################
f.rbc.get<-function(year){
  #抓RBC 資料
  year = as.character(year)

  rbc.file.name = paste0("RBC",year,".Rdata")
  load(paste0(dir.r.raw,rbc.file.name))
  temp.data = eval(parse(text=paste0("RBC.",year)))

  if (isMac()){
    if (nchar(year)==4){
      idicator = as.numeric(year)
    }else{
      idicator = as.numeric(year)/100
    }

    if (idicator>=2021 | idicator==2020){
    }else{
      print("轉檔")
      temp.data %<>%
        tidy.df.big5_to_utf8()
    }
  }
  return(temp.data)
}


f.rbc.select_sheet <-function(df,sheet){
  #選取RBC的表
  sheet = enquo(sheet)
  df %<>%
    filter(sheet==!!sheet) %>%
    rbc.filter.life.all() %>%
    rbc.add.name()

  return(df)
}



f.rbc.rbc.tidy<-function(year){
  print(year)
  year = as.character(year)
  temp.data = f.rbc.get(year)

  if (nchar(year)==4){
    idicator = as.numeric(year)
  }else{
    idicator = as.numeric(year)/100
  }

  if (idicator>=2019 & year!="201906"){
    #因為2018年value是c3欄位，之後是c2欄位。
    df.rbc.ratio<-temp.data %>%
      f.rbc.select_sheet("表30-1") %>%
      #filter(Name=="宏泰") %>%
      filter(row>=12 &row<=25) %>%
      select(Name,row,account=c1,value=c2) %>%
      {
        if(is.factor(.$value[1]))
          mutate(.,value = as.numeric.factor(value))
        else
          mutate(.,value = as.numeric(value))
      } %>%
      {
        if(is.factor(.$account[1]))
          mutate(.,account = as.character.factor(account))
        else
          mutate(.,account = as.character(account))
      } %>%
      mutate(account=trimws(account)) %>%
      as.data.table() %>%
      dcast(row+account~Name,sum) %>%
      data.frame() %>%
      arrange.rbc.com.colname() %>%
      mutate(國泰 = 原國泰+原國寶幸福,
               南山 = 原南山+原朝陽,
               全球 = 原全球+原國華) %>%
      select(-原國泰,-原國寶幸福,-原南山,-原朝陽,-原全球,-原國華,-row) %>%
      as.data.table() %>%
      melt(id.vars= c("account"),variable.name="Name") %>%
      mutate(account = ifelse(account=="風險資本總額(註)","風險資本總額",account)) %>%
      filter(account!="資本適足率") %>%
      mutate(value = value/10^8) %>%
      dcast(Name~account,sum) %>%
      mutate(RBC = 自有資本總額/風險資本總額) %>%
      mutate(Year = year,.before="Name")
  }else{
    df.rbc.ratio<-temp.data %>%
      f.rbc.select_sheet("表30-1") %>%
      #filter(Name=="宏泰") %>%
      filter(row>=12 &row<=25) %>%
      select(Name,row,account=c2,value=c3) %>%
      {
        if(is.factor(.$value[1]))
          mutate(.,value = as.numeric.factor(value))
        else
          mutate(.,value = as.numeric(value))
      } %>%
      {
        if(is.factor(.$account[1]))
          mutate(.,account = as.character.factor(account))
        else
          mutate(.,account = as.character(account))
      } %>%
      mutate(account=trimws(account)) %>%
      as.data.table() %>%
      dcast(row+account~Name,sum) %>%
      data.frame() %>%
      arrange.rbc.com.colname() %>%
      mutate(國泰 = 原國泰+原國寶幸福,
               南山 = 原南山+原朝陽,
               全球 = 原全球+原國華) %>%
      select(-原國泰,-原國寶幸福,-原南山,-原朝陽,-原全球,-原國華,-row) %>%
      as.data.table() %>%
      melt(id.vars= c("account"),variable.name="Name") %>%
      mutate(account = ifelse(account=="風險資本總額(註)","風險資本總額",account)) %>%
      filter(account!="資本適足率") %>%
      mutate(value = value/10^8) %>%
      dcast(Name~account,sum) %>%
      mutate(RBC = 自有資本總額/風險資本總額) %>%
      mutate(Year = year,.before="Name")
  }
  rm(temp.data)
  return(df.rbc.ratio)
}

#' RBC報表皆是用c1:C256去存取資料庫，故使用表格表的表頭去修改表的欄位名稱
#'
#' @param df
#' @param x 欄位名稱所在row
#' @param y 複合欄位
#'
#' @return
#' @export
#'
#' @examples
f.rbc.add_colname<-function(df,x,y){

  cols<-df %>%
    colnames()
  cols_header = cols[!grepl("c[0-9]+",cols)] #不更名的部份
  cols_tail   = cols[ grep("^c[0-9]+",cols)] #要更名的部份

  if (missing(y)){
    cols_tail   = df[x,cols_tail]
  }else{
    cols_tail   = df[x,cols_tail] %+% df[y,cols_tail]
  }
  cols = c(cols_header,cols_tail)

  df %<>%
    `colnames<-`(cols)
  return(df)
}

#' 去除RBC中空白欄位和不必的系統欄位
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples
f.rbc.drop_cols<-function(df){
  df %<>%
    select(-last_mod_time,-last_mod_com,-last_mod_acc) %>%
    tidy.df.drop.na_cols()
  return(df)
}


#################purrr #################

#' 創造loops for purrr::map
#'
#' @param rows
#' @param cols
#'
#' @return
#' @export
#'
#' @examples
purrr.loops2map<-function(loops_i,loops_j){
  maps.rows = NULL
  maps.cols = NULL
  for (i in loops_i){
    for (j in loops_j){
      maps.rows = c(maps.rows,i)
      maps.cols = c(maps.cols,j)
    }
  }
  return(list("rows"=maps.rows,
              "cols"=maps.cols))
}

#################plot #################
geom_vhline <- function(x=0,y=0) {
  list(
    geom_point(),
    #coord_flip(),
    geom_hline(yintercept=x),
    geom_vline(xintercept=y)
  )
}


geom_text_repel_all<-function(column){
  column = ensym(column)
  list(
    geom_text_repel(aes(label=!!column),max.overlaps = getOption("ggrepel.max.overlaps", default = 50),
                    min.segment.length = unit(0,'line'))

  )

}


geom_text_repel_addline<-function(column){
  column = ensym(column)
  list(
    geom_text_repel(aes(label=!!column),
                    min.segment.length = unit(0,'line'))

  )

}



plot.quadrant<-function(x,y,title,quadrant1=NULL,quadrant2=NULL,quadrant3=NULL,quadrant4=NULL){
  library(ggplot2)
  # custom empty theme to clear the plot area
  empty_theme <- theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_text(angle = 90)
  )

  plot <- ggplot(NULL, aes()) +
    # fix the scale so it's always a square
    coord_fixed() +
    # set the scale to one greater than 0-10 in each direction
    # this gives us some breating room and space to add some arrows
    scale_x_continuous(expand = c(0, 0), limits = c(-1, 11),
                       breaks = c(2,8), labels=c("2" = "", "8" = "")) +
    scale_y_continuous(expand = c(0, 0), limits = c(-1,11),
                       breaks = c(2,8), labels=c("2" = "", "8" = "")) +
    # apply the empty theme
    empty_theme +
    # labels
    labs(title = title,
         x = x,
         y = y )+
    # create the quadrants
    geom_segment(aes(x = 10, y = 0, xend = 10, yend = 10)) +
    geom_segment(aes(x = 0, y = 0, xend = 0, yend = 10)) +
    geom_segment(aes(x = 0, y = 0, xend = 10, yend = 0)) +
    geom_segment(aes(x = 0, y = 5, xend = 10, yend = 5)) +
    geom_segment(aes(x = 5, y = 0, xend = 5, yend = 10)) +
    geom_segment(aes(x = 0, y = 10, xend = 10, yend = 10)) +
    # quadrant labels
    ggplot2::annotate("text", x = 2.5, y = 2.5, alpha = 0.8,  label = quadrant3,size=8) +
    ggplot2::annotate("text", x = 2.5, y = 7.5, alpha = 0.8,  label = quadrant2,size=8) +
    ggplot2::annotate("text", x = 7.5, y = 2.5, alpha = 0.35, label = quadrant4,size=8) +
    ggplot2::annotate("text", x = 7.5, y = 7.5, alpha = 0.35, label = quadrant1,size=8) +
    # arrows are cut in half which conveniently matches the gartner one
    ggplot2::annotate("segment", x = 0, xend = 10, y = -1, yend = -1,colour = "blue",
                      size=2, alpha=1, arrow=arrow(type = "closed", angle = 15)) +
    ggplot2::annotate("segment", x = -1, xend = -1, y = 0, yend = 10, colour = "blue",
                      size=2, alpha=1, arrow=arrow(type = "closed", angle = 15))
  return(plot)
}


##################### dropbox ##############################
drop.list.files<-function(dir.from_drop){
  list<-drop_dir(dir.from_drop) %>%
    filter(.tag =="file") %>%
    select(name) %>%
    mutate(where="drop",.before=name)

  return(list)
}


drop.check_diff<-function(dir_or_lists.from_localhost,dir.from_drop){
  #查看dropbox與localhost的檔案差異
  #diff = drop.check_diff(dir.r.stock.bio,dir.drop.bio)

  if (length(dir_or_lists.from_localhost)==1){
    lists = list.files(dir_or_lists.from_localhost)
  }else{
    lists = dir_or_lists.from_localhost
  }

  df.diff<-lists %>%
    data.frame() %>%
    dplyr::rename(name = ".") %>%
    mutate(where = "localhost",.before=name) %>%
    rbind(.,drop_dir(dir.from_drop) %>%
            filter(.tag =="file") %>%
            select(name) %>%
            mutate(where="drop",.before=name)) %>%
    mutate(count = 1) %>%
    as.data.table() %>%
    dcast(name~where,sum,value.var = "count") %>%
    mutate(queue = ifelse(localhost==0 & drop!=0,1,0))


  queue<-df.diff %>%
    filter(queue==1) %>%
    pull(name)

  diff = list("table"=df.diff,"queue"=queue)
  return(diff)
}



##################### news ##############################
news.get_from_local <-function(){
  news.tw <<- read_excel(dir.news %+% "news.xlsx")%>%
    trim_u00a0() %>%
    mutate(date= as.Date(date)) %>%
    #filter(Time>=as.Date("2020-01-01")) %>%
    filter(abstract!="")
  return(news.tw)
}

news.add_hyperlink<-function(news){
  news %<>%
    mutate(link = "file:///D:/news/news" %+%
             str_sub(download,1,4) %+%
             "/"                   %+%
             download              %+%
             ifelse(date.num>=20161006,".pdf",".tif"))
  return(news)
}


news.get<-function(){
  if (!exists("news.tw")){
    news.tw = news.get_from_local()
  }
  return(news.tw)
}


news.grep<-function(news,keyword.re.format,afterday.date.format=NULL){

  if (missing(afterday.date.format)){
    df<-news %>%
      filter(grepl(keyword.re.format,title)|grepl(keyword.re.format,abstract))
  }else{
    df<-news %>%
      filter(date>=afterday.date.format) %>%
      filter(grepl(keyword.re.format,title)|grepl(keyword.re.format,abstract))
  }
  return(df)
}

news.show<-function(df){
  df %>%
    news.add_hyperlink() %>%
    mutate(link = cell_spec(title, "html", link = link)) %>%
    select(date,link,print,abstract) %>%
    kable(format = "html", escape = FALSE,table.attr = "style = \"color: black;\"") %>%
    kable_styling(bootstrap_options = c("hover", "condensed"))
  #kbl.display_with_hyperlink.news()
}

##################### shiny ##############################
shiny.insurance <-function(f){
  #
  ui <- fluidPage(
    inputPanel(
      selectInput('x', 'X', choices = c(Hash.Company.Uni %>% pull(Name),"業界"),
                  selected = "業界") #defalut
    ),

    mainPanel(plotOutput("outplot",width = "100%", height = "800px"),
              width=12
    )
  )

  server <- function(input, output) {

    output$outplot <- renderPlot({
      f(input$x)
    })

  }
  shinyApp(ui = ui, server = server)
}


##################### sqlite ##############################
sqlite.con <-function(file.database){
  #建立database連線con
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = file.database)
  return(con)
}

sqlite.db_to_dfdb<-function(file.database.name,table.name){
  #database.name = paste0(dir.r.stock,'stock.ohlc.db')
  #table.name    ="stock.ohlc"
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = file.database.name)
  dfdb <- dplyr::tbl(con,table.name)
  return(dfdb)
}

##################### stock ##############################
stock.ohlc.tidy<-function(json.ohlc){
  jj = fromJSON(json.ohlc)
  df.ohlc<-jj$data9 %>%
    data.frame() %>%
    `colnames<-`(jj$fields9) %>%
    filter(!證券代號 %in% paste0("0",as.character(30001:90000))) %>%
    filter(!grepl("P|F|Q|C|B|X|Y",證券代號)) %>%
    mutate_at(c("開盤價","最高價","最低價","收盤價","最後揭示買價","最後揭示賣價"),~ifelse(.=="--",NA,.)) %>%
    mutate(across(everything(), ~str_replace_all(.,",",""))) %>%
    mutate(across(c(everything(),-證券代號,-證券名稱,-`漲跌(+/-)`),
                  ~as.numeric(.))) %>%
    arrange(desc((成交金額))) %>%
    mutate(date = jj$date,.before =證券代號) %>%
    mutate(成交股數     = 成交股數/1000,
               #最後揭示買量 = 最後揭示買量/1000,
               #最後揭示賣量 = 最後揭示賣量/1000,
               成交金額     = 成交金額/10^8) %>%
    dplyr::rename(stockcode = 證券代號,
                  stockname = 證券名稱,
                  成交張數  = 成交股數,
                  最後揭示買張 = 最後揭示買量,
                  最後揭示賣張 = 最後揭示賣量,
                  成交金額億   = 成交金額,
                  漲跌         =`漲跌(+/-)`) %>%
    mutate(market = "twse",.before=date)

  return(df.ohlc)
}

stock.ohlc.json_to_db<-function(lists){
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = paste0(dir.r.stock,'stock.ohlc.db'))
  for (file in lists){
    print(file)
    json.ohlc = paste0(dir.r.stock.ohlc , file)
    df = stock.ohlc.tidy(json.ohlc)
    dbWriteTable(con, df, name="stock.ohlc",append=TRUE )
  }
  dbDisconnect(con)

}

stock.ohlc.drop_to_db<-function(){
  #同步local和dropbox
  if (file.exists(paste0(dir.r.stock,'stock.ohlc.db'))){
    con <<- DBI::dbConnect(RSQLite::SQLite(), dbname = paste0(dir.r.stock,'stock.ohlc.db'))
    df <- dplyr::tbl(con, "stock.ohlc")
    date.newest<-df %>%
      arrange(desc(date)) %>%
      head() %>%
      collect() %>%
      pull(date) %>%
      .[1]
    dbDisconnect(con)
    #rm(con)

    lists<-drop_dir(dir.drop.ohlc) %>%
      mutate(date = str_extract(name,"\\d{8}"),.before=name) %>%
      filter(date>date.newest) %>%
      pull(name)

    ic("更新" %+% lists)

    map(lists,~drop_download(paste0(dir.drop.ohlc,.x),
                             paste0(dir.r.stock.ohlc,.x),overwrite = T))

    stock.ohlc.json_to_db(lists)

  }else{
    lists<-drop_dir(dir.drop.ohlc) %>%
      mutate(date = str_extract(name,"\\d{8}"),.before=name) %>%
      pull(name)

    ic("更新" %+% lists)

    map(lists,~drop_download(paste0(dir.drop.ohlc,.x),
                             paste0(dir.r.stock.ohlc,.x)))

    stock.ohlc.json_to_db(lists)
  }

  ic("完成")

}

#' get OHLC from db
#'
#' @param dates 可以空白，空白抓全期
#' @param stocks 可以空白，空白抓全部股票
#'
#' @return
#' @export
#'
#' @examples
stock.ohlc.get<-function(dates,stocks){
  #get stocks ohlc
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = paste0(dir.r.stock,'stock.ohlc.db'))
  df <- dplyr::tbl(con, "stock.ohlc")

  if (missing(dates) & missing(stocks)){
    df %<>%
      collect()
  }else if (missing(dates)){
    stocks = as.character(stocks)
    df %<>%
      filter(stockname %in% stocks | stockcode %in% stocks) %>%
      collect()
  }else if (missing(stocks)){
    dates = as.character(dates)
    df %<>%
      filter(date %in% dates) %>%
      collect()
  }else{
    dates = as.character(dates)
    stocks = as.character(stocks)
    df %<>%
      filter(date %in% dates) %>%
      filter(stockname %in% stocks | stockcode %in% stocks) %>%
      collect()
  }

  dbDisconnect(con)
  return(df)
}

stock.bio.db_to_dbs<-function(db.bio){
  #db 輸入資料庫
  ic(db.bio)
  #db
  #dfdb = sqlite.db_to_dfdb(dir.r.stock.bio %+% db.bio,"bio")
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = dir.r.stock.bio %+% db.bio)
  dfdb <- dplyr::tbl(con,"bio")
  df<-dfdb  %>%
    select(-filename,-brokercode,-brokercodes,-brokername) %>%
    collect()  %>%
    mutate(date = paste0(year,month,day),.before=stockcode) %>%
    select(-year,-month,-day) %>%
    mutate(price     = as.numeric(price),
           buypiece  = as.numeric(buypiece) /1000,
           sellpiece = as.numeric(sellpiece)/1000) %>%
    filter(!is.na(price) & !is.na(buypiece) & !is.na(sellpiece))
  dbDisconnect(con)

  #dbs
  con = sqlite.con(dir.r.stock.bio %+% "stock.bio.db")

  if (length(dbListTables(con))==0){
    dbWriteTable(con,name="stock.bio",df)
  }else{
    dbWriteTable(con,name="stock.bio",df,append=TRUE)
  }
  ic("bio已更新至資料庫")
  dbDisconnect(con)

}

stock.bio.drop_to_db<-function(){
  #自動dropbox to dbs
  #查看資料庫裡的日期與dropbox上檔案的差異
  if (file.exists(dir.r.stock %+% "stock.bio.db")){
    dfdb = sqlite.db_to_dfdb(dir.r.stock %+% "stock.bio.db","stock.bio")
    diff<-dfdb %>%
      select(date) %>%
      distinct() %>%
      collect() %>%
      mutate(date = paste0("bio_",date,".db")) %>%
      pull(date) %>%
      drop.check_diff(.,dir.drop.bio)

    if (length(diff$queue)==0){
      ic("bio已是最新")
    }else{
      map(diff$queue,~drop_download(dir.drop.bio     %+% .x,
                                    dir.r.stock.bio  %+% .x,overwrite=T))

      map(diff$queue,~stock.bio.db_to_dbs(.x))
    }
  }else{
    list <- drop.list.files(dir.drop.bio) %>%
      pull(name)


    map(list,~drop_download(dir.drop.bio     %+% .x,
                            dir.r.stock.bio  %+% .x))

    map(list,~stock.bio.db_to_dbs(.x))

  }



  #下載差異

}

stock.bio.get<-function(days=NULL,stocks=NULL){
  #days 可選回溯天數，也可以選擇確定日期
  #stocks 可以是證券代號，也可以是證券名稱

  dfdb <<- sqlite.db_to_dfdb(dir.r.stock.bio %+% "stock.bio.db","stock.bio")
  df   = stock.parameter.get.days_x_stocks(dfdb,days,stocks)
  if (exists("dfdb")){
    #rm(dfdb)
  }

  return(df)
}


stock.date.tolist<-function(){
  tryCatch({
    stock.ohlc.drop_to_db()
  }, warning = function(w) {
    # 警告處理
  }, error = function(e) {
    print("處於限制性網路環境!")
    # 錯誤處理
  }, finally ={

  })
  #更新OHLC from dropbox

  #列出所有交易日
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = paste0(dir.r.stock,'stock.ohlc.db'))
  df <- dplyr::tbl(con, "stock.ohlc")
  lists<-df %>%
    select(date) %>%
    distinct() %>%
    arrange(desc(date)) %>%
    collect() %>%
    pull(date) %>%
    as.character()

  dbDisconnect(con)
  return(lists)
}


stock.date.pick<-function(pick.days){
  #選取近1日、3日、7日等區間
  lists = stock.date.tolist()
  lists[1:pick.days]
}

stock.date.move_period<-function(days.move_period,days.total){

  #df = stock.date.move_period(7,28)
  #df %>% vv
  period = stock.date.pick(days.total)
  periods = data.frame()

  for (i in 1:(length(period)-days.move_period+1)){
    periods = rbind(periods,period[i:(i+days.move_period-1)])
  }

  colnames(periods) = NULL

  return(periods)

}


stock.tags.tidy<-function(){
  #取得各公司的特色tags
  path = paste0(dir.r.stock,"tag/","stock_tags.csv")
  if(!file.exists(path)){
    drop_download(dir.drop %+% "tag/stock_tags.csv",dir.r.stock %+% "tag/"%+%"stock_tags.csv")
  }else{
    print("file exist")
  }

  stock.tags <- read.csv(path) %>%
    select(-X) %>%
    tidyr::extract(stock,c("stockcode"),"([a-zA-Z0-9]{4,6})",remove=F) %>%
    distinct()
  return(stock.tags)
}

stock.tags.grep<-function(grep.keyword){
  #stock.tags.grep("南帝|申豐")
  stock.tags=stock.tags.tidy()
  result<-stock.tags %>%
    filter(grepl(grep.keyword,stock)|grepl(grep.keyword,tag)) %>%
    mutate(count=1) %>%
    select(-stockcode) %>%
    as.data.table() %>%
    dcast(stock~tag,value.var ="count" ) %>%
    mutate(across(c(everything(),-stock),~ifelse(.==1,"●",""))) %>%
    mutate(across(c(everything(),-stock),~ifelse(is.na(.),"",.)))
  return(result)
}

#' 取得股票代號與股票名稱之對照表
#'
#' @param dates
#'  20211224 數字格式
#' @return df
#' @export
#'
#' @examples
#' hash.stock = stock.hash.get
stock.hash.get <-function(dates=NULL){
  if (missing(dates)){
    dates = stock.date.pick(30)
  }
  hash.stock<<-stock.ohlc.get(dates) %>%
    select(stockcode,stockname) %>%
    distinct()

  return(hash.stock)
}

stock.tool.addname<-function(df){
  if (!exists("hash.stock")){
    stock.hash.get(stock.date.pick(1))
  }
  res<-df %>%
    merge(hash.stock, by="stockcode",all.x=T) %>%
    select(stockname,everything())
  return(res)
}

#' 載入最詳細的股票(上市、上櫃、興櫃、公開發行)基本資料
#' https://mops.twse.com.tw/mops/web/t51sb01
#'
#' @return
#' @export
#'
#' @examples
stock.hash.get.detail<-function(){
  hash.stock.detail <<- read_xlsx(dir.r.stock %+% "Hash.TWSE股票重要基本資料.xlsx") %>%
    dplyr::rename(stockcode=公司代號,stockname=公司簡稱) %>%
    mutate(stockcode=as.character(stockcode))
}


#' 加入stockcode
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples
stock.tool.add_stockname<-function(df){
  if (!exists("hash.stock.detail")){
    stock.hash.get.detail()
  }

  res<-df %>%
    merge(select(hash.stock.detail,stockcode,stockname),by="stockcode",all.x=T) %>%
    select(stockname,everything())
  return(res)
}




#' stockcode to stockname
#'
#' @param code
#'
#' @return
#' @export
#'
#' @examples
#' stock.tool.code2name(c(2330,2317))
#' stock.tool.code2name(2330)
stock.tool.code2name<-function(code){
  #library(dplyr, verbose=TRUE)

  if (!exists("hash.stock.detail")){
    stock.hash.get.detail()
  }

  if (length(code)<2){
    res<-hash.stock.detail %>%
      filter(stockcode %in% as.character(code)) %>%
      pull(stockname)
  }else{
    res<-hash.stock.detail %>%
      filter(stockcode %in% as.character(code))
  }
  return(res)
}


#' stockname to stockcode
#'
#' @param name
#' 公司名稱或公司名稱序列
#' @return
#' @export
#'
#' @examples
#' stock.tool.name2code(c("永豐實","台積電"))
#' stock.tool.name2code("台積電")
stock.tool.name2code<-function(name){
  #library(dplyr, verbose=TRUE)
  if (!exists("hash.stock.detail")){
    stock.hash.get.detail()
  }
  if (length(name)<2){
    res<-hash.stock.detail %>%
      filter(stockname %in% as.character(name)) %>%
      pull(stockcode)
  }else{
    res<-hash.stock.detail %>%
      filter(grepl(as.character(name),stockname))
  }
  return(res)
}

#' Title 排除週六與週日之非交易日。
#'
#' @param list
#'
#' @return
#' @export
#'
#' @examples
#' stock.tool.date.num_to_date(c(20210511:20220111))
stock.tool.date.num_to_date<-function(list){
  res<-tool.list.date.num_to_date(list) %>%
    data.frame() %>%
    `colnames<-`("date") %>%
    mutate(day = weekdays(date)) %>%
    arrange(desc(date)) %>%
    filter(weekdays(date)!="週六",weekdays(date)!="週日") %>%
    pull(date)
  return(res)
}


stock.profile.link <-function(stockcode){
  #直接複製該檔股票之雅虎基本資料連結至剪貼簿。
  #stock.profile.link(2317)
  stockcode = as.character(stockcode)
  link = glue::glue('https://tw.stock.yahoo.com/quote/{stockcode}.TW/profile')
  clipr::write_clip(link)
}


stock.parameter.get.days_x_stocks<-function(dbfb,days,stocks){
  if ((missing(days)|is.null(days)) & (missing(stocks)|is.null(stocks))){
    df<-dfdb %>%
      collect()
  }else if(missing(days)){
    stocks = as.character(stocks)
    df<-dfdb %>%
      filter(stockcode %in% stocks) %>%
      collect()
  }else if(missing(stocks)){
    if (nchar(as.character(days))<8){
      #代表是輸入回溯天數，而非確定日期
      date.pick = stock.date.pick(days)
    }else{
      date.pick = as.character(days)
    }
    print(date.pick)
    df<-dfdb %>%
      filter(date %in% date.pick) %>%
      collect()
  }else{
    if (nchar(as.character(days))<8){
      #代表是輸入回溯天數，而非確定日期
      date.pick = stock.date.pick(days)
    }else{
      date.pick = as.character(days)
    }
    print(date.pick)
    stocks = as.character(stocks)
    df<-dfdb %>%
      filter(date %in% date.pick) %>%
      filter(stockcode %in% stocks) %>%
      collect()
  }

}

#' 從dropbox下載deallist
#'
#' @return
#' @export
#'
#' @examples
#' stock.deallist.drop_to_db()
stock.deallist.drop_to_db <-function(){
  lists = drop.list.files(dir.drop.deallist) %>%
    pull(name)

  lists=setdiff(lists,list.files(dir.r.stock.deallist))
  if (length(lists)!=0){
    walk(lists,~drop_download(dir.drop.deallist %+% .x,
                              dir.r.stock.deallist %+% .x,overwrite = T))
    print("ic | deallist更新完成!!!")

  }else{
    print("ic | deallist無需更新!!!")
  }

}

#' Title 取得多個日期、多個股票的交易明細
#'
#' @param dates
#' @param stocks
#'
#' @return
#' @export
#'
#' @examples
#' stock.deallist.twse.get(c(20220110,20220111),2330)
stock.deallist.twse.get<-function(dates,stocks){
  print(dates)
  file<-list.files(dir.r.stock.deallist) %>%
    grep(as.character(dates),.,value=TRUE) %>%
    grep("twse",.,value=TRUE)

  char<-read_lines(dir.r.stock.deallist %+% file,skip=1)

  #filter stocks
  char.filter<-char %>%
    grep('"id"":"' %+% '"' %+% as.character(stocks) %+% '"' %+%'"',.,value=T)

  raw.json<-char.filter %>%
    gsub('""','"',.) %>%
    gsub('^[^\\{]*\\{', '{', .)
  json<-jsonlite::fromJSON(sub('^(^\\{)*\\{', '{', raw.json))


  df<-json$tick %>%
    mutate(stockcode = json$id,
           stockname = json$mem$name,.before=t) %>%
    mutate(time = ISOdatetime(year    = as.numeric(str_sub(t,1,4)),
                              month   = as.numeric(str_sub(t,5,6)),
                              day     = as.numeric(str_sub(t,7,8)),
                              hour    = as.numeric(str_sub(t,9,10)),
                              min     = as.numeric(str_sub(t,11,12)),
                              sec     = 0),.before=t)
  return(df)
}

stock.yahoo.get.twii_and_dji<-function(start.date,close.date){
  #前後皆不包含
  #start.date = 20190630
  #close.date = 20200701
  start.date.posixct = as.character(as.integer(as.POSIXct(as.Date(start.date))))
  close.date.posixct = as.character(as.integer(as.POSIXct(as.Date(close.date))))

  query.twii = paste0("https://query1.finance.yahoo.com/v7/finance/download/%5E","TWII","?period1=",start.date.posixct,
                      "&period2=",close.date.posixct,"&interval=1d&events=history")


  query.dji = paste0("https://query1.finance.yahoo.com/v7/finance/download/%5E","DJI","?period1=",start.date.posixct,
                     "&period2=",close.date.posixct,"&interval=1d&events=history")


  download.file(query.twii, destfile = paste0("D:\\R_notebook\\Stock\\price\\","^TWII",".csv"), method="curl", extra="-k")
  download.file(query.dji , destfile = paste0("D:\\R_notebook\\Stock\\price\\","^DJI" ,".csv"), method="curl", extra="-k")

  df.stock.yahoo.twii <<- read.csv("D://R_notebook//Stock//price//^TWII.csv")
  df.stock.yahoo.dowj <<- read.csv("D://R_notebook//Stock//price//^DJI.csv")



}

stock.financialstatement.link<-function(stocks){
  #stock.financialstatement(c(2330,2317,2880))
  url.fs = "https://mops.twse.com.tw/mops/web/t203sb01?encodeURIComponent=1&step=1&firstin=1&off=1&keyword4=&code1=&TYPEK2=&checkbtn=&queryName=co_id&inpuType=co_id&TYPEK=all&co_id="
  df = map_df(stocks,~data.frame(stock = .x,link= paste0(url.fs,as.character(.x))))
  kbl.display_with_hyperlink(df)
}


stock.tool.finstatement.link<-function(stockcode,year,quarter){
  #直接複製該檔股票之證交所財報連結至剪貼簿。
  #stock.tool.finstatement.link
  stockcode = as.character(stockcode)
  code      = stockcode
  year      = as.character(year)
  quarter   = as.character(quarter)

  if (isMac()) {
    path = "/Users/HaHaChang/R_notebook/Stock/Hash.TWSE各公司財報網站分類字串.csv"
  } else{
    path = "D:\\R_notebook\\Stock\\Hash.TWSE各公司財報網站分類字串.csv"
  }

  df = read_csv(path)

  webstring<-df %>%
    filter(stockcode == code,yq==(year %+% "Q" %+% quarter)) %>%
    pull(text)


  link=glue::glue("https://mops.twse.com.tw/server-java/t164sb01?step={quarter}&SYEAR={year}&file_name={webstring}-{stockcode}-{year}Q{quarter}.html")
  clipr::write_clip(link)
  return(link)
}

#' 透過python去抓取單檔股票的財務報表，資產負債表、損益表與現金流量表
#'
#' 網站:https://mops.twse.com.tw/server-java/t164sb01?step=2&SYEAR=2021&file_name=tifrs-fr1-m1-ci-cr-2330-2021Q2.html
#' @param stockcode
#' 數字格式與字串格式皆可
#'
#' @return
#'
#' @export
#' 直接輸出，df.stockcode的後綴型式的dataframe
#' @examples
#' py.stock.finstatement.parse(2317)
py.stock.finstatement.parse<-function(stock,year,quarter){
  #library(reticulate,verbose = T)
  py_run_file("parse_TWSE_FinancialStatement.py")
  year = as.character(year)
  quarter = as.character(quarter)
  print(year %+% "/" %+% "Q" %+% quarter)
  #py_run_string(glue::glue("res=(soup({stock},{year},{quarter},'tifrs-fr1-m1-ci-cr'))"))
  py_run_string(glue::glue("res=(soup({stock},{year},{quarter}))"))
  Sys.sleep(runif(1,2,3))
  # py_run_string("res =(soup("  %+% stock
  #               %+% ","
  #               %+% year
  #               %+% ","
  #               %+% quarter
  #               %+% ","
  #               %+% as.character(category)
  #               %+%"))" )
  py_run_string("df_pl = pl(res)")
  py_run_string("df_bs = bs(res)")
  py_run_string("df_cf = cf(res)")
  py_run_string("df = df_pl.append(df_bs).append(df_cf).reset_index()")
  df=py$df

  #df = list("pl"=py$df_pl,
  #          "bs"=py$df_bs,
  #          "cf"=py$df_cf)
  return(df)
}


#' 迴圈版的py.stock.finstatement.parse
#'
#' @param stockcode,all=T為所有year和quarter,posfix為df的後綴為股票代碼
#'
#' @return
#' @export
#'
#' @examples
loops.yq.py.stock.finstatement.parse<-function(stockcode,all=F,posfix=F){
  possible.f = possibly(.f = py.stock.finstatement.parse, otherwise = NULL)
  #safe.f = safely(.f = py.stock.finstatement.parse) #實驗性
  # maps = purrr.loops2map(2019:year(today()),1:4)
  #

  # 以上是實驗性
  # df <- map2_dfr(maps$rows, #loops_year
  #                maps$cols, #loops_quarter
  #                ~py.twse.finstatement.parse(stockcode,.x,.y)) %>%
  #   select(-index) %>%
  #   dplyr::rename(date=variable) %>%
  #   mutate(value=as.numeric(value)/10^5)

  #
  if (all==T){
    df <- map2_dfr(c(2021,2021,2021,2020,2020,2020,2020,2019,2019,2019,2019),
                   c(1,2,3,1,2,3,4,1,2,3,4),
                   ~possible.f(stockcode,.x,.y))
  }else{
    df <- map2_dfr(c(2021),
                   c(4),
                   ~possible.f(stockcode,.x,.y))
  }



  if (nrow(df) == 0) {
    print("Null")
    return(df)
  }else{
    df %<>%
      select(-index) %>%
      dplyr::rename(date=variable) %>%
      mutate(value=as.numeric(value)/10^5)

    if (posfix==T){
      assign("df." %+% stockcode,df,envir= parent.frame())
    }else{
      return(df)
    }

  }

  #' 網路取資料並儲存到db，loop(stockcode+year+quarter)
  #'
  #' @return
  #' @export 資料庫路徑:dir.r.stock,'stock.finstatement.db'
  #'
  #' @examples
  loops.syq.to_db.py.stock.finstatement.parse<-function(){
    # loops
    loop_stocks<-read_csv(dir.r.stock %+% "Hash.TWSE各公司財報網站分類字串.csv") %>%
      filter(yq=="2021Q4") %>%
      pull(stockcode) %>%
      unique()


    con <<- DBI::dbConnect(RSQLite::SQLite(), dbname = paste0(dir.r.stock,'stock.finstatement.db'))
    # 第一次重建資料庫時才會用到。
    # df <- dplyr::tbl(con, "finstatement")
    # occupy<-df %>%
    #   pull(stockcode) %>%
    #   unique()
    #
    # loop_stocks = setdiff(loop_stocks,occupy)
    # rm(df)


    list.error <- read.csv(dir.r.stock %+% "loop_error.stock.finstatement.csv") %>%
      pull(x)

    loop_stocks = setdiff(loop_stocks,list.error)

    # main
    for (stockcode in loop_stocks){
      print(stockcode %+%" : " %+% stock.tool.code2name(stockcode))
      df = loops.yq.py.stock.finstatement.parse(stockcode,all=F,posfix = F)
      if (nrow(df)==0){
        list.error = c(list.error,stockcode)
      }else{
        if (file.exists(paste0(dir.r.stock,'stock.finstatement.db'))){
          dbWriteTable(con, value = df, name="finstatement",append=TRUE )
        }else{
          dbWriteTable(con, value = df ,name="finstatement")
        }
      }

      Sys.sleep(runif(1,5,10))
      tool.clear_console()
      rm(df)
    }

    write.csv(list.error,file=dir.r.stock %+% "loop_error.stock.finstatement.csv" )

    dbDisconnect(con)
  }






}

#' 分析現金流量表
#'
#' @param df
#'
#' @return
#' res包含現金流量(營業、投資、自由、融資)、四象限圖、熱力圖。
#' @export
#'
#' @examples
stock.finstatement.analysis.cf<-function(df){

  hash.stock.finstatement.cf_state <<-
    tribble(
      ~state,   ~mean,
      "A"   ,"營運成長且財務穩健", #+ - - 最佳
      "B"   ,"營運成長且積極投資", #+ - +
      "C"   ,"營運成長且投資保守", #+ + -
      "D"   ,"營運成長且變現借錢", #+ + +

      "E"   ,"本業趨緩且投資保守", #- - - (平)
      "F"   ,"本業趨緩且投資積極", #- - +
      "G"   ,"本業轉差且投資保守", #- + -
      "H"   ,"本業轉差且變現借錢" #- + + 最差
    )


  tbl<-df %>%
    filter(sheet=="現金流量表") %>%

    select(stockcode,sheet,代號Code,會計項目,date,value) %>%
    filter(代號Code %in% c("AAAA","BBBB","CCCC")) %>%
    as.data.table() %>%
    mutate(value=as.numeric(value)) %>%
    select(-`會計項目`,-sheet) %>%
    dcast(stockcode+date~代號Code,sum) %>%
    dplyr::rename(營業=AAAA,
                    投資=BBBB,
                    理財=CCCC) %>%
    separate(date,into=c("year","month"),sep="/") %>%
    mutate(year=as.numeric(year),
           month=as.numeric(month)) %>%
    arrange(desc(year),desc(month)) %>%
    mutate(state = case_when((營業>0 & 投資<0 & 理財<0)~"A",
                             (營業>0 & 投資<0 & 理財>0)~"B",
                             (營業>0 & 投資>0 & 理財<0)~"C",
                             (營業>0 & 投資>0 & 理財>0)~"D",

                             (營業<0 & 投資<0 & 理財<0)~"E",
                             (營業<0 & 投資<0 & 理財>0)~"F",
                             (營業<0 & 投資>0 & 理財<0)~"G",
                             (營業<0 & 投資>0 & 理財>0)~"H"),.before=營業) %>%
    mutate(自由 = 營業+投資,.before=投資) %>%
    stock.tool.addname()

  plt.l<-tbl%>%
    mutate(time = time.month.lastday(year,month),.before=year) %>%
    ggplot(aes(理財,投資,color=time)) +
    geom_point()+
    geom_path()+
    geom_text_repel(aes(label=(as.character(year) %+% "/" %+% as.character(month))))+
    geom_vhline()+
    labs(subtitle = "新台幣億元")

  plt.r<-tbl %>%
    mutate(time = time.month.lastday(year,month),.before=year) %>%
    ggplot(aes(營業,自由,color=time)) +
    geom_point()+
    geom_path()+
    geom_text_repel(aes(label=(as.character(year) %+% "/" %+% as.character(month))))+
    geom_vhline()+
    geom_abline(intercept=0,slope=1,color=default_red)+
    labs(subtitle = "新台幣億元")

  plt = plt.l+plt.r+plot_layout(nrow=1)

  plt.heatmap<-hash.stock.finstatement.cf_state %>%
    merge(tbl %>%
            mutate(date = year %+% "/" %+% month,.before=year) %>%
            tidy.df.date.ym_to_ymd.lastday(col=date) %>%
            select(date,state),by="state",all.x=T) %>%
    select(-mean) %>%
    mutate(count = 1) %>%
    as.data.table() %>%
    dcast(date~state,sum,value.var = "count") %>%
    filter(!is.na(date)) %>%
    gather(variable,value,A:H) %>%
    dplyr::rename(state=variable) %>%
    merge(hash.stock.finstatement.cf_state,by="state",all.x=T) %>%
    mutate(state = state %+% ":" %+% mean) %>%
    plot.heatmap(x="date",y="state",z="value")+
    theme_xy_blank()+
    theme_legend_blank()+
    labs(title    =stock.tool.code2name(df %>% pull(stockcode) %>% .[1]),
         subtitle =df %>% pull(stockcode) %>% .[1])
  res = list("tbl"=tbl,"plt"=plt,"heatmap"=plt.heatmap)

  return(res)

}



#' 抓取TWSE整份財報。
#'
#' https://mops.twse.com.tw/mops/web/t203sb02
#' @param year
#' @param quarter
#'
#' @return
#'
#' @export
#' download file，預設是在「下載」資料夾。
#' @examples
#' stock.finstatement.parse.download_allfiles(2017,2)
stock.finstatement.parse.download_allfiles <-function(year,quarter){
  library(downloader)
  url = glue::glue("https://mops.twse.com.tw/server-java/FileDownLoad?step=9&functionName=show_file2&fileName=tifrs-{year}Q{quarter}.zip&filePath=/ifrs/{year}/")
  #download.file(url = url,
  #              destfile = dir.download %+%  glue::glue("tifrs-{year}Q{quarter}.zip"),
  #              method="libcurl")

  download(url, destfile = dir.download %+%  glue::glue("tifrs-{year}Q{quarter}.zip"), mode = "wb")
}




#' 抓取TWSE網站中全部財務報表至本機端後，去清整出「類別」(tifrs-fr1-m1-ins-ir)清單以加入爬蟲網址
#'
#' "https://mops.twse.com.tw/server-java/t164sb01?step=3&SYEAR=2021&file_name=tifrs-fr1-m1-ins-ir-2823-2021Q3.html"#中壽
#'"https://mops.twse.com.tw/server-java/t164sb01?step=3&SYEAR=2021&file_name=tifrs-fr1-m1-fh-cr-2882-2021Q3.html"#國泰金
#'"https://mops.twse.com.tw/server-java/t164sb01?step=3&SYEAR=2021&file_name=tifrs-fr1-m1-ins-cr-5846-2021Q3.html"#國泰人壽
#' @param year
#' @param quarter
#'
#' @return
#' @export
#'
#' @examples
#' res = stock.finstatement.tidy.download_allfiles(2021,3)
#' 需在硬碟裡已有下載資料夾。
stock.finstatement.tidy.download_allfiles<-function(year,quarter){
  path = dir.download %+% glue::glue("tifrs-{year}Q{quarter}")

  if (dir.exists(path)){
    res<-list.files(path) %>%
      data.frame() %>%
      `colnames<-`("file") %>%
      tidy.df.col.seperate_to_unlimit_col(col=file,sep="-") %>%
      dplyr::rename(stockcode=X6) %>%
      mutate(text = paste(X1,X2,X3,X4,X5,sep="-")) %>%
      mutate(X7 = str_remove_all(X7,".html")) %>%
      mutate(X7 = str_remove_all(X7,".xml")) %>%
      select(yq = X7,stockcode,text )
  }else{
    print(glue::glue("{year}Q{quarter} | 資料夾不存在!!!"))
    res=data.frame()
  }
  return(res)
}

loops.stock.finstatement.tidy.download_allfiles<-function(){
  res = data.frame()
  for (year in 2013:year(today())){
    for (quarter in 1:4){
      print(glue::glue("{year}Q{quarter}"))
      df = stock.finstatement.tidy.download_allfiles(year,quarter)
      res = bind_rows(res,df)
    }
  }
  res %>%
    write.csv(file = dir.r.stock %+% "Hash.TWSE各公司財報網站分類字串.csv", row.names = FALSE)
}

#' 從本地端資料端取得資料
#'
#' @param stockcode
#'
#' @return
#' @export
#'
#' @examples
#' stockcode=2317
#' res = stock.db.finstatement.get(stockcode)
stock.db.finstatement.get<-function(stockcode){
  con <<- DBI::dbConnect(RSQLite::SQLite(), dbname = paste0(dir.r.stock,'stock.finstatement.db'))
  dfdb <<- dplyr::tbl(con, "finstatement")
  res <- dbSendQuery(con, glue::glue("SELECT * FROM finstatement WHERE stockcode = {stockcode}"))
  df<-dbFetch(res) %>%
    mutate(會計項目 = str_extract(`會計項目Accounting Title`,"([\u4e00-\u9fa5_\\s]*)")
               ,.before=`會計項目Accounting Title`) %>%
    select(stockcode,sheet,代號Code,會計項目,date,value)
  dbClearResult(res)

  dbDisconnect(con)
  return(df)
}



#' 清洗產出跨期間比較之現金流量表。
#'
#' @param res 來自res = stock.db.finstatement.get(stockcode)
#' @param code
#'
#' @return
#' @export
#'
#' @examples
stock.finstatement.report.cf<-function(res,code){
  res %>%
    filter(sheet=="現金流量表") %>%
    distinct() %>%
    as.data.table() %>%
    separate(date,c("year","month"),sep="/") %>%
    mutate(quarter = case_when(month==3 ~"Q1",
                               month==6 ~"Q2",
                               month==9 ~"Q3",
                               month==12~"Q4")) %>%
    mutate(Code = str_sub(代號Code,1,3),.before=代號Code) %>%
    filter(Code!="") %>%
    #filter(year>=2020) %>%
    dcast(stockcode+代號Code+會計項目~year+quarter,sum,value.var="count") %>%
    arrange(代號Code)
}


#' 清洗產出跨期間比較之綜合損益表
#'
#' @param res 來自res = stock.db.finstatement.get(stockcode)
#' @param code 其實就是stockcode
#'
#' @return
#' @export
#'
#' @examples
stock.finstatement.report.pl<-function(res,code=NULL){
  if (is.null(code)){
    code<-res %>%
      pu(stockcode)
  }

  raw.pl<-res %>%
    filter(stockcode==as.character(code)) %>%
    filter(sheet=="綜合損益表") %>%
    distinct() %>% #因為會有去年同期的重疊。
    as.data.table() %>%
    separate(date,c("year","month"),sep="/") %>%
    mutate(quarter = case_when(month==3 ~"Q1",
                               month==6 ~"Q2",
                               month==9 ~"Q3",
                               month==12~"Q4")) %>%
    mutate(Code = str_sub(代號Code,1,3),.before=代號Code) %>%
    filter(Code!="") %>%
    #filter(year>=2020) %>%
    dcast(stockcode+代號Code+會計項目~year+quarter,sum) %>%
    arrange(代號Code)

  return(raw.pl)
}


#' 產生「營業毛利率」、「營業利益率」、「營業淨利率」、「營業純益率」
#'
#' @param stockcode
#'
#' @return
#' @export
#'
#' @examples
#' stock.finstatement.tidy.pl.margin(2330)
stock.finstatement.tidy.pl.margin <-function(stockcode){
  res<-stock.db.finstatement.get(stockcode) %>%
    stock.finstatement.report.pl(.,stockcode) %>%
    as.data.table() %>%
    mutate(代號Code = factor(代號Code,levels=(a %>% pull(代號Code)))) %>%
    melt(id.vars=c("stockcode","代號Code","會計項目")) %>%
    dcast(stockcode+variable~代號Code+會計項目,sum) %>%
    transmute(stockcode,
              y_q = variable,
              營業毛利率 =`5900_營業毛利`/`4000_　營業收入合計　`,
              營業利益率 =`6900_營業利益`/`4000_　營業收入合計　`,
              營業淨利率 =`7900_繼續營業單位稅前淨利`/`4000_　營業收入合計　`,
              營業純益率 =`8000_繼續營業單位本期淨利`/`4000_　營業收入合計　`) %>%
    arrange(desc(y_q))
  return(res)
}


#' Title 計算布林通道
#'
#' @param df
#' @param p
#' @param n
#' @param sd
#'
#' @return
#' @export
#'
#' @examples
stock.skill.bbands<-function(df,p,n,sd){
  require(RcppRoll)
  p = enquo(p)
  res<-df %>%
    mutate(mvag = roll_mean(!!p, n = n, align = "right", fill = NA),
           mvsd = roll_sd  (!!p, n = n, align = "right", fill = NA)) %>%
    mutate(up   = mvag+mvsd*sd*sqrt((n-1)/n),
           down = mvag-mvsd*sd*sqrt((n-1)/n)) %>%
    mutate(n = n) %>%
    select(time,!!p,n,up,mvag,down)
  return(res)
}

stock.info.twse.get<-function(){
  load(dir.r.stock %+% "stock.info.twse.Rdata")
}




##################### marco ##############################

plot.marco_economic.twii_and_dji<-function(start,end,past=F){

  stock.yahoo.get.twii_and_dji(start,end)

  plot.twii<-df.stock.yahoo.twii %>%
    mutate(year = year(Date)) %>%
    mutate(Close = as.numeric(Close)) %>%
    rename(Time=Date) %>%
    mutate(Time = as.Date(Time)) %>%
    #filter(year==2008|year==2009) %>%
    ggplot(aes(Time,Close))+
    geom_line(color=default_red)+
    scale_x_date_my()+
    #scale_x_date(date_labels=("%m-%Y"),date_breaks = "4 month")+
    #scale_x_date_m()+
    ggtitle("臺股加權指數")+
    #ggplot2::annotate("rect", xmin = as.Date("2020-01-01"), xmax = as.Date("2020-04-01"), ymin = -Inf, ymax = Inf,alpha = .1)+
    #ggplot2::annotate("rect", xmin = as.Date("2020-04-01"), xmax = as.Date("2020-07-01"), ymin = -Inf, ymax = Inf,alpha = .3)+
    #ggplot2::annotate("rect", xmin = as.Date("2020-07-01"), xmax = as.Date("2020-10-01"), ymin = -Inf, ymax = Inf,alpha = .5)+
    theme_xy_blank()+
    theme_tii_white()

  plot.dowj<-df.stock.yahoo.dowj %>%
    mutate(year = year(Date)) %>%
    mutate(Close = as.numeric(Close)) %>%
    rename(Time=Date) %>%
    mutate(Time = as.Date(Time)) %>%
    ggplot(aes(Time,Close))+
    geom_line(color=default_red)+
    scale_x_date_my()+
    #scale_x_date(date_labels=("%m-%Y"),date_breaks = "4 month")+
    ggtitle("Dow J")+
    #ggplot2::annotate("rect", xmin = as.Date("2020-01-01"), xmax = as.Date("2020-04-01"), ymin = -Inf, ymax = Inf,alpha = .1)+
    #ggplot2::annotate("rect", xmin = as.Date("2020-04-01"), xmax = as.Date("2020-07-01"), ymin = -Inf, ymax = Inf,alpha = .3)+
    #ggplot2::annotate("rect", xmin = as.Date("2020-07-01"), xmax = as.Date("2020-10-01"), ymin = -Inf, ymax = Inf,alpha = .5)+
    theme_xy_blank()+
    theme_tii_white()

  # if (past==F){
  #   plot.twii<-plot.twii +
  #     ggplot2::annotate("rect", xmin = as.Date(now.quarter.start), xmax = as.Date(now.quarter.end), ymin = -Inf, ymax = Inf,alpha = .2)
  #
  #   plot.dowj<-plot.dowj +
  #     ggplot2::annotate("rect", xmin = as.Date(now.quarter.start), xmax = as.Date(now.quarter.end), ymin = -Inf, ymax = Inf,alpha = .2)
  # }


  return(plot = list("twii" = plot.twii,
                     "dowj" = plot.dowj))
}


plot.marco_economic.usdtwd<-function(start,end,past=F){
  USDTWD<<-read_excel(dir.r.in.fx %+% "tw_fx_usdtwd.xlsx") %>%
    dplyr::rename(Time=date,USDTWD=usdtwd)

  plot.usdtwd<-USDTWD %>%
    filter(Time>=as.Date(start),Time<=as.Date(end)) %>%
    mutate(Time=as.Date(Time)) %>%
    ggplot(aes(Time,USDTWD))+
    geom_line(color=default_blue)+
    scale_x_date_my()+
    theme_xy_blank()+
    labs(title="USDTWD")+
    theme_tii_white()

  if (past==F){
    plot.usdtwd<-plot.usdtwd+
      ggplot2::annotate("rect", xmin = now.quarter.start, xmax = now.quarter.end, ymin = 27.5, ymax = 28.75,
                        alpha = .2)
  }
  return(plot.usdtwd)
}



parse.marco_economic.bond<-function(start,end){
  #bond.us <<- read_excel(dir.py.bond.us %+% "us_bond_rate.xlsx")
  bond.us <<- read_excel(dir.r.in.bond %+% "us_bond_rate.xlsx")
  bond.tw <<- read_excel(dir.r.in.bond %+% "tw_bond_rate.xlsx")

}



plot.marco_economic.bond<-function(start,end,past=F){
  bond.us <<- read_excel(dir.r.in.bond %+% "us_bond_rate.xlsx")
  bond.tw <<- read_excel(dir.r.in.bond %+% "tw_bond_rate.xlsx")

  plot.us.bond10yr<-bond.us %>%
    select(time,`10 yr`) %>%
    as.data.table() %>%
    melt(id.vars=c("time")) %>%
    filter(variable == "10 yr") %>%
    mutate(time = as.Date(time),
           value = value/100) %>%
    filter(time>=as.Date(start),time<=as.Date(end)) %>%
    ggplot(aes(time,value))+
    geom_line(color=default_green)+
    theme_xy_blank()+
    scale_y_percentage()+
    scale_x_date_my()+
    labs(title="美國10年期公債利率(%)")+
    theme_tii_white()

  plot.tw.bond10yr<-bond.tw %>%
    filter(tenor ==10) %>%
    filter(time>=as.Date(start),time<=as.Date(end)) %>%
    mutate(time=as.Date(time)) %>%
    ggplot(aes(time,cubic))+
    geom_line(color=default_green)+
    theme_xy_blank()+
    scale_y_percentage()+
    scale_x_date_my()+
    labs(title="台灣10年期公債利率(%)")+
    theme_tii_white()



  if(past==F){
    plot.us.bond10yr <- plot.us.bond10yr +
      ggplot2::annotate("rect", xmin  = now.quarter.start,
                        xmax  = now.quarter.end,
                        ymin  = -Inf,
                        ymax  = Inf,
                        alpha = .2)
    plot.tw.bond10yr <- plot.tw.bond10yr +
      ggplot2::annotate("rect", xmin  = now.quarter.start,
                        xmax  = now.quarter.end,
                        ymin  = -Inf,
                        ymax  = Inf,
                        alpha = .2)
  }

  return(plot = list("bond.tw" = plot.tw.bond10yr,
                     "bond.us" = plot.us.bond10yr))

}

plot.marco_economic<-function(start=now.year.start,end=as.Date(now()),up_to_down=T,past=F){

  p1 = plot.marco_economic.twii_and_dji(start,end,past)
  p2 = plot.marco_economic.usdtwd(start,end,past)
  p3 = plot.marco_economic.bond(start,end,past)

  if (up_to_down==T){
    pp  = p1$twii + p1$dowj + p3$bond.us + p3$bond.tw + p2 +plot_layout(ncol=1)
  }else{
    pp  = p1$twii + p1$dowj + p3$bond.us + p3$bond.tw + p2 +plot_layout(nrow=1)
  }

  return(pp)
}


##################### gt ##############################
gt.data_color<-function(gt,col){
  gt %>%
    data_color(
      columns = col,
      colors = scales::col_numeric(
        # Using a function from paletteer to generate a vector of colors
        # Note that you need to wrap paletteer_d outputs in as.character()
        palette = as.character(paletteer::paletteer_d("ggsci::red_material", n = 5)),
        # Domain is from scales::col_numeric
        # Domain = The possible values that can be mapped
        # We don't HAVE to set a range for this since
        # we're using it inside data_color()
        domain = NULL
      )
    )
}


tidy.news.MSlink_to_Mac<-function(df.filter.news){
  if (date.num>=20161006){
    news.download.subtype=".pdf"

  }else{
    news.download.subtype=".tif"
  }

  pdf = "file:///Users/HaHaChang/Downloads/11005" %+% news.download.subtype
  df <- tibble(
    country = c("UK", "US"),
    name = c("BBC", "CNN"),
    link = c(pdf, pdf))
}


gt.display_with_hyperlink<-function(df){
  #使gt可以建立hyperlink
  df %>%
    mutate(
      link = map(link, ~ htmltools::a(href = .x, "link")),
      link = map(link, ~ gt::html(as.character(.x)))) %>%
    gt()
}

##################### kable    ##############################
kbl.display_with_hyperlink<-function(dt){
  dt %>%
    mutate(link = cell_spec(row.names(.), "html", link = link)) %>%
    kable(format = "html", escape = FALSE,table.attr = "style = \"color: black;\"") %>%
    kable_styling(bootstrap_options = c("hover", "condensed"))
}

kbl.display_with_hyperlink.news<-function(dt){
  dt %>%
    mutate(link = cell_spec(row.names(.), "html", link = link)) %>%
    select(-downloadlink,-download) %>%
    kable(format = "html", escape = FALSE,table.attr = "style = \"color: black;\"") %>%
    kable_styling(bootstrap_options = c("hover", "condensed"))
}

##################### dygraphs ##############################

plot.dygraph.add_news<-function(p.dygraph,df.filter.news){
  #將資料加入
  for (i in 1:nrow(df.filter.news)){
    p.dygraph %<>%
      dyAnnotation(as.character(df.filter.news[i,1]),
                   text = i,
                   tooltip = df.filter.news$abstract[i])
  }

  return(p.dygraph)

}

plot.dygraph.add_contron_tools<-function(p.dygraph){
  p.dygraph%<>%
    #dyOptions(labelsUTC = TRUE, fillGraph=TRUE, fillAlpha=0.1, drawGrid = FALSE, colors="#D8AE5A") %>%
    dyRangeSelector(dateWindow = c(
      strptime(now()-years(1), "%Y-%m-%d") %>% as.character(),
      strptime(now()         , "%Y-%m-%d") %>% as.character())
    ) %>%
    dyRoller(rollPeriod = 5) %>%
    dyCrosshair(direction = "vertical") %>%
    dyHighlight(highlightCircleSize = 5,
                highlightSeriesBackgroundAlpha = 0.2,
                hideOnMouseOut = T,
                highlightSeriesOpts = list(strokeWidth = 3)) %>%
    dyLegend(show = "always", hideOnMouseOut = FALSE)

  return(p.dygraph)
}



##################### General ##############################
time.month.lastday <-function(year,month){
  Time = (as.Date(as.character(paste(as.numeric(year)+ifelse(month==12,1,0),ifelse(month==12,1,month+1),1,sep="-")))-1)
  return(Time)
}

time.month.firstday <- function(year,month){
  Time = (as.Date(
    as.character(
      paste(as.character(year),as.character(month),"01",sep="-"))))
  return(Time)
}


grep.col2grep<-function(col){
  list<-col %>%
    paste(collapse="|")

  return(list)
}


path.correct <- function(path = "clipboard") {
  #處理資料夾路徑問題
  y <- if (path == "clipboard") {
    readClipboard()
  } else {
    cat("Please enter the path:\n\n")
    readline()
  }
  x <- chartr("\\", "/", y)
  writeClipboard(x)
  return(x)
}


nrow.10k <- function(...){
  nrow(...)/10000
}



ic<-function(message){
  #more beautiful print
  fun = (sys.calls())[[1]]
  #print("ic| " %+% fun %+% " : " %+% as.character(message))
  sprintf("%-40s", "ic |= " %+% fun %+% " : " %+% as.character(message) %+% " =|" )
}


df.show_missings <- function(df) {
  n <- sum(is.na(df))
  cat("Missing values: ", n, "\n", sep = "")

  invisible(df)
}


commas <- function(...) stringr::str_c(..., collapse = ", ")
pipes  <- function(...) stringr::str_c(..., collapse = "|")



df2grep <- function(df,col){
  #將dtaframe的某個col抽出作為regular expression的關鍵字。
  col = enquo(col)
  grep<-df %>%
    pull(keyword) %>%
    as.character() %>%
    paste(collapse="|")
  return(grep)
}



news2line <- function(news,prompt){
  msg<-news %>%
    filter(date==date(now())) %>%
    mutate(url ="http://114.34.213.38/DMSA/File/" %+% str_sub(download,1,8) %+% "/" %+% download %+% ".pdf" ) %>%
    select(title,url) %>%
    t() %>%
    as.vector() %>%
    paste(.,collapse =  "\n")

  if (missing(prompt)){
    line(paste(now(),"\n",msg,collapse = "\n"))
  }else{
    line(paste(now(),"\n",prompt,"\n",msg,collapse = "\n"))
  }

}



`%+%` <- function(...){
  #snippet::pst
  paste0(...)
}

#head()+gt()
hdgt<-function(dataset){
  dataset %>%
    head() %>%
    gt()

}

kp <- function(dataset){
  dataset %>%
    kable() %>%
    kable_paper("hover",full_width=F,html_font = "Microsoft JhengHei")

}


ed <-function(file.name){
  file.edit(file.name)

}




source_rmd <- function(rmd_file){
  knitr::knit(rmd_file,quiet=T)
}




haha.progressbar <- function(end,i){
  cat('\r',
      "Program::",
      i/end*100,
      '% |',
      rep('=', i / (end/50)),
      ifelse(i == end, '|\n',  '>'), sep = '')
  Sys.sleep(.01)
}

pu <-function(df,col){
  #pull and unique
  col = enquo(col)
  df %>%
    pull(!!col) %>%
    unique()
}

pwd <-function(){
  getwd()

}

ls<-function(){
  list.files()
}


lsf<-function(...){
  list.files(...,full.names=T)
}


cd <-function(path){
  setwd(path)


}

haha.string.replace <- function(filename, name.old, name.new, filename.new){
  x <- readLines(filename)
  y <- gsub(name.old,name.new,x)

  if (missing(filename.new)){
    cat(y, file=filename, sep="\n")
  }else{
    cat(y, file=filename.new, sep="\n")
  }


}




##################### Function ##############################

as.vector.df <-function(datasets){
  as.character(unlist(datasets))
}

## add.date
add.date <-function(x){
  x %<>%
    mutate(Date=paste0((Year-1911)*100+Month))

}

## add.nickname
add.nickname <- function(datasets){

  datasets %<>%
    merge(Hash.Company.Unique %>%
            filter(!Name %in% c("大型公司","中型公司","小型公司")) %>%
            mutate(nickname = LETTERS[1:nrow(.)]) %>%
            select(Name,nickname),by="Name"
    )

}

add.name.frame<-function(df){
  Hash.Company.alive<-Hash.Company %>%
    filter(Exist==1) %>%
    select(Name)

  df.new<-df %>%
    merge(Hash.Company.alive,by=("Name"),all.y = T) %>%
    add.name.order() %>%
    arrange(Name)
  return(df.new)
}


add.name.order<-function(x){
  x %<>%
    mutate(Name = factor(Name,levels=c("國泰","南山","富邦","新光","中國","台灣",
                                       "三商美邦","全球","全球非區隔","全球區隔","中華郵政",
                                       "臺銀","遠雄","安聯",
                                       "宏泰","法國巴黎","元大",
                                       "保誠","台新","合庫",
                                       "安達","友邦","第一金",
                                       "康健","業界"))) %>%
    arrange(Name)

  return(x)
}


add.size.order<-function(x){
  x %<>%
    mutate(Size = factor(Size,levels=c("大型公司","中型公司","小型公司")))

  return(x)
}


## add.order
add.order <- function(x){

  # if (isMac()){
  #   path = "./Data_Insurance/"
  # } else{
  #   path = ".\\Data_Insurance\\"
  # }
  path = dir.r.raw

  load(paste(path,"Hash_Company_Unique.Rdata",sep=""))

  x %<>%
    merge(select(Hash.Company.Unique,Name,Order),by="Name") %>%
    data.frame()

}


add.order.all <- function(x){
  x %<>%
    mutate(Order = 23) %>%
    mutate(Name="業界")

}


add.order.size <- function(x){
  x %<>%
    mutate(Order = case_when(Size=="large"~8.5,
                             Size=="medium"~15.5,
                             Size=="small"~22.5)) %>%
    rename(Name=Size)

}

add.order.size.chinese <- function(x){
  x %<>%
    mutate(Order = case_when(Size=="大型公司"~8.5,
                             Size=="中型公司"~15.5,
                             Size=="小型公司"~22.5)) %>%
    rename(Name=Size)

}

## add.name
add.name <- function(x){

  # if (isMac()){
  #   path = "./Data_Insurance/"
  # } else{
  #   path = ".\\Data_Insurance\\"
  # }
  path = dir.r.raw

  load(paste(path,"Hash_Company.Rdata",sep=""))

  x %<>%
    merge(select(Hash.Company,CorpCode,Name) %>%
            mutate(CorpCode = as.character(CorpCode)),by="CorpCode") %>%
    select(-CorpCode) %>%
    data.frame()

}

add.name.rbc <- function(x){
  # if (isMac()){
  #   path = "./Data_Insurance/"
  # } else{
  #   path = ".\\Data_Insurance\\"
  # }

  path = dir.r.raw

  load(paste(path,"Hash_Company_RBC.Rdata",sep=""))

  x %<>%
    merge(select(Hash.Company.RBC,companyID,shortname),by="companyID") %>%
    select(-companyID) %>%
    rename(Name=shortname) %>%
    data.frame()
}

add.producttype.order <-function(x){
  x %<>%
    #mutate(ProductType = factor(ProductType,levels = c("利變型年金","利變型壽險","投資型年金","投資型壽險","健康險","傳統型年金","傳統型壽險","傷害險","萬能壽險")))
    mutate(ProductType = factor(ProductType,levels = c("利變型年金","利變型壽險","投資型年金","投資型壽險","傳統型年金","傳統型壽險","健康險","傷害險","萬能壽險")))

  return(x)
}


## add.name
add.order.fv <- function(x){

  if (isMac()){
    path = "./Data_Insurance/"
  } else{
    path = ".\\Data_Insurance\\"
  }

  load(paste(path,"Hash_Company_FV.Rdata",sep=""))

  x %<>%
    merge(select(Hash.Company.FV %>% filter(Order!=99),Name,Order),by="Name") %>%
    data.frame()

}

add.name.english <- function(x){

  path = dir.r.raw

  load(paste(path,"Hash_Company_Unique.Rdata",sep=""))

  x %<>%
    merge(select(Hash.Company.Unique,Name,Name_English),by="Name") %>%
    data.frame()


}

## add.size
add.size <- function(x){

  # if (isMac()){
  #   path = "./Data_Insurance/"
  # } else{
  #   path = ".\\Data_Insurance\\"
  # }

  path = dir.r.raw

  load(paste(path,"Hash_Company_Unique.Rdata",sep=""))

  x %<>%
    merge(select(Hash.Company.Unique,Name,Size),by="Name") %>%
    data.frame()

}


size_english.to.size_chinese <- function(x){
  x %<>%
    mutate(Size = case_when(Size=="large" ~"大型公司",
                            Size=="medium"~"中型公司",
                            Size=="small" ~"小型公司"))
  return(x)
}


arrange.order<- function(datasets){
  datasets %<>%
    arrange(Order)

}


arrange.size<- function(datasets){
  datasets %<>%
    arrange(match(Size,c("large","medium","small")))

}

arrange.size.chinese <- function(datasets){
  datasets %<>%
    mutate(Size = factor(Size,levels=c("大型公司","中型公司","小型公司"))) %>%
    arrange(Size)

  return(datasets)
}



arrange.com.colname <- function(datasets){

  datasets %<>%
    data.frame()
  name = colnames(datasets)

  sort.name = c("國泰",
                "南山",
                "富邦",
                "新光",
                "中國",
                "台灣",
                "三商美邦",
                "全球",
                "中華郵政",
                "臺銀",
                "遠雄",
                "安聯",
                "宏泰",
                "法國巴黎",
                "元大",
                "保誠",
                "台新",
                "合庫",
                "安達",
                "友邦",
                "第一金",
                "康健")


  datasets = datasets[, c(setdiff(name,sort.name),sort.name)]
  return(datasets)
}


arrange.rbc.com.colname <- function(datasets){

  name = colnames(datasets)

  sort.name = c("國泰",
                "原國泰",
                "原國寶幸福",
                "南山",
                "原南山",
                "原朝陽",
                "富邦",
                "新光",
                "中國",
                "台灣",
                "三商美邦",
                "全球",
                "原全球",
                "原國華",
                "中華郵政",
                "臺銀",
                "遠雄",
                "安聯",
                "宏泰",
                "法國巴黎",
                "元大",
                "保誠",
                "台新",
                "合庫",
                "安達",
                "友邦",
                "第一金",
                "康健")


  datasets[, c(setdiff(name,sort.name),sort.name)]

}

arrange.property.com.colname <- function(datasets){

  name = colnames(datasets)

  sort.name = c("臺灣產物",
                "兆豐產物",
                "富邦產物",
                "和泰產物",
                "泰安產物",
                "明台產物",
                "南山產物",
                "第一產物",
                "旺旺友聯產物",
                "新光產物",
                "華南產物",
                "國泰世紀產物",
                "新安東京海上產物",
                "台壽保產物",
                "裕利安宜產物",
                "美國國際產物",
                "科法斯產物",
                "安達產物",
                "亞洲產物",
                "安盛產物",
                "法國巴黎產物",
                "漁保社")


  datasets[, c(setdiff(name,sort.name),sort.name)]

}





## as.numeric.factor
as.numeric.factor <- function(x) {
  as.numeric(as.character.factor(x))
}

## big5 to utf8
## On Mac, Prem.Raw's colnames display garbled due to encoding.
big5_to_utf8 <- function(datasets){
  for (col in colnames(datasets)){
    Encoding(datasets[[col]]) <- "Big5"
  }

  temp = colnames(datasets)


  temp <- iconv(temp, "big5", "utf8")


  colnames(datasets)<-temp


}


utf8_to_big5 <- function(datasets){
  for (col in colnames(datasets)){
    Encoding(datasets[[col]]) <- "utf8"
  }

  temp = colnames(datasets)


  temp <- iconv(temp, "utf8", "big5")


  colnames(datasets)<-temp


}


#change data's fomrat from character to number
char2num = function(datasets){
  if("DataDate" %in% colnames(datasets)){
    datasets %<>%
      mutate(DataDate=as.numeric(DataDate))
  }

  if("Date" %in% colnames(datasets)){
    datasets %<>%
      mutate(Date=as.numeric(Date))
  }

  if("Year" %in% colnames(datasets)){
    datasets %<>%
      mutate(Year=as.numeric(Year))
  }

  if("Month" %in% colnames(datasets)){
    datasets %<>%
      mutate(Year=as.numeric(Month))
  }

  if("Order" %in% colnames(datasets)){
    datasets %<>%
      mutate(Order=as.numeric(Order))
  }
}




#Change Date Format
change_date = function(datasets){
  if("DataDate" %in% colnames(datasets)){
    datasets %>%
      tidyr::extract(DataDate,c("Year","Month"), "(^\\w{2,3})(\\w{2}+)$",remove=F) %>%
      mutate(Year = as.numeric(Year)+1911) %>%
      mutate(Month = as.numeric(Month))
  }else if("Date" %in% colnames(datasets)){
    datasets %>%
      tidyr::extract(Date,c("Year","Month"), "(^\\w{2,3})(\\w{2}+)$",remove=F) %>%
      mutate(Year = as.numeric(Year)+1911) %>%
      mutate(Month = as.numeric(Month))
  }
}

crawl.ptt.query <- function(keyword,collection){
  library(mongolite)

  if(missing(collection)){
    collection="Gossiping"
  }


  dmd<- mongo(collection =collection, db = 'ptt',url =   "mongodb://localhost:27017")

  query.keyword = paste0('{"title":{"$regex":','"', keyword,'"', ",",'"$options" : "i"}}')

  result <- dmd$find(query.keyword)

  crawl.ptt.query<-result %>%
    arrange(desc(score))

}

complete.name <-function(x){
  x %<>%
    merge(Hash.Company %>%
            filter(Exist==1) %>%
            select(Name),by="Name",all.y=T) %>%
    mutate_each(~ifelse(is.na(.),0,.),-Name)
  return(x)
}


complete.producttype <-function(x){
  df = data.frame(c("利變型年金","利變型壽險","投資型年金","投資型壽險","健康險","傳統型年金","傳統型壽險","傷害險","萬能壽險"))
  colnames(df) = "ProductType"
  x %<>%
    merge(df ,by="ProductType",all.y=T)
  #mutate_each(~ifelse(is.na(.),0,.),-Name)
  return(x)
}




comma2 <-function(input,digit){
  if(missing(digit)){
    digit=0
  }
  output = comma(round(input,digit))
}



data2map <- function(datasets,zoom){
  map <- get_map(location = 'Taiwan', zoom = zoom,language = "zh-TW", maptype = "roadmap")
  ggmap(map) +
    geom_point(aes(x = lon, y = lat), data = datasets)
}


df2grep <- function(df,col){
  col = enquo(col)
  grep<-df %>%
    pull(keyword) %>%
    as.character() %>%
    paste(collapse="|")
  return(grep)
}


ls <-function(path){
  list.files(path)
}

dirs <-function(path){
  list.dirs(path,recursive=F,full.names = F)
}


rename.remove.prefix <- function(datasets,prefix){
  datasets %<>%
    rename_at(.vars = vars(starts_with(prefix)),
              .funs = funs(sub(prefix, "", .)))

}

sleep.random<-function(wait.sec.start,wait.sec.end){

  sleeptime = runif(1,wait.sec.start,wait.sec.end)
  Sys.sleep(sleeptime)
}


show.comma <- function(datasets){

  datasets %<>%
    mutate_if(is.numeric,funs(comma2(.,0)))

}

show.percent <- function(datasets){

  datasets %<>%
    mutate_if(is.numeric,funs(percent(.,0)))

}


sum.row <- function(datasets){
  datasets %<>%
    mutate(total = rowSums(  select_if(., is.numeric) %>%
                               select_if(!str_detect(names(.), "Date")), na.rm = TRUE))
  return(datasets)
}

sum.col <- function(datasets){
  datasets %<>%
    bind_rows(summarise_all(., funs(if(is.numeric(.)) sum(.,na.rm=T) else "合計")))
  return(datasets)
}

sum.col.wd <- function(datasets){
  datasets %<>%
    bind_rows(summarise_all(., funs(if(is.numeric(.)) sum(.,na.rm=T) else "業界")))
  return(datasets)
}

##################### Plot ##############################
haha.plot.window.new <-function(){
  dev.new(width=9,height=6,noRStudioGD = TRUE,xpos=1540,ypos=0)
}



haha.plot.gif <-function(path,gif.name){
  library(gifski)
  png_files <- list.files(path, pattern = ".*png$", full.names = TRUE)
  gifski(png_files, gif_file = gif.name , width = 800, height = 600, delay = 0.1)
}








haha.plot.todevs <- function(...){
  #plot2devs("p1","p2","p3","p4","p5")
  graphics.off()
  loops =list(...) # THIS WILL BE A LIST STORING EVERYTHING:

  for (loop_plot in loops){
    loop.position = match(loop_plot,loops)
    dev.new(width=5,height=4,noRStudioGD = TRUE,xpos=1540+500*((loop.position-1)%%3),ypos=0+((loop.position-1)%/%3)*400)
    print(eval(parse(text=loop_plot)))

  }

  # Example of inbuilt function
}





#Function to shift x-axis to 0 adapted from link shown above
timeline2<-function(data,start,end){
  shift_axis <- function(p, xmin, xmax, y=0){
    g <- ggplotGrob(p)
    dummy <- data.frame(y=y)
    ax <- g[["grobs"]][g$layout$name == "axis-b"][[1]]
    p + annotation_custom(grid::grobTree(ax, vp = grid::viewport(y=1, height=sum(ax$height))),
                          ymax=y, ymin=y) +
      ggplot2::annotate("segment", y = 0, yend = 0, x = xmin, xend = xmax,
                        arrow = arrow(length = unit(0.1, "inches"))) +
      theme(axis.text.x = element_blank(),
            axis.ticks.x=element_blank())

  }

  #Tidy Data
  data%<>%
    arrange(start_date) %>%
    mutate(rank=1:nrow(.)) %>%
    mutate(upordown = ifelse(rank%%2==0,1,-1)) %>%
    mutate(line_height = (upordown*displ*rnorm(nrow(.)))) %>% #create different longer lollipop
    filter(start_date>=ymd(start),start_date<=ymd(end))


  #Conditionally set whether text will be above or below the point
  vjust = ifelse(data$displ > 0, -1, 1.5)


  #font size ON/OFF


  #plot
  p1 <- data %>%
    #filter(start_date>=ymd(start),start_date<=ymd(end)) %>%
    ggplot(aes(start_date, line_height,color=group)) +
    geom_lollipop(point.size = 1) +
    geom_text(aes(x = start_date, y = line_height, label = event), data = data,
              hjust = 0, vjust = vjust, size = 3) +
    theme(axis.title = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.line = element_blank(),
          axis.text.x = element_text(size = 8)) +
    expand_limits(x = c(ymd(start), ymd(end)), y = 1.2) +
    scale_x_date(breaks = scales::pretty_breaks(n = 9))+
    theme_legend_bottom()+
    theme_legend_title_blank()+
    theme_backgroud_white()


  shift_axis(p1, ymd(start), ymd(end))

}








##################### Output Data ##############################


line<-function(message,plot){

  library(lineNotify)
  line_token_info = "apfSt2nGRwzBuCuFWtH9qRCX8BGATHEyCiBAIQzAwup"
  Sys.setenv(LINE_API_TOKEN=line_token_info)


  if(missing(plot)){
    notify_msg(message)
  }else{

    notify_ggplot(message, plot = plot)
    notify_ggplot(message, plot = plot)
    notify_ggplot(message, plot = plot)


  }




}





#
write.csv3<-function(datasets,path,file.name){
  datasets %>%
    write.csv(.,paste0(path,file.name,".csv"))
}


ggsave2 <- function(path,file.name,width,height){

  ggsave(paste0(path, file.name ,".png"), width=width, height=height,dpi=100)

}

#' ym:民國年轉西元年, 並以"/"區分
#'
#' @param df
#' @param col
#'
#' @return
#' @export
#'
#' @examples
tidy.df.date.ym.ROC_to_CE<-function(df,col=NULL){
  col = enquo(col)

  df %<>%
    mutate(!!col:=as.character(!!col)) %>%
    mutate(!!col:=as.character(as.numeric(str_sub(!!col,1,3))+1911) %+% "/" %+%  str_sub(!!col,4,5))
  return(df)
}



#' ymd:民國年轉西元年, 並以"/"區分
#'
#' @param df
#' @param col
#'
#' @return
#' @export
#'
#' @examples
tidy.df.date.ymd.ROC_to_CE<-function(df,col=NULL){
  col = enquo(col)

  df %<>%
    mutate(!!col:=as.character(!!col)) %>%
    mutate(!!col:=as.character(as.numeric(str_sub(!!col,1,3))+1911)
           %+% "/"
           %+%  str_sub(!!col,4,5)
           %+% "/"
           %+%  str_sub(!!col,6,7)
    )
  return(df)
}





##################### Date ##############################
#' 日期格式ym轉成ymd，其中d是月份的最後一天
#'
#' @param df
#' @param col
#'
#' @return
#' @export
#'
#' @examples
tidy.df.date.ym_to_ymd.lastday<-function(df,col){
  col = enquo(col)
  df %<>%
    separate(!!col,into=c("year","month"),sep="/") %>%
    mutate(year=as.numeric(year),
           month=as.numeric(month)) %>%
    mutate(date = time.month.lastday(year,month),.before=year) %>%
    select(-year,-month)
  return(df)
}


tidy.df.date.ym_to_ymd.firstday<-function(df,col){
  col = enquo(col)
  df %<>%
    separate(!!col,into=c("year","month"),sep="/") %>%
    mutate(year=as.numeric(year),
           month=as.numeric(month)) %>%
    mutate(date = time.month.firstday(year,month),.before=year) %>%
    select(-year,-month)
  return(df)
}

#' 找尋data.frame裡每年的最後一天
#'
#' @param df
#' @param col
#'
#' @return
#' @export
#'
#' @examples
date.find.year_lastday<-function(df,col){
  col = enquo(col)
  lastday <- df %>%
    group_by(year(!!col)) %>%
    summarise(lastday  = max(!!col)) %>%
    pu(lastday)
  res<-df %>%
    filter(!!col %in% lastday)
  return(res)
}





#' 找尋data.frame裡每個月的最後一天
#'
#' @param df
#' @param col
#'
#' @return
#' @export
#'
#' @examples
date.find.month_lastday<-function(df,col){
  col = enquo(col)
  lastday <- df %>%
    dplyr::group_by(year(!!col),month(!!col)) %>%
    dplyr::summarise(lastday  = max(!!col)) %>%
    pu(lastday)

  res<-df %>%
    dplyr::filter(!!col %in% lastday)
  return(res)
}


#' 找尋data.frame裡每個月的第一天
#'
#' @param df
#' @param col
#'
#' @return
#' @export
#'
#' @examples
date.find.month_firstday<-function(df,col){
  col = enquo(col)
  lastday <- df %>%
    dplyr::group_by(year(!!col),month(!!col)) %>%
    dplyr::summarise(lastday  = min(!!col)) %>%
    pu(lastday)
  res<-df %>%
    dplyr::filter(!!col %in% lastday)
  return(res)
}

date.num_to_date<-function(num){
  num   = as.character(num)
  year  = as.character(str_sub(num,1,4))
  month = as.character(str_sub(num,5,6))
  day   = as.character(str_sub(num,7,8))

  return(year %+% "-" %+% month %+% "-" %+% day)
}


#' number transform to AC time format
#'
#' @param datasets
#' @param col
#'
#' @return
#' @export
#'
#' @examples
tidy.df.date.num2char <-function(datasets,col){
  col = enquo(col)
  datasets %<>%
    mutate(!!col := as.character(!!col)) %>%
    mutate(date = case_when(
      nchar(!!col)==8~str_sub(!!col,1,4) %+% "-" %+% str_sub(!!col,5,6) %+% "-" %+% str_sub(!!col,7,8), #西元:20220101
      nchar(!!col)==7~as.character(as.numeric(str_sub(!!col,1,3))+1911) %+% "-" %+% str_sub(!!col,4,5) %+% "-" %+% str_sub(!!col,6,7), #台灣: 1110101
      nchar(!!col)==6~str_sub(!!col,1,4) %+% "-" %+% str_sub(!!col,5,6),                                #西元:  202201
      nchar(!!col)==5~as.character(as.numeric(str_sub(!!col,1,3))+1911) %+% "-" %+% str_sub(!!col,4,5),                                #台灣:   11101
    )
    )
  return(datasets)
}


tidy.df.date.num_to_date<-function(df,col){
  col = enquo(col)
  res<-df %>%
    tidy.df.date.num2char(col = !!col) %>%
    mutate(date= as.Date(date)) %>%
    drop_na()
  return(res)
}


tidy.df.date.num2date <-function(datasets,col){
  col=enquo(col)
  res<-datasets %>%
    tidy.df.date.num2char(col=!!col) %>%
    mutate(date=as.Date(date))
  return(res)
}


#' count digits of num like "nchar"
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
nnum<-function(x){
  return(nchar( trunc( abs(x) ) ))
}



date.num2char <-function(datasets,col){
  col = enquo(col)
  datasets %<>%
    mutate(date = ifelse(length(str_sub(!!col,1,4) %+% "-" %+% str_sub(!!col,5,6) %+% "-" %+% str_sub(!!col,7,8))))
}




datechange.char_no_hypen2date <- function(date_char_no_hypen){

  date = as.Date(ISOdate(substr(date_char_no_hypen,1,4),
                         substr(date_char_no_hypen,5,6),
                         substr(date_char_no_hypen,7,8)))
}


date2time.excel = function(datasets){
  datasets %<>%
    mutate(date = as.Date(date, origin = "1899-12-30"))
}

#date2actime其實用ymd可以取代
date2actime = function(datasets,colname){
  if (missing(colname)) {

    datasets %<>%
      mutate(year = as.numeric(substr(date,1,4))) %>%
      mutate(month = as.numeric(substr(date,5,6))) %>%
      mutate(day   = as.numeric(substr(date,7,8)))%>%
      mutate(time  = ISOdate(year,month,day) ) %>%
      select(time,year,month,day,everything())

  }else{
    colname = enquo(colname)
    datasets %<>%
      mutate(year = as.numeric(substr(!!colname,1,4))) %>%
      mutate_(month = as.numeric(substr(!!colname,5,6))) %>%
      mutate_(day   = as.numeric(substr(!!colname,7,8)))%>%
      mutate(time  = ISOdate(year,month,day) ) %>%
      select(time,year,month,day,everything())

  }
}

#應更名twdate.ym2ac.ym
date2ac = function(datasets){
  if("DataDate" %in% colnames(datasets)) {

    datasets %<>%
      mutate(year = as.numeric(substr(DataDate,1,3))+1911) %>%
      mutate(month = as.numeric(substr(DataDate,4,5))) %>%
      mutate(date.ac = year*100+month  ) %>%
      select(date.ac,everything(),-year,-month,-DataDate)

  }else if("Date" %in% colnames(datasets)){

    datasets %<>%
      mutate(year = as.numeric(substr(Date,1,3))+1911) %>%
      mutate(month = as.numeric(substr(Date,4,5))) %>%
      mutate(date.ac = year*100+month  ) %>%
      select(date.ac,everything(),-year,-month,-Date)

  }
}





date2stocktime = function(datasets,colname){
  if (missing(colname)) {

    datasets %<>%
      mutate(year = as.numeric(substr(date,1,4))) %>%
      mutate(month = as.numeric(substr(date,5,6))) %>%
      mutate(day   = as.numeric(substr(date,7,8)))%>%
      mutate(hour  = 13) %>%
      mutate(min   = 30 ) %>%
      mutate(time  = ISOdate(year,month,day,hour,min) ) %>%
      select(time,year,month,day,everything())

  }else{

    datasets %<>%
      mutate(year = as.numeric(substr(colname,1,4))) %>%
      mutate_(month = as.numeric(substr(colname,5,6))) %>%
      mutate_(day   = as.numeric(substr(colname,7,8)))%>%
      mutate(hour  = 13) %>%
      mutate(min   = 30 ) %>%
      mutate(time  = ISOdate(year,month,day,hour,min) ) %>%
      select(time,year,month,day,everything())

  }
}

#應更名為twdate.ym2ac.time
date2time = function(datasets){
  datasets %>%
    change_date() %>%
    month_firstday()

}


date2zoo = function(datasets){
  datasets %>%
    mutate(zoo = as.yearmon(as.character(paste(Year,Month,sep="-")))) %>%
    select(zoo,everything())
}

today <- function () {
  assign("today",Sys.Date() , envir = .GlobalEnv)
  assign("today.quarter",quarter(Sys.time()) , envir = .GlobalEnv)
  assign("today.month",month(Sys.time()) , envir = .GlobalEnv)
  assign("today.year",year(Sys.time()) , envir = .GlobalEnv)
}

#與date2ac重覆
ym2date = function(datasets){
  datasets %>%
    mutate(Date = 100*(as.numeric(Year)-1911)+as.numeric(Month)) %>%
    select(Date,everything())
}



filter.year <- function(datasets){
  if("Time" %in% colnames(datasets)){
    datasets %>%
      filter(Time >= as.Date(floor_date(today,"year")),Time<=today)


  }else if ("time" %in% colnames(datasets)){
    datasets %>%
      filter(time >= as.Date(floor_date(today,"year")),time<=today)
  }
}


filter.quarter <- function(datasets){
  if("Time" %in% colnames(datasets)){
    datasets %>%
      filter(Time >= as.Date(floor_date(today,"quarter")),Time<=today)


  }else if ("time" %in% colnames(datasets)){
    datasets %>%
      filter(time >= as.Date(floor_date(today,"quarter")),time<=today)
  }
}

filter.month <- function(datasets){
  if("Time" %in% colnames(datasets)){
    datasets %>%
      filter(Time >= as.Date(floor_date(today,"month")),Time<=today)


  }else if ("time" %in% colnames(datasets)){
    datasets %>%
      filter(time >= as.Date(floor_date(today,"month")),time<=today)
  }
}

filter.week <- function(datasets){
  if("Time" %in% colnames(datasets)){
    datasets %>%
      filter(Time >= as.Date(floor_date(today,"week")),Time<=today)


  }else if ("time" %in% colnames(datasets)){
    datasets %>%
      filter(time >= as.Date(floor_date(today,"week")),time<=today)
  }
}


#create timeline point for loop
loop.timeline<- function(year_start,year_end){
  #今日時間
  today = Sys.time()

  today <- today %>%
    str_match("^(\\w{4})-(\\w{2})-(\\w{2})")

  ## leading zero
  today.year  = today[1,2]
  today.month = today[1,3]
  today.day   = today[1,4]

  today = paste(today.year ,
                today.month ,
                today.day ,
                sep="")

  timeline = vector(mode="character", length=0)
  for (year in (year_start:year_end)){
    for (month in 1:12){
      for (day in 1:31){
        date = paste(year,month,day,sep="/")
        date = gsub("-","",as.character(as.Date(date,format="%Y/%m/%d")))
        timeline = c(timeline,date)
      }
    }
  }

  timeline=timeline[!is.na(timeline)]
  timeline=timeline[timeline<=today]
}
######### WD ############################
#' 預警窗口通訊錄
#'
#' @return
#' @export
#'
#' @examples
wd.kbl.window<-function(){
  source("WD_Tidy_WatchDogWindow.R",encoding="utf-8")
  wd.corp.window %>%
    select(Name,con_name,con_tel,con_email,con_name2,con_tel2,con_email2) %>%
    add.name.order() %>%
    arrange(Name) %>%
    kbl_tii()

}



wd.company.alive<-function(){
  result<-Hash.Company %>%
    filter(Exist==1) %>%
    select(Name)
  return(result)
}

wd.company.add_frame <-function(df){
  company.alive = wd.company.alive()

  result <- df %>%
    merge(company.alive,by="Name",all.y=T) %>%
    add.name.order() %>%
    arrange(Name)
  return(result)
}


wd.addressbook<-function(list=NULL){
  channel   = odbcConnect("j1452",uid="400201",pw.wd)
  corp.window   = sqlQuery(channel,"SELECT * FROM [TIIReportDB].[dbo].[company]")

  # if (isMac()){
  #   path = "./Data_Insurance/"
  # } else{
  #   path = ".\\Data_Insurance\\"
  # }

  path = dir.r.raw

  save(corp.window   ,file = paste(path,"corp.window.RData",sep=""))
  load(paste(path,"corp.window.Rdata",sep=""))


  corp.email<-corp.window %>%
    mutate(companyID = as.numeric(companyID)) %>%
    rbc.filter.life.alive() %>%
    select(company_shortname,con_name,con_email,con_name2,con_email2)


  if (is.null(list)){
    corp.email_to_send<-corp.email%>%
      select(con_email,con_email2) %>%
      unlist() %>%
      paste(.,collapse=",")

    corp.email_to_send = paste(corp.email_to_send,"Hellen.Hsu@yuanta.com","wanru.lin@yuanta.com",sep=",")
  }else{
    corp.email_to_send<-corp.email%>%
      filter(!grepl(list,company_shortname)) %>%
      select(con_email,con_email2) %>%
      unlist() %>%
      paste(.,collapse=",")
  }

  addressbook = list("window"      = corp.window,
                     "mail"        = corp.email,
                     "mail.filter" = corp.email_to_send)

  return(addressbook)
}


invest.splitsource.hedgecost <-function(Datasets){
  temp_0 = data.frame()
  for (loop_i in 1:12){
    temp_i <- Datasets  %>%
      mutate(資金運用收益率_分母 = ( (可運用資金總計+lag(可運用資金總計,n=loop_i)-投資業務損益)/2 )) %>%
      mutate(資金運用收益率_利息收入 = 12 / loop_i * 利息收入/資金運用收益率_分母) %>%
      mutate(資金運用收益率_AC = 12 /loop_i * AC/資金運用收益率_分母) %>%
      mutate(資金運用收益率_FVPL = 12 /loop_i * (FVPL-衍生性商品避險損益)/資金運用收益率_分母) %>%
      mutate(資金運用收益率_FVOCI = 12 /loop_i * FVOCI/資金運用收益率_分母) %>%
      mutate(資金運用收益率_Hedge = 12 / loop_i * (兌換損益+衍生性商品避險損益+外匯價格變動準備金淨變動-避險成本)/資金運用收益率_分母) %>%
      mutate(資金運用收益率_避險成本 = 12 / loop_i * (避險成本/資金運用收益率_分母)) %>%
      mutate(資金運用收益率_覆蓋法 = 12 / loop_i * (採用覆蓋法重分類之損益)/資金運用收益率_分母) %>%
      mutate(資金運用收益率_Others = 12 /loop_i * (投資業務損益-(AC+FVPL+FVOCI+利息收入+兌換損益+外匯價格變動準備金淨變動+採用覆蓋法重分類之損益))/資金運用收益率_分母) %>%
      mutate(資金運用收益率 = 12 / loop_i * 投資業務損益 / ( (可運用資金總計+lag(可運用資金總計,n=loop_i)-投資業務損益)/2 ) ) %>%
      filter(Month==loop_i) %>%
      select(-資金運用收益率_分母)
    temp_0 = rbind(temp_0,temp_i)
  }
  Datasets = temp_0
}


cc <- function(datasets){
  datasets %>%
    colnames()

}



#Unit
#dollar.bank = dollar_format()

dollar.million = function(input){
  output =  input/(10^6)

}

dollar.hundredmillion = function(input){
  output =  input/(10^8)
}

dollar.trillion = function(input){
  output =  input/(10^12)
}




endmonth = function(Datasets){
  #for USDTWD datasets
  Datasets.xts <- xts(Datasets[,-1],order.by=Datasets[,1])
  endmonth_points <- Datasets.xts %>%
    endpoints(on="months",k=1)
  Datasets.xts[endmonth_points,]
}



export.doc = function(Datasets){
  doc <- docx()
  doc <- addFlexTable( doc, vanilla.table(Datasets))
  writeDoc(doc, file = "D:\\R資料輸出.docx")
}



financial_ratio = function(Datasets){
  Datasets  %<>%
    mutate(負債比例              = 負債/資產) %<>%
    mutate(業主權益比例          = 業主權益/資產) %<>%
    mutate(業主權益比例_不含分離 = 業主權益/(資產-分離帳戶)) %<>%
    #mutate(業主權益報酬率        = 12 / 年化因子 *稅後損益/( (業主權益+lag(業主權益,n=年化因子))/2) ) %<>%
    mutate(業主權益變動率        = ( 業主權益-lag(業主權益,n=12) ) / abs(lag(業主權益,n=12)) )    %<>%
    mutate(純益率                = 稅後損益/營業收入)     %<>%
    mutate(淨利變動率            = (稅後損益 - lag(稅後損益,n=12))/abs(lag(稅後損益,n=12)) )     %<>%
    mutate(不動產投資與抵押對資產比率  = (投資用不動產+不動產抵押放款)/  ( (資產+lag(資產,n=12) )/2)  )   %<>%
    mutate(逾放比                = 逾放金額/放款總額)   %<>%
    mutate(各種責準金淨變動對保費收入比率 = ifelse(
                                    is.infinite((保險負債淨變動+金融商品性質之準備淨變動)/保費收入),
                                    0,
                                    (保險負債淨變動+金融商品性質之準備淨變動)/保費收入 )) %<>%
    mutate(現金流量              = (營業收入+營業外收入)/(營業成本+營業外支出+預計所得稅)) %>%
    mutate(各種準備金淨增額對保費收入比率 = (保險負債淨變動+金融商品性質之準備淨變動)/保費收入)




  Datasets %<>%
    mutate(銀行存款p = 銀行存款/資金運用總計) %<>%
    mutate(有價證券p = 有價證券/資金運用總計) %<>%
    mutate(國外投資p = 國外投資/資金運用總計) %<>%
    mutate(不動產p   = 不動產/資金運用總計) %<>%
    mutate(放款p     = 放款/資金運用總計) %<>%
    mutate(專案運用與公共投資p = 專案運用與公共投資/資金運用總計) %<>%
    mutate(投資保險相關事業p = 投資保險相關事業/資金運用總計) %<>%
    mutate(從事衍生性商品交易p = 從事衍生性商品交易/資金運用總計) %<>%
    mutate(其他資金運用p = 其他資金運用/資金運用總計) %<>%
    mutate(資金運用組合變動率    = (  abs(銀行存款p-lag(銀行存款p,n=12)) +
                             abs(有價證券p-lag(有價證券p,n=12)) +
                             abs(國外投資p-lag(國外投資p,n=12)) +
                             abs(不動產p  -lag(不動產p,n=12)) +
                             abs(放款p-lag(放款p,n=12)) +
                             abs(專案運用與公共投資p-lag(專案運用與公共投資p,n=12))+
                             abs(投資保險相關事業p -lag(投資保險相關事業p ,n=12))+
                             abs(從事衍生性商品交易p-lag(從事衍生性商品交易p,n=12))+
                             abs(其他資金運用p-lag(其他資金運用p,n=12))   )/資金運用項目種數)


  temp_0 = data.frame()
  for (loop_i in 1:12){
    Datasets  %>%
      mutate(資金運用收益率old = 12 / loop_i * 淨投資損益 / ( (資金運用總計+lag(資金運用總計,n=loop_i)-淨投資損益)/2 ) ) %>%
      mutate(資金運用收益率_Son = 12 / loop_i * 淨投資損益) %>%
      mutate(資金運用收益率_Mom = ( (資金來源總計+lag(資金來源總計,n=loop_i)-淨投資損益)/2 )) %>%
      mutate(資金運用收益率 = 12 / loop_i * 淨投資損益 / ( (資金來源總計+lag(資金來源總計,n=loop_i)-淨投資損益)/2 ) ) %>%
      mutate(業主權益報酬率 = 12 / loop_i * 稅後損益   / ( (業主權益+lag(業主權益,n=loop_i))/2) ) %>%
      mutate(資產報酬率 = 12 / loop_i * 稅後損益   / ( (資產+lag(資產,n=loop_i))/2) ) %>%
      filter(Month==loop_i) ->temp_i
    temp_0 = rbind(temp_0,temp_i)
  }
  Datasets = temp_0

}




filtercurrent = function(dataset){
  result <- dataset %>%
    filter(Date==本期)


}

filterstart = function(dataset,date){
  result <- dataset %>%
    filter(Date>= date)


}


filtername = function(dataset,target){
  name.english2chinese <- Hash.Company %>%
    mutate(English_Name = tolower(English_Name) ) %>%
    filter(grepl(tolower(target),English_Name)) %>%
    .$Name


  result<-dataset %>%
    filter(grepl(target,Name)|Name==name.english2chinese)

}

## getLocation

## 這個會受查詢次數所限，所以必須用Sleep來防止問題的發生
getLocation = function(addr){

  library(magrittr)
  library(jsonlite)
  library(data.table)
  addr %<>% as.character
  url =
    paste0("https://maps.googleapis.com/maps/api/geocode/json?address=",
           addr)
  res = fromJSON(url, flatten = T)
  lat = res$results$geometry.location.lat
  lng = res$results$geometry.location.lng
  result = data.table("addr" = addr,
                      "lat" = lat,
                      "lng" = lng)
  return(result)
}


#Kit tool to filter Data
get_data = function(Data,Item,Time){
  Item = as.name(Item)
  filter(Data,Date %in% Time)   %>% group_by(Date)  %>% summarise(TempName = sum(Item))
}

geom_bar_dodge <- function(){

  geom_bar(stat="identity",position = "dodge")

}


str_fix_special_string<-function(string){
  string %<>%
    str_replace_all(pattern = "\\[",replacement = "\\\\[") %>%
    str_replace_all(pattern = "\\]",replacement = "\\\\]") %>%
    str_replace_all(pattern = "\\(",replacement = "\\\\(") %>%
    str_replace_all(pattern = "\\)",replacement = "\\\\)")

  return(string)
}


#find keyword in which R-file
kof<-function(keyword){
  keyword %<>%
    str_fix_special_string()

  dir("D:\\R_notebook\\bin\\")
  filenames = list.files(pattern=".R$|.Rmd$|.qmd$")
  #keyword="網站"
  for( f in filenames ){
    #print(f)
    kf(keyword,f)

  }
}


kf <-function(keyword,f){
  #print(f)
  x <- readLines(f,encoding = "UTF-8")
  y <- grep(keyword, x )
  if (length(y)!=0){
    print(paste0("Catch ====>  ",f))
  }
}


hh <-function(datasets){
  datasets %>%
    head()

}


vvh <- function(datasets){
  datasets %>%
    head() %>%
    View()

}



vvt <- function(datasets){
  datasets %>%
    tail() %>%
    View()

}

kkh <- function(datasets){

  datasets %>%
    head() %>%
    kable()

}


kk <- function(datasets){

  datasets %>%
    kable()

}


kkt <- function(datasets){

  datasets %>%
    tail() %>%
    kable()

}

ss <-function(datasets){
  datasets %>%
    str()
}

gg <-function(datasets){
  datasets %>%
    glimpse()
}


lookup.stockname <- function(datasets,stockcode){
  output = datasets$stockname[datasets$stockcode==stockcode]
}

month_lastday = function(datasets){
  datasets %>%
    mutate(Time = (as.Date(as.character(paste(as.numeric(Year)+ifelse(Month==12,1,0),ifelse(Month==12,1,Month+1),1,sep="-")))-1)) %>%
    select(Time,everything())
}


month_firstday = function(datasets){
  datasets %>%
    mutate(Time = (as.Date(
      as.character(
        paste(as.numeric(Year),as.numeric(Month),1,sep="-"))))) %>%
    select(Time,everything())
}

name_by_row <- function(x,row){
  if(missing(row)){
    row=1
  }

  #remove current colnames
  colnames(x) = ""

  #add new colnames from specified row
  colnames(x) = x[row,]

  #delete
  x = x[-row,]
}


#Normal_From
nf = function(Datasets,Hash){
  Datasets %>%
    merge(unique(select(Hash,Name,Size,Order)),by="Name") %>%
    arrange((Order))
}



#change data's fomrat from number to character
num2char = function(datasets){
  if("DataDate" %in% colnames(datasets)){
    datasets %<>%
      mutate(DataDate=toString(DataDate))
  }

  if("Date" %in% colnames(datasets)){
    datasets %<>%
      mutate(Date=as.character(Date))
  }

  if("Year" %in% colnames(datasets)){
    datasets %<>%
      mutate(Year=as.character(Year))
  }

  if("Month" %in% colnames(datasets)){
    datasets %<>%
      mutate(Year=as.character(Month))
  }

  if("Order" %in% colnames(datasets)){
    datasets %<>%
      mutate(Order=as.character(Order))
  }
}



#low level parsing link by regular expression
parse.link.base <- function(url,pattern){
  html <- paste(readLines(url), collapse="\n")
  matched <- str_match_all(html, pattern)

}


#simle method for parsing link by regular expression
parse.link.fast <- function(url){
  page <- read_html(url)
  temp.link = (html_attr(html_nodes(page, "a"), "href"))
}










#pecentage
percent = function(x,digit){
  if(missing(digit)){
    digit=2
  }

  sprintf(paste0("%1.",digit,"f%%"), 100*x)
}


#' Heatmap
#'
#' @param datasets
#' @param x 要加入quo
#' @param y 要加入quo
#' @param z 要加入quo
#'
#' @return
#' @export
#'
#' @examples
plot.heatmap <- function(datasets,x,y,z){
  datasets %>%
    ggplot(aes_string((x), (y), fill=(z))) + geom_tile() +
    geom_text(aes_string(label=(z)),colour="white")+
    theme(axis.text.x=element_text(size = 12),axis.text.y=element_text(size = 12)) +
    theme_cht_JhengHei()
}
##### -- 下面寫法值得學習
# # 寫法1，重點在轉化
# heatmap <- function(datasets,x_col,y_col,z_col){
#   plot_data <- datasets[,c(x_col, y_col,z_col)]
#   colnames(plot_data) <- c('x', 'y','z')
#
#   plot_data %>%
#     ggplot(aes(x, y, fill=z)) + geom_tile() +
#     geom_text(aes(label=z),colour="white")+
#     theme(axis.text.x=element_text(size = 15),axis.text.y=element_text(size = 15)) +
#     theme_cht_JhengHei()
#
# }
#
# # 寫法2，重點在get
# heatmap <- function(datasets,x,y,z){
#
#   datasets %>%
#     ggplot(aes(get(x), get(y), fill=get(z))) + geom_tile() +
#     geom_text(aes(label=get(z)),colour="white")+
#     theme(axis.text.x=element_text(size = 15),axis.text.y=element_text(size = 15)) +
#     theme_cht_JhengHei()
#
# }
#


r2html <- function(filepath){
  #為的是將script convert to html
  spin(filepath)  # default markdown
  o = spin(s, knit = FALSE)  # convert to Rmd only
  knit2html(o)  # compile to HTML

  #  filepath = "D:\\R_notebook\\Watch Dog\\WD_Analysis_CountryRisk.R"
}

scale_x_date_Y <-function(){
  scale_x_date(date_labels=("%Y"),date_breaks = "1 year")
}

scale_x_date_Q <-function(){
  scale_x_yearqtr(format = "%q")
}


scale_x_date_m <-function(){
  scale_x_date(date_labels=("%m"),date_breaks = "1 month")
}


scale_x_date_my <-function(){
  #scale_x_date(date_labels=("%m/%y"),date_breaks = "1 month")
  scale_x_date(date_breaks = "1 months",
               labels = function(x) if_else(is.na(lag(x)) | !year(lag(x)) == year(x),
                                            paste(month(x), "\n", year(x)),
                                            paste(month(x))))

}



scale_x_date_Ymd <-function(){
  scale_x_date(date_labels = "%Y/%m/%d")
}

scale_x_percentage <- function(){
  scale_x_continuous(labels = scales::percent)

}


scale_y_percentage <- function(){
  scale_y_continuous(labels = scales::percent)

}

rbc.filter.life.alive <- function(datasets){

  if ("companyID" %in% colnames(datasets)){
    result<-datasets %>%
      filter(companyID>200,companyID<300,companyID!=20401,companyID!=20402,companyID!=26401,companyID!=26402) %>%
      filter(!companyID %in% c(207,209,210,212,215,254,258,262,265,269,271,272))
  }else{
    result<-datasets %>%
      filter(!Name %in% c("原國泰","原國寶幸福","原南山","原朝陽","原全球","原國華"))

  }
}


rbc.filter.life.all <- function(datasets){
  result<-datasets %>%
    filter(companyID>200,companyID<300|companyID==20401|companyID==20402|companyID==26401|companyID==26402|companyID==20601|companyID==20602)


}

rbc.filter.life.global <- function(datasets){
  #含全球區隔與非區隔
  result<-datasets %>%
    filter(companyID>200,companyID<300|companyID==26401|companyID==26402)


}


rbc.add.name<-function(datasets){
  result <- datasets %>%
    merge(select(Hash.Company.RBC,companyID,Name),by="companyID") %>%
    select(Name,everything())

}

rbc.check.delaycom<-function(dataset){
  filing.com <- dataset %>%
    rbc.filter.life.alive() %>%
    filter(sheet=="表06") %>%
    add.name.rbc() %>%
    add.name.order() %>%
    pull(Name) %>%
    unique() %>%
    as.character()

  all.com <- Hash.Company.Uni %>%
    pull(Name)

  return(setdiff(all.com,filing.com))

}

split.moneyuse = function(Datasets){
  temp_0 = data.frame()
  for (loop_i in 1:12){
    Datasets  %>%
      mutate(資金運用收益率   = 12 / loop_i * 淨投資損益 / ( (資金運用總計+lag(資金運用總計,n=loop_i)-淨投資損益)/2 ) ) %>%
      mutate(淨投資對資產     = 淨投資損益  / (資產-分離帳戶) ) %>%
      mutate(資產對資金運用   = 12 / loop_i * (資產-分離帳戶) / ( (資金運用總計+lag(資金運用總計,n=loop_i)-淨投資損益)/2 ) ) %>%
      filter(Month==loop_i) ->temp_i
    temp_0 = rbind(temp_0,temp_i)
  }
  Datasets = temp_0

}

split.netincome = function(Datasets){
  temp_0 = data.frame()
  for (loop_i in 1:12){
    Datasets  %>%
      transmute(Date,Name,Size,Order,純益率 = 稅後損益/營業收入,ROA= 稅後損益/資產,桿槓 = 資產/營業收入)

  }


}


split.moneyperform = function(Datasets){
  temp_0 = data.frame()
  for (loop_i in 1:12){
    Datasets  %>%
      mutate( 資金運用收益率          = 12 / loop_i * 投資業務損益 / ( (資金運用總計+lag(資金運用總計,n=loop_i)-投資業務損益)/2 )) %>%
      mutate( 資金運用收益率_Son      = 12 / loop_i * 投資業務損益 ) %>%
      mutate( 資金運用收益率_Mom      = ( (資金運用總計+lag(資金運用總計,n=loop_i)-投資業務損益)/2 ) ) %>%
      mutate( 資金運用收益率_利息收入 = 12 / loop_i * 利息收入 / 資金運用收益率_Mom ) %>%
      mutate( 資金運用收益率_避險     = 12 / loop_i * (兌換損益+外匯價格變動準備金淨變動+profit) / 資金運用收益率_Mom ) %>%
      mutate( 資金運用收益率_其他     = 12 / loop_i * (投資業務損益-(利息收入+兌換損益+外匯價格變動準備金淨變動+profit)) / 資金運用收益率_Mom) %>%
      mutate( 資金運用收益率_資本利得 = 資金運用收益率_避險+資金運用收益率_其他 ) %>%
      mutate( 資金運用收益率_leverage = -資金運用收益率_其他/資金運用收益率_避險) %>%
      filter(Month==loop_i) ->temp_i
    temp_0 = rbind(temp_0,temp_i)
  }
  Datasets = temp_0
}

split.moneyperform2 = function(Datasets){
  temp_0 = data.frame()
  for (loop_i in 1:12){
    Datasets  %>%
      mutate( 資金運用收益率          = 12 / loop_i * 投資業務損益 / ( (資金運用總計+lag(資金運用總計,n=loop_i)-投資業務損益)/2 )) %>%
      mutate( 資金運用收益率_Son      = 12 / loop_i * 投資業務損益 ) %>%
      mutate( 資金運用收益率_Mom      = ( (資金運用總計+lag(資金運用總計,n=loop_i)-投資業務損益)/2 ) ) %>%
      mutate( 資金運用收益率_利息收入 = 12 / loop_i * 利息收入 / 資金運用收益率_Mom ) %>%
      mutate( 資金運用收益率_避險     = 12 / loop_i * (兌換損益+外匯價格變動準備金淨變動+profit) / 資金運用收益率_Mom ) %>%
      mutate( 資金運用收益率_其他_國外= 12 / loop_i * 國外投資損益 / 資金運用收益率_Mom )%>%
      mutate( 資金運用收益率_其他_國內= 12 / loop_i * (投資業務損益-(利息收入+兌換損益+外匯價格變動準備金淨變動+profit+國外投資損益)) / 資金運用收益率_Mom) %>%
      mutate( 資金運用收益率_資本利得 = 資金運用收益率_避險+資金運用收益率_其他 ) %>%
      mutate( 資金運用收益率_leverage = -資金運用收益率_其他/資金運用收益率_避險) %>%
      filter(Month==loop_i) ->temp_i
    temp_0 = rbind(temp_0,temp_i)
  }
  Datasets = temp_0
}



# display data by Single + Size + Agg
ssa = function(Data,Select_Date,Item){
  Data.Ratio = Data
  Data.Ratio %<>%
    financial_ratio %>%
    select(Time:Size,`負債比例`:`資產報酬率`)

  Data.Ratio %<>%
    filter(Date==Select_Date) %>%
    select(Date,Name,Order,get(Item))

  Data.Agg.Size %<>%
    filter(Date==Select_Date) %>%
    select(Date,Size,get(Item))

  Data.Agg.Total %<>%
    filter(Date==Select_Date) %>%
    select(Date,get(Item))

  ssa<- Data.Ratio %>%
    merge(select(Hash.Company,Name,Size),by="Name") %>%
    merge(Data.Agg.Size,by=c("Date","Size")) %>%
    merge(Data.Agg.Total,by="Date") %>%
    setnames(ncol(.),"整體") %>%
    setnames(ncol(.)-1,"規模") %>%
    setnames(ncol(.)-2,"個別") %>%
    arrange(Order)

}




theme_clear <- function () {
  theme_bw()+
    theme(legend.position = "bottom",
          axis.title.x = element_blank(),
          legend.title = element_blank())
}



theme_cht <-function(){
  theme(text = element_text(family = '黑體-繁 中黑'))
}


font.load_haha_fonts<-function(){
  ## 粉圓體
  library(showtext)
  showtext_auto()
  font_add("jf", dir.r.font %+% "jf-openhuninn-1.1.ttf")
  ## 正黑體
  font_add("zh.blod"  , dir.r.font %+% "msjhbd.ttc")
  font_add("zh"       , dir.r.font %+% "msjh.ttc")
  font_add("zh.sling" , dir.r.font %+% "msjhl.ttc")

  ## 激燃體
  font_add("fire" , dir.r.font %+% "burnfont-1.0.otf")
}




theme_cht_JhengHei <-function(){
  theme(text = element_text(family = 'zh.blod'))
}

theme_font_size <-function(size){
  theme(text = element_text(size=size))
}




theme_x_blank <-function(){

  theme(axis.title.x = element_blank())

}

theme_y_blank <-function(){

  theme(axis.title.y = element_blank())

}

theme_xy_blank <-function(){

  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

}


theme_y.axis_blank <-function(){

  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

}

theme_legend_title_blank <- function(){
  theme(legend.title=element_blank())

}

theme_legend_blank <- function(){
  theme(legend.position='none')

}


theme_legend_bottom <- function(){
  theme(legend.position='bottom')

}

theme_title_bold <- function(fontsize){
  if(missing(fontsize)){
    fontsize=14
  }
  theme(plot.title = element_text(color=default_blue, size=fontsize, face="bold",hjust=0,family="BL"))

}

theme_backgroud_transparent <- function(){
  theme(
    #panel.background = element_rect(fill = "transparent") # bg of the panel
    plot.background = element_rect(fill = "transparent") # bg of the plot
    #, panel.grid.major = element_blank() # get rid of major grid
    #, panel.grid.minor = element_blank() # get rid of minor grid
    #, legend.background = element_rect(fill = "transparent") # get rid of legend bg
    #, legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
  )
}



theme_tii <- function(){
  theme_fivethirtyeight()+
    theme_cht_JhengHei()
}

theme_tii_white <- function(){
  theme_tii()+
    theme_backgroud_white()
}



theme_backgroud_white <-function(){
  theme(panel.background = element_rect(fill = "white", colour = "white"))+
    theme(plot.background = element_rect(fill = "white"))

}



theme_title_center <- function(){

  theme(plot.title = element_text(hjust = 0.5))

}




tool.iconChart <- function(values, bar_width=round(.10 * max(values)), icon, categories=NULL) {

  max_val <- max(values)
  padding <- .15 * bar_width

  # Setup blank plot.
  xlim <- c(0, (bar_width+padding)*length(values))
  ylim <- c(0, ceiling(max_val / bar_width)+1)
  plot(0, 0, type="n", xlab="", ylab="", bty="n", xlim=xlim, ylim=ylim, asp=1, axes=FALSE)

  # Get coordinates for each element.
  for (i in 1:length(values)) {
    xoffset <- (i-1)*(bar_width+padding)
    num_rows <- ceiling(values[i] / bar_width)
    xleft <- rep(xoffset:(xoffset+bar_width-1), num_rows)[1:values[i]]
    ybottom <- rep(1:num_rows, each=bar_width)[1:values[i]]
    xright <- xleft + 1
    ytop <- ybottom + 1

    # Plot grid for current value.
    if (i <= length(icon)) {
      rasterImage(icon[[i]], xleft, ybottom, xright, ytop)
    } else {
      rasterImage(icon[[1]], xleft, ybottom, xright, ytop)
    }


    if (i <= length(categories)) {
      mtext(categories[i], 1, line = -2, at = (xoffset+bar_width/2))
    }
  }
}


trim <- function (x){
  gsub("^\\s+|\\s+$", "", x)
}

trim_space <- function (x){
  str_replace_all(x, regex("\\s*"), "")
}



trim_u00a0 <-function(x){
  x %<>%
    mutate(across(everything(),
                  ~(str_replace_all(.,pattern = "\U00A0",replacement = ""))
    )
    )

}


#tt
tt = function(datasets){
  datasets %>%
    t %>%
    data.frame() %>%
    View()
}
#vv
vv = function(datasets){
  datasets %>%
    View()
}


ee = function(datasets){
  datasets %>%
    edit()
}

warn.close <- function(){
  options(warn=-1) #close warning message
}

warn.open  <- function(){
  options(warn=0)  #open warning message
}



###################### color palette #############
default_red    = "#F8766D"
default_green  = "#00BFC4"
default_blue   = "#0044BB"
default_yellow = "#ffd306"



##################### Check #####################

wd.check.rpt19 <-function(){
  #新契約費用金額一致性
  rpt19 %>%
    filter(Date==now) %>%
    add.name() %>%
    filter(NormCode ==206|NormCode ==210) %>%
    mutate(NormValue = ifelse(NormCode==206,NormNumerator,NormValue)) %>%
    select(Name,NormCode,NormValue) %>%
    as.data.table() %>%
    dcast(Name~NormCode,sum) %>%
    mutate(diff = (`206`-`210`)/10^8)

}

check.ratio.moneyuse <- function(datasets,com,lag_period){
  datasets  %>%
    transmute(Time,Name,資金運用總計,資金運用總計lag  = lag(資金運用總計,n=lag_period),淨投資損益)  %>%
    filter(Name==com) %>%
    dollar.hundredmillion()
}

check.ratio.mortgage <-function(datasets){
  datasets %>%
    transmute(Date,投資用不動產,不動產抵押放款,資產,去年同期資產=lag(資產,n=12))

}

###################### Stock #############
tidy.stock.price <- function(datasets){
  datasets %>%
    select(-time,-year,-month,-day) %>%
    date2actime() %>%
    mutate(time = as.Date(time))

}


macro.plot.stock.index<-function(start.date,close.date){
  #macro.plot.stock.index("2021-01-30","2021-03-10")


  #?e???????]?t
  #start.date = "20190630"
  #close.date = "20200701"
  start.date.posixct = as.character(as.integer(as.POSIXct(start.date)))
  close.date.posixct = as.character(as.integer(as.POSIXct(close.date)))

  query.twii = paste0("https://query1.finance.yahoo.com/v7/finance/download/%5E","TWII","?period1=",start.date.posixct,
                      "&period2=",close.date.posixct,"&interval=1d&events=history")


  query.dji = paste0("https://query1.finance.yahoo.com/v7/finance/download/%5E","DJI","?period1=",start.date.posixct,
                     "&period2=",close.date.posixct,"&interval=1d&events=history")


  download.file(query.twii, destfile = paste0("D:\\R_notebook\\Stock\\price\\","^TWII",".csv"), method="curl", extra="-k")
  download.file(query.dji , destfile = paste0("D:\\R_notebook\\Stock\\price\\","^DJI" ,".csv"), method="curl", extra="-k")

  twii = read.csv("D://R_notebook//Stock//price//^TWII.csv")
  dowj = read.csv("D://R_notebook//Stock//price//^DJI.csv")

  plot.twii<-twii %>%
    mutate(year = year(Date)) %>%
    mutate(Close = as.numeric(Close)) %>%
    dplyr::rename(Time=Date) %>%
    mutate(Time = as.Date(Time)) %>%
    #filter(year==2008|year==2009) %>%
    ggplot(aes(Time,Close))+
    geom_line()+
    scale_x_date_m()+
    #scale_x_date(date_labels=("%m-%Y"),date_breaks = "4 month")+
    ggtitle("TWII")+
    theme_xy_blank()+
    theme_tii_white()

  plot.dowj<-dowj %>%
    mutate(year = year(Date)) %>%
    mutate(Close = as.numeric(Close)) %>%
    dplyr::rename(Time=Date) %>%
    mutate(Time = as.Date(Time)) %>%
    ggplot(aes(Time,Close))+
    geom_line()+
    scale_x_date_m()+
    #scale_x_date(date_labels=("%m-%Y"),date_breaks = "4 month")+
    ggtitle("Dow J")+
    theme_xy_blank()+
    theme_tii_white()
  haha.plot.window.new()

  print(plot.twii+plot.dowj+plot_layout(ncol = 1, heights = c(2, 2)))
  return(plot.twii+plot.dowj+plot_layout(ncol = 1, heights = c(2, 2)))
  #ggsave(paste0(quarter.save_path, ".png"), width=9, height=6,dpi=100)

}


###################### Query #############
query.news<-function(){
  df = read_excel(dir.news %+% "news.xlsx")
  return(df)
}


query.cbc_maincurrency <- function(){
  load(paste0(dir.r.raw ,"cbc_main_currency.RData"))
  return(cbc_main_currency.narrow)
}

query.usdtwd <- function(){
  USDTWD = read_excel(dir.r.in.fx %+% "tw_fx_usdtwd.xlsx")
  #USDTWD %>% str()
  USDTWD %<>%
    select(Time=date,USDTWD=usdtwd)
  return(USDTWD)

}
