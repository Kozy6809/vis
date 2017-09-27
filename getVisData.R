#dbから粘度データを取得する
getVisData <- function() {
library("RODBC")
ch<-odbcConnect("qc")
format(Sys.Date()-3652, "#%Y/%m/%d#")->day
t<-sqlQuery(ch, 
            paste("select input_date, seizou_code, lot_no, day_10, day_100, week_10, week_100, heat_10, heat_100 from 粘度データ where input_date > ", day, "and judgement = 0 order by seizou_code, input_date;"))
}