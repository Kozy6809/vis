#工程能力指数を計算する
#データ区間はrratioの戻り値によって制限する
#オプションlimで計算に使用するデータ区間を指定する
#デフォルトで1日粘度の解析。1週間はix=11とする
#オプションrm.outでboxplotの外れ値を除去した解析を指定する
#戻り値:
pci <- function (pcode, ix = 9, rm.out = F, lim = NULL, verbose = T) {
  x <- t[t$pcode == pcode, ]
  x <- x[order(x$day), ]
  d <- rratio(pcode, ix = ix, rm.out = rm.out, plot = F, verbose = verbose)
  pcode_ <- pcode
  specv <- ss(spec, spec$pcode == pcode_)
  sd.new <- d$sd.res[1]
  if (is.null(lim)) lim <- as.integer(row.names(d[1,]))
  #print(x[-1:-(lim-1),])
  sd.old <- sd(x[-1:-(lim-1),ix], na.rm = T)
  w <- ifelse(ix == 9, specv$dt - specv$db, specv$wt - specv$wb)
  cp.old <- w / (6 * sd.old)
  #旧規格と同じ工程能力指数となる新規格幅を計算する
  w.new <- w * (sd.new / sd.old)
  #cp.oldと新規格値を返す
  res <- c(pcode, cp.old, d$mean.est[1]-w.new/2, d$mean.est[1]+w.new/2)
  if (verbose) print(res)
  res
}