#安定度指数を計算する
si <- function (pcode, ix = 9, rm.out = F, lim = NULL, verbose =T) {
  x <- t[t$pcode == pcode, ]
  x <- x[order(x$day), ]
  d <- rratio(pcode, ix = ix, rm.out = rm.out, plot = F, verbose = verbose)
  if (is.null(lim)) lim <- as.integer(row.names(d[1,]))
  x <- x[-1:-(lim-1),]
  #変動係数の計算
  cv.d0 <- sd(x$d0h, na.rm=T) / mean(x$d0h, na.rm=T)
  cv.d <- sd(x$dh, na.rm=T) / mean(x$dh, na.rm=T)
  cv.w <- sd(x$wh, na.rm=T) / mean(x$wh, na.rm=T)
  cv.a <- sd(x$ah, na.rm=T) / mean(x$ah, na.rm=T)
  #安定度指数の計算
  si.0d <- cv.d / cv.d0
  si.0w <- cv.w / cv.d0
  si.dw <- cv.w / cv.d
  si.da <- cv.a / cv.d
  res <- c(pcode, si.0d, si.0w, si.dw, si.da)
  if (verbose) print(res)
  res
}