#because date_time_j is calculated, has a bunch of hidden decimal places that a seq produced vector doesn't
  ##this makes it difficult to compare the data logger time stamps to a sequences created time, like what I have
  ## for my predicted temp data. This means you have to do some fiddling to be able to compare them


#first off, the datalogger data has alternating intervals--some steps increase by .007, and some by .0069
  ##the following function creates a sequence with alternating steps
  ##taken from https://stackoverflow.com/questions/41586530/get-a-seq-in-r-with-alternating-steps
seqAlt <- function(start, stop, by1, by2) {
  out <- diffinv(rep(c(by1, by2), ceiling(stop / (by1 + by2))), xi=start)
  return(out[out <= stop])
}

tst <- seqAlt(200.5625, 218.3681, 0.0069, 0.007)


#next, you have to specify the number of decimal places you want to round the data logger data to
  ##the following function does that, and trims any white space around the number. The only weird thing is that
  ##the output is a character, which is fine for matching purposes, but would have to be changed for any
  ##numerical calculations you want to do.
  ##function taken from https://stackoverflow.com/questions/3443687/formatting-decimal-places-in-r
specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k))

#I create a new column for this rounded, character date time, so that I have the raw datalogger time stamp if 
  ##I need it
temp_sum_h_con$date_time_seq <- specify_decimal(temp_sum_h_con$date_time_j, 4)

#now also convert your test sequence to a character
tst <- as.character(tst)

#run a simple ifelse argument to make sure your sequences are matching (use a row other than the first,
  ##that one always seems to match)

ifelse(temp_sum_h_con[2,10] == tst[2], 1, 0)
