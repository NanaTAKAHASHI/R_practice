dat_mt <- matrix (1:10, c(5, 2))
dat_mt

a <- dim(dat_mt)
row_sum <- 0
res_sum <- 0

for (i in seq_len(a[1])){
    row_sum <- dat_mt[i,1] + dat_mt[i,2]
    res_sum <- res_sum + row_sum
}
res_sum

