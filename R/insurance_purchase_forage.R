"
Scratchpad for development of 'optimal'
range insurance purchase allocation. Ranchers
are assumed to choose the interval featuring the
months with highest combined weight, followed by
the next-highest weighted non-consecutive 
interval. 

For now 50% protection will be allocated
to each interval. This should be refined so that
protection amounts are proportional to each weight.

Support for choosing up to 5 intervals should 
also be incorporated here.

Should be merged with the insurance prep 
section in 'vars.R' when developed further.
"

target.loc="BOULDER, COLORADO"
source("R/vars.R")

fpwt=as.numeric(zonewt[stzone,]) # our forage potential weight (e.g. for MLRA 49)

# Bin forage potential weights into two-month intervals by mean
fpwt_iv=c()
for(m in 1:11){
  
  fpwt_iv=c(fpwt_iv,(sum(fpwt[m],fpwt[m+1])/2)) # need to calc mean manually - not sure why
  names(fpwt_iv[m])=paste0("i",m)
  
}

##OPTION 1 - RANKING##
# This one is very simple to implement when just two intervals are insured. 
# Ideally, the interval with the 2nd highest weights *does not* overlap
# with our top-weighted interval. If it does, the rancher chooses the 
# 3rd most highly-ranked interval instead -- unless that interval *also*
# overlaps our top-ranked interval, in which case the rancher settles on
# interval ranked 4 (this occurs in our Boulder example).
# 
# This could be extended to a simple algorithm that performs this process
# n-1 times with a rank shift for each secondary interval chosen. Eventually
# this might converge when additional choices are exhausted 

fpwt_iv_rank=rank(-fpwt_iv) # rank interval weights descending
names(fpwt_iv_rank)=paste0("i",1:11)
top_iv=which.min(fpwt_iv_rank) # index of top-ranked interval (i6, Jun-Jul)
adj_iv_rank=fpwt_iv_rank[c((top_iv-1),(top_iv+1))] # adjacent intervals to top rank

# is next rank (2) in either interval adjacent to 'top_iv'?
# if not, we can simply choose wherever interval rank 2 occurs 
2 %in% adj_iv_rank

# in this case, rank two is adjacent (in interval 7), 
# so now we need to check whether rank 3 also occurs in
# a consecutive interval
3 %in% adj_iv_rank

# Rank 3 is also in an interval adjacent to rank 1 (i5), 
# so now we should look for wherever rank 4 occurs as
# our second interval
sec_iv=which(fpwt_iv_rank==4) # i4, Apr-May

# alternatively...
if(min(adj_iv_rank)==2){
  if(3 %in% adj_iv_rank){
    sec_iv=which(fpwt_iv_rank==4)
  }else{
    sec_iv=which(fpwt_iv_rank==3)
  }
}else{
  sec_iv=which(fpwt_iv_rank==2)
}

# even better!
cand_iv=fpwt_iv_rank[-c((top_iv-1):(top_iv+1))] # candidate secondary intervals
sec_iv=as.numeric(substr(names(cand_iv[which.min(cand_iv)]),2,2)) # grab interval index from iv name

fpwt_iv[c(top_iv,sec_iv)] # inspect the weights

# we can even repeat that last technique
# for additional allocation intervals
cand_iv=fpwt_iv_rank[-unique(c((top_iv-1):(top_iv+1),(sec_iv-1):(sec_iv+1)))] # candidate tertiary intervals
thr_iv=as.numeric(substr(names(cand_iv[which.min(cand_iv)]),2,2)) # grab interval index from iv name

wt_iv=c(top_iv,sec_iv,thr_iv) # Jun-Jul, Apr-May, Aug-Sep
wt_choice=fpwt_iv[wt_iv] # inspect the weights
wt_choice # inspect the weights

## Convert weights to allocation amounts
wt_alloc=wt_choice/sum(wt_choice)

# or, if we wanted (or needed) to scale
# to a maximum allocation amount...
max_alloc=0.6
blah=c(0.7,0.2,0.1) # some toy allocation amts not valid for the model
blah[1]=blah[1]*(max_alloc/blah[1])
blah[2:3]=(blah[2:3]/sum(blah[2:3]))*(1-max_alloc)
blah

# with the actual weights
wt_alloc_scaled=wt_alloc
wt_alloc_scaled[1]=wt_alloc_scaled[1]*(max_alloc/wt_alloc_scaled[1])
wt_alloc_scaled[2:3]=(wt_alloc_scaled[2:3]/sum(wt_alloc_scaled[2:3]))*(1-max_alloc)
wt_alloc_scaled

# Final insurance purchase input 
cbind(wt_iv,round(wt_alloc,2))
cbind(wt_iv,round(round(wt_alloc,2))) # with more generalized rounding


##OPTION 2 - HIGHEST WEIGHTED INTERVAL SETS##
# This approach finds sets of n 
# non-consecutive intervals with the highest 
# overall (mean) weights. 
#
# However, a rancher making these choices might fail to 
# insure the interval with highest mean forage potential. 
# In this case, the optimal choice for two intervals
# is to insure intervals 5 (May-Jun) and 7 (Jul-Aug), 
# which are both highly weighted, but not quite as high 
# as the interval they bound (Jun-July).

niv=2 # number of intervals to insure 

# Separate intervals 
# this ensures no overlap
odd_int=seq(1,11,by=2)
even_int=seq(2,10,by=2)



