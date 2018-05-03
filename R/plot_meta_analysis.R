library(HGUtils)
startup("MetaSurv analysis/")
load_common_packages()
install_load_packages(c("metafor"))

dat = get(data(dat.bcg))
dat = escalc(measure="RR", ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat)
fit = rma(dat)
forest(fit)
