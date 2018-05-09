library(HGUtils)
startup("HGUtils/")
load_common_packages()
install_load_packages("metafor")

dat = get(data(dat.bcg))
dat = escalc(measure = "RR", ai = tpos, bi = tneg, ci = cpos, di = cneg, data = dat)
fit = rma(dat)
forest.rma(fit)

yi = fit$yi
vi = fit$vi
lbi = yi - qnorm(fit$level/2, lower.tail = FALSE) * sqrt(vi)
ubi = yi + qnorm(fit$level/2, lower.tail = FALSE) * sqrt(vi)
pred = predict.rma(fit)
pooled = pred$pred
se = pred$se
lb = pred$ci.lb
ub = pred$ci.ub

ind = cbind(est = yi, lb = lbi, ub = ubi, size = vi) %>% as_tibble
summary = cbind(est = pooled, lb = lb, ub = ub, size = se^2) %>% as_tibble
diamond_x = c(lb, pooled, ub, pooled)
diamond_y = c(0, 1, 0, -1) * 0.3
diamond = cbind(x = diamond_x, y = diamond_y) %>% as_tibble
ind$y = nrow(ind):1 + 1

# geom_point(aes(size=1/vi, shape=type), fill='black')
ggplot(ind, aes(x = est, y = y, xmin = lb, xmax = ub)) + geom_vline(aes(xintercept = pooled), linetype = "dashed") + geom_errorbarh(height = 0.2) + geom_point(aes(size = size), 
    fill = "black", shape = 22) + geom_polygon(data = diamond, mapping = aes(x = x, y = y), inherit.aes = F) + scale_x_continuous(breaks = plot_breaks(max_breaks = 10)) + 
    theme_classic()
