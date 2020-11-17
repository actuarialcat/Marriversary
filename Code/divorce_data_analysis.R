library(data.table)
library(akima)
library(ggplot2)



# trend analysis -------------------------------------------------

trend = fread("tables/divorce_trend.csv")

trend[, t := Year - trend[1, Year]]
trend[, ln_div_rate := log(trend$General_divorce_rate / 1000)]

trend_lm = lm(ln_div_rate ~ t, trend)

summary(trend_lm)
#plot(trend_lm)

trend_factor = exp(trend_lm$coefficients[2])
print(trend_factor)




# interpolate rate table -----------------------------------------

class_mark = function(label, class_interval){
    upper = strtoi(substr(label, 3, 4))
    return (upper + class_interval / 2)
}


# input
raw_divorce_inp = fread("tables/divorce_rate_raw.csv")

# pre-process
raw_divorce = melt(raw_divorce_inp, "V1")
setnames(raw_divorce, "V1", "M_class")
setnames(raw_divorce, "variable", "F_class")

raw_divorce[, M_age := class_mark(M_class, 4)]
raw_divorce[, F_age := class_mark(F_class, 4)]

# plot
ggplot(raw_divorce, aes(x = F_age, y = M_age, z = value, fill = value)) +
    geom_tile() + 
    stat_contour(color = "red")



# interpolate

raw_divorce_grid = dcast(raw_divorce, M_age ~ F_age)

raw_divorce_matrix = as.matrix(raw_divorce_grid[,-1])
x_cord = seq(22, 52, 5)
y_cord = seq(22, 52, 5)

#bicubic(x_cord, y_cord, raw_divorce_matrix, 20, 20)

divorce_grid = bicubic.grid(x_cord, y_cord, raw_divorce_matrix, 
                            xlim = c(18,65), ylim = c(18,65), dx = 1, dy = 1)

min_value = raw_divorce[nrow(raw_divorce), value]
divorce_matrix = apply(divorce_grid$z, c(1,2), max, min_value)

rownames(divorce_matrix) = 18:65
colnames(divorce_matrix) = 18:65


# output CSV
write.csv(divorce_matrix, "tables/divorce_rate_table.csv")


# plot

divorce_rates = data.table(M_age = rownames(divorce_matrix), divorce_matrix)
divorce_rates = melt(divorce_rates, "M_age", variable.name = "F_age")
divorce_rates[, M_age := strtoi(M_age)]
divorce_rates[, F_age := strtoi(F_age)]


ggplot(divorce_rates, aes(x = F_age, y = M_age, z = value, fill = value)) +
    geom_tile() + 
    stat_contour(color = "red")



