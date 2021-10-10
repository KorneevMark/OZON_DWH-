install.packages("readxl")
library(readxl)
data_pat <- read_excel("C:/Users/markk/Desktop/Курсовая/Счёт/Данные/база данных_RnD_DEA.xlsx", sheet = "1")
data_expend <- read_excel("C:/Users/markk/Desktop/Курсовая/Счёт/Данные/база данных_RnD_DEA.xlsx", sheet = "2")
data_emp <- read_excel("C:/Users/markk/Desktop/Курсовая/Счёт/Данные/база данных_RnD_DEA.xlsx", sheet = "3")
data_pat_frame <- data.frame(data_pat)
data_expend_frame <- data.frame(data_expend)
data_emp_frame <- data.frame(data_emp)
dea.plot(x, y, RTS = "vrs",ORIENTATION="in",lty="dashed", txt =LETTERS[1:length((data_for_dea$name))])


data_for_dea_1 <- data.frame(input1 = data_expend_frame$X2014, input2 = data_emp_frame$X2014, output = data_pat_frame$X2015)
data_for_dea = data_for_dea_1[c(1:83),]
library(Benchmarking)
x <- matrix(c(data_for_dea$input1, data_for_dea$input2),  ncol=2)
y <- matrix(c(data_for_dea$output), ncol=1)
e_drs <- dea(x, y, RTS = "drs", ORIENTATION = "in")
eff(e_drs)
result15 <- data.frame(eff(e_drs))


#счёт горизонт 
r <- 83
hor<- data.frame(hor_input1 = data_expend_frame[r,], hor_input2 = data_emp_frame[r,], hor_output = data_pat_frame[r,])
hor_data = hor[1,] 
print(hor_data)
input1_hor <- hor_data[,c(3:23)]
input2_hor <- hor_data[,c(27:47)]
output_hor <- hor_data[,c(51:71)]
in1 <- matrix(input1_hor)
in2 <- matrix(input2_hor)
out_bad <- matrix(output_hor)
out <- as.numeric(out_bad)
mix <-  cbind(in1, in2)
in_2 <- as.numeric(mix)

print(in_2)
print(out)
library(Benchmarking)
in_mix <- matrix(c(in_2), ncol = 2)
print(in_mix)
out_to <- matrix(c(out), ncol = 1)
print(out_to)
evv <- dea(in_mix, out_to, RTS = "drs", ORIENTATION = "in")
t <- eff(evv)
print(t)
ul <- matrix(t, nrow = TRUE) 
print(ul)
w <- data.frame(ul)







dea.plot(in_mix, out_to, RTS = "drs",ORIENTATION="in",lty="dashed")
