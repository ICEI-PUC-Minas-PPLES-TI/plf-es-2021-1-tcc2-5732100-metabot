require(likert)

survey <- read.csv("path_to_file", header = T)
colnames(survey) <- c("Q4_1", "Q4_2", "Q4_3", "Q4_4", "Q4_5", "Q4_6", "Q4_7", "Q4_8")

# set levels and their ordering
mylevels <- c("Discordo fortemente", "Discordo", "Neutro", "Concordo", "Concordo fortemente")

# columns to generate the chart
columns = c("Q4_1", "Q4_2", "Q4_3", "Q4_4", "Q4_5", "Q4_6", "Q4_7", "Q4_8")

# likert scale
filtered = survey[columns]

# make sure all columns have all the levels (otherwise 'likert' function breaks)
for (i in seq_along(filtered)) {
  filtered[, i] <- factor(filtered[, i], levels = mylevels)
}

x <- likert(filtered)

### GENERATE DEFINITIONS.TEX
strongly_agree = 6
agree = 5
neutral = 4
disagree = 3
strongly_disagree = 2


### GENERATE FIGURES

# figure 1
columns_fig1 = c("Q4_1", "Q4_2", "Q4_3", "Q4_4", "Q4_5", "Q4_6", "Q4_7", "Q4_8") # order in the paper
filtered = survey[columns_fig1]

# make sure all columns have all the levels (otherwise 'likert' function breaks)
for (i in seq_along(filtered)) {
  filtered[, i] <- factor(filtered[, i], levels = mylevels)
}


x <- likert(filtered)
x$results$Item <- c("Q1. Os bots forneceram informações adequadas para a finalidade da contribuição",
                    "Q2. Eu não consegui entender as informações oferecidas pelos bots",
                    "Q3. Foi fácil de entender os bots",
                    "Q4. Eu percebi a presença dos bots",
                    "Q5. A informação oferecida pelos bots era clara",
                    "Q6. Os bots ofereceram informações que eram desnecessárias",
                    "Q7. A quantidade de informações oferecidas pelos bots foi adequada",
                    "Q8. Os bots contribuiram para melhorar a eficiência do meu trabalho")
colnames(x$items) <- c("Q1. Os bots forneceram informações adequadas para a finalidade da contribuição",
                    "Q2. Eu não consegui entender as informações oferecidas pelos bots",
                    "Q3. Foi fácil de entender os bots",
                    "Q4. Eu percebi a presença dos bots",
                    "Q5. A informação oferecida pelos bots era clara",
                    "Q6. Os bots ofereceram informações que eram desnecessárias",
                    "Q7. A quantidade de informações oferecidas pelos bots foi adequada",
                    "Q8. Os bots contribuiram para melhorar a eficiência do meu trabalho")

# x$results$Item <- c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8")
# colnames(x$items) <- c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8")

pdf("survey-likert-contributors.pdf", width = 26, height = 8.5)
likert.bar.plot(x, text.size=7, centered = T, group.order = x$results$Item) + labs(y = "Porcentagem") + guides(fill=guide_legend("Legenda:")) + theme(text = element_text(size = rel(6.5)),
                                                                                             legend.text = element_text(size = rel(3.5)),
                                                                                             axis.title.x = element_text(size = rel(3)),
                                                                                             legend.title = element_text(size = rel(3.2)))
dev.off()