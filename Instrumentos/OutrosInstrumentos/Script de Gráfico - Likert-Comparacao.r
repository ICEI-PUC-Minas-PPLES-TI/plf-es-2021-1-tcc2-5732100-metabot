require(likert)

survey <- read.csv("path_to_file", header = T)
colnames(survey) <- c("Q1_1", "Q2_1", "Q3_1", "Q4_1", "Q5_1")

# set levels and their ordering
mylevels <- c("Vários bots", "Tende aos vários bots", "Neutro", "Tende ao meta-bot", "Meta-bot")
#mylevels <- c("1", "2", "3", "4", "5")

# columns to generate the chart
columns = c("Q1_1", "Q2_1", "Q3_1", "Q4_1", "Q5_1")

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
columns_fig1 = c("Q1_1", "Q2_1", "Q3_1", "Q4_1", "Q5_1") # order in the paper
filtered = survey[columns_fig1]

# make sure all columns have all the levels (otherwise 'likert' function breaks)
for (i in seq_along(filtered)) {
  filtered[, i] <- factor(filtered[, i], levels = mylevels)
}


x <- likert(filtered)
x$results$Item <- c("Q1. Em qual das interações foi possível obter a informação de forma mais objetiva?",
                    "Q2. Em qual das interações foi mais rápido para encontrar as informações referentes ao caso analisado?",
                    "Q3. Em qual das interações a informação estava disposta de maneira mais propícia ao entendimento do problema?",
                    "Q4. Em qual das interações foi possível notar que a quantidade de informações dificultou a busca pela informação?",
                    "Q5. Em qual das interações foi possível identificar os bots presentes no repositório com mais facilidade?")
colnames(x$items) <- c("Q1. Em qual das interações foi possível obter a informação de forma mais objetiva?",
                    "Q2. Em qual das interações foi mais rápido para encontrar as informações referentes ao caso analisado?",
                    "Q3. Em qual das interações a informação estava disposta de maneira mais propícia ao entendimento do problema?",
                    "Q4. Em qual das interações foi possível notar que a quantidade de informações dificultou a busca pela informação?",
                    "Q5. Em qual das interações foi possível identificar os bots presentes no repositório com mais facilidade?")

# x$results$Item <- c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8")
# colnames(x$items) <- c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8")

pdf("survey-likert-contributors.pdf", width = 15, height = 6.5)
likert.bar.plot(x, text.size=5, centered = T, group.order = x$results$Item) + labs(y = "Porcentagem") + guides(fill=guide_legend("Legenda:")) + theme(text = element_text(size = rel(5.5)),
                                                                                             legend.text = element_text(size = rel(3.5)),
                                                                                             axis.title.x = element_text(size = rel(3)),
                                                                                             legend.title = element_text(size = rel(3.2)))
dev.off()