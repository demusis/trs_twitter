# Examinar corpus de múltiplos documentos de texto

require(quanteda)

data(data_char_ukimmig2010)
head(data_char_ukimmig2010)

meuCorpus = corpus(data_char_ukimmig2010)  # corpus de teste (substitua pelo obtido dos Tweets)
summary(meuCorpus)
# definido como uma coleção de textos que inclui
# variáveis específicas de cada texto no nível do documento


# Adiciona variáveis no nível do documento
# docvars

docvars(meuCorpus, "Partido") = names(data_char_ukimmig2010)
docvars(meuCorpus, "Ano") = 2010
summary(meuCorpus)

# adiciona metadados

metadoc(meuCorpus, "idioma") = "inglês" # Alterar para português (se for o caso)
metadoc(meuCorpus, "fontedoc")  = paste("data_char_ukimmig2010", 1:ndoc(meuCorpus), sep = "_")
summary(meuCorpus, showmeta = TRUE)


texts(meuCorpus)[3] # procura texto no corpus

# cria um dfm
# matriz de frequência do documento
meuDfm = dfm(meuCorpus)
meuDfm[, 1:5]

# cria um dfm, removendo stopwords e aplicando stemming
meuStemMat = dfm(meuCorpus, remove = stopwords("inglês"), stem = TRUE, remove_punct = TRUE)
meuStemMat[, 1:5]

topfeatures(meuStemMat, 20) ## 20 palavras mais frequentes