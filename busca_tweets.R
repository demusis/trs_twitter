# Rotina de extração de tweets
# Carlo
# 27/05/2022

# setwd("C:/Users/GPCMT/OneDrive/Text Mining and Natural Language Processing in R/Code_n_data/section5")
# setwd("/cloud/project")

library(base64enc)
library(dplyr)
library(ggplot2)
library(httr)
library(plyr)
library(ROAuth)
library(syuzhet)
library(tm)
library(twitteR)
library(wordcloud)

api_key = ""
api_secret = "" 
access_token = ""
access_token_secret = ""

setup_twitter_oauth(api_key,
                    api_secret, # api secret
                    access_token, # access token
                    access_token_secret # access token secret
)

texto_busca <- "Amazonia"
n_tweets <- 2000


# Centro geográfico do Brasil
# Município de Peixoto de Azevedo
# Coordenadas: -10.24211,-54.99517
# Círculo com raio de aproximadamente 2700km

busca_tweets <- searchTwitter(texto_busca, 
                              n = n_tweets,
                              # geocode = "-10.24211,-54.99517, 2700km",
                              # since="2021-01-01", until="2021-12-31",
                              lang="pt",
                             )

busca_tweets_df <- do.call("rbind", lapply(busca_tweets, 
                                           as.data.frame))
feed_busca_tweets <- laply(busca_tweets, 
                           function(t) t$getText())


texto <- as.character(busca_tweets_df$text) # Converte em vetor de textos

# Limpa pontuações, números, espaços e stopwords
amostra <- sample(texto, (length(texto)))

corpus <- Corpus(VectorSource(list(amostra)))
corpus <- tm_map(corpus, removePunctuation) # Remove pontuações
corpus <- tm_map(corpus, content_transformer(tolower)) # Coloca todos caracteres em maiúsculas
corpus <- tm_map(corpus, removeNumbers) # Remove os números
corpus <- tm_map(corpus, stripWhitespace) # Remove tabulações e espaços extras
corpus <- tm_map(corpus, removeWords, stopwords('portuguese')) # Remove "palavras vazias"

corpus <- tm_map(corpus, stemDocument) # lematiza

dtm <- DocumentTermMatrix(corpus)

# Lista os termos com f>20
findFreqTerms(dtm, lowfreq=20)

# Guarda os termos e frequencias
removeDisperso <- removeSparseTerms(dtm, 0.9) 

# Remove termos pouco frequentes
# A "dispersão" refere-se ao máximo da frequência relativa do documento para um 
# termo, acima do qual o mesmo será removido. 
# Por exemplo, um termo que apareça apenas 4 vezes em um corpus de tamanho 1000, 
# terá uma frequência de aparecimento de 0,004 = 4/1000.

# A dispersão deste termo será (1000-4)/1000 = 1- 0,004 = 0,996 = 99,6%.
# Portanto, se o limite for = 0,90, esse termo será removido.
# No entanto, se o limite for = 0,999, esse termo não será removido.


freq_up = colSums(as.matrix(removeDisperso)) # Frequência dos termos

# Nuvem de palavras
bag = as.matrix(freq_up)
bag = sort(rowSums(bag), decreasing = T)
bag.df = data.frame(word = names(bag), freq = bag)

set.seed(154)
str(bag)

wordcloud(words = bag.df$word, 
          freq = bag.df$freq, 
          min.freq = 5,
          max.words=15, 
          random.order=FALSE, 
          rot.per=0.25,
          colors=brewer.pal(8, "Dark2"),
          scale = c(0.5,1))


# Análise de sentimentos

# Limpando com gsub
outro_texto <- gsub("(RT|via)((?:\\b\\w*@\\w+)+)","", busca_tweets_df$text)
outro_texto <- gsub("http[^[:blank:]]+", "", outro_texto)
outro_texto <- gsub("@\\w+", "", outro_texto)
outro_texto <- gsub("[[:punct:]]", " ", outro_texto)
outro_texto <- gsub("[^[:alnum:]]"," ",outro_texto)

# Utiliza o dicionário de sentimentos NRC para calcular a presença de 8 emoções.
tweetSentimento <- get_nrc_sentiment(outro_texto, language = "portuguese")

barplot(
  sort(colSums(prop.table(tweetSentimento[, 1:8]))), 
  #  horiz = TRUE, 
  cex.names = 0.7, 
  las = 1, 
  main = "Emoções", xlab="Percentual"
)


SentimentoEscores <- data.frame(colSums(tweetSentimento[,]))
names(SentimentoEscores) <- "escore"
SentimentoEscores <- cbind("sentimento"=rownames(Sentimentscores), SentimentoEscores)

rownames(SentimentoEscores) <- NULL

ggplot(data = SentimentoEscores, 
       aes(x=sentimento, y=escore))+
       geom_bar(aes(fill=sentimento),stat = "identity")+
       theme(legend.position="none")+
       xlab("Sentimentos")+ylab("Escores")+
       ggtitle("Sentimentos totais baseado em escores")
