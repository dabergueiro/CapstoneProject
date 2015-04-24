library(shiny)
library(tm)
library(SnowballC)
library(RWeka)

Quizzes <- c("When you breathe, I want to be the air for you. I'll be there for you, I'd live and I'd die for you",
             "Guy at my table's wife got up to go to the bathroom and I asked about dessert and he started telling me about his marital problems",
             "I'd give anything to see arctic monkeys this weekend",
             "Talking to your mom has the same effect as a hug and helps reduce your stress",
             "I can't deal with unsymetrical things. I can't even hold an uneven number of bags of groceries in each hand",
             "Every inch of you is perfect from the bottom to the top",
             "I'm thankful my childhood was filled with imagination and bruises from playing outside",
             "I like how the same people are in almost all of Adam Sandler's movies",
             "The guy in front of me just bought a pound of bacon, a bouquet, and a case of beer",
             "You're the reason why I smile everyday. Can you follow me please? It would mean the world",
             "Hey sunshine, can you follow me and make me the happiest",
             "Go on a romantic date at the beach",
             "Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my way",
             "Ohhhhh #PointBreak is on tomorrow. Love that film and haven't seen it in quite some time",
             "After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little fingers",
             "Be grateful for the good times and keep the faith during the bad",
             "If this isn't the cutest thing you've ever seen, then you must be insane")

BuildMat <- function(gram)
{
  n <<- gram
  if (gram == 4)
  {
    twitter <- readLines("Data/en_US.twitter.txt", n=20000)
    blogs <- readLines("Data/en_US.blogs.txt", n=20000)
    news <- readLines("Data/en_US.news.txt", n=20000)
    Predic <<- paste(ParsedPhrase[(length(ParsedPhrase)-2)], ParsedPhrase[(length(ParsedPhrase)-1)], ParsedPhrase[length(ParsedPhrase)], sep = " ")
  }
  else
  {
    if (gram ==3)
    {
      twitter <- readLines("Data/en_US.twitter.txt", n=10000)
      blogs <- readLines("Data/en_US.blogs.txt", n=10000)
      news <- readLines("Data/en_US.news.txt", n=10000)
      Predic <<- paste(ParsedPhrase[(length(ParsedPhrase)-1)], ParsedPhrase[length(ParsedPhrase)], sep = " ")
    }
    else
    {
      if (gram == 2)
      {
        twitter <- readLines("Data/en_US.twitter.txt", n=300)
        blogs <- readLines("Data/en_US.blogs.txt", n=300)
        news <- readLines("Data/en_US.news.txt", n=300)
        Predic <<- paste(ParsedPhrase[length(ParsedPhrase)], sep = " ")
      }
    }
  }
  
  Predic <<- tolower(Predic)
  Predic <<- gsub( "[[:punct:]]", "", Predic )
  
  RawData <- c(twitter[grep(Predic, twitter)], blogs[grep(Predic, blogs)], news[grep(Predic, news)], Quizzes)#[grep(Predic, Quizzes)])
  rm(twitter, blogs, news)
  Pred.Corpus <- Corpus(VectorSource(RawData))
  rm(RawData)
  Pred.Corpus <- tm_map(Pred.Corpus, content_transformer(tolower))
  Pred.Corpus <- tm_map(Pred.Corpus, removeNumbers)
  Pred.Corpus <- tm_map(Pred.Corpus, removePunctuation)
  Pred.Corpus <- tm_map(Pred.Corpus, stripWhitespace)
  
  ngramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = gram, max = gram))
  tdmNgram <- TermDocumentMatrix(Pred.Corpus, control = list(tokenize = ngramTokenizer))
  mat.dtmNgram <- as.matrix(tdmNgram)
}

BuildDF <- function(SearchPhrase)
{
  #Parse search phrase and then get the last 3 words
  tmp <- vector(mode = "numeric")
  ParsedPhrase <<- as.vector(strsplit(SearchPhrase, " ")[[1]])
  ParsedPhrase <<- ParsedPhrase[nchar(ParsedPhrase)>0]
  
  if(length(ParsedPhrase) > 3)
  {
    mat.dtmNgram <- BuildMat(4)
    tmp <- rowSums(mat.dtmNgram)
    tmp <- tmp[grep(paste("^", Predic, sep="", collapse="|"), rownames(mat.dtmNgram))]  ##checkpoint, if 0 then reduce ngram
  }
  else
  {
    if (length(tmp) == 0 & length(ParsedPhrase) >= 2)
    {
      mat.dtmNgram <- BuildMat(3)
      tmp <- rowSums(mat.dtmNgram)
      tmp <- tmp[grep(paste("^", Predic, sep="", collapse="|"), rownames(mat.dtmNgram))]  ##checkpoint, if 0 then reduce ngram
    }
    else
    {
      if (length(tmp) == 0 & length(ParsedPhrase) < 2)
      {
        mat.dtmNgram <- BuildMat(2)
        tmp <- rowSums(mat.dtmNgram)
        tmp <- tmp[grep(paste("^", Predic, sep="", collapse="|"), rownames(mat.dtmNgram))]  ##checkpoint, if 0 then reduce ngram
      }
    }
  }
  
  df <- as.data.frame(cbind(rownames(mat.dtmNgram), rowSums(mat.dtmNgram), rowSums(mat.dtmNgram)/dim(mat.dtmNgram)[2]))
  rownames(df) <- NULL
  df <- df[grep(paste("^", Predic, sep="", collapse="|"), rownames(mat.dtmNgram)),]
  
  t <- character(length(df[,1]))
  if (length(df[,1])>0)
  {
    for (i in 1:length(df[,1]))
    {
      if (i<1000)
      {
        t[i] <- as.vector(strsplit(as.character(df[,1]), " "))[[i]][n] #need to adjust the last parameter to ngram+1
      }
      else
      {
        break
      }
    }
    
    freq.df <- as.data.frame(cbind(t, as.numeric(as.character(df[,2]))))
    freq.df$Probability <- (as.numeric(as.character(freq.df[,2]))/sum(as.numeric(as.character(freq.df[,2]))))
    colnames(freq.df) <- c("Term", "Support", "Probability")
    
    head(freq.df[order(as.numeric(as.character(freq.df[,2])), decreasing=T),c(1,3)],5)
  }
  else
  {
    data.frame("Result"="No results found", "Probability"=0)
  }
  
}

shinyServer(
  function(input, output, session) {
      
    output$pred1 <- renderText({
        input$predict
        isolate(DF <<- BuildDF(input$pred))
        as.character(DF[1,1])
    })
    output$predPlot <- renderPlot({
      input$predict
      #isolate(DF <<- BuildDF(input$pred))
      #barplot(DF[,2], names.arg=DF[,1], ylab="Weight", xlab="Words", main="Top 5 word prediction")
      isolate(barplot(DF[,2], names.arg=DF[,1], ylab="Weight", xlab="Words", main="Top 5 word prediction"))
    })
    
  }
)