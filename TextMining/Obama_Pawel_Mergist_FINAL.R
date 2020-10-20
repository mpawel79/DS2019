### Example to sum up the lab & more visualisations
#This example is based on books "Mastering machine learning with R".

library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(arulesSequences)

slide <-function(slide_num, title, txt=c('Text Mining','DataScience ed.4 2018-2019', 'Pawel Mergist' ))
{
  
  par(mar = c(0,0,0,0))
  plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
  
  text(x = 0.505, y = 0.995, paste(title),  cex = 1.8, col = "grey")
  text(x = 0.5, y = 1.0, paste(title),  cex = 1.8, col = "black")
  
  y = 0.85
  for (t in txt)
  {
    text(x = 0.1, y = y, paste(t),  cex = 1.5, col = "black", adj = c(0,0))
    y = y-0.1
  }
  par(mar = c(2,2,2,2))
}

next_slide <-function(sn){sn = sn +1 }
slide_num = 0

show_slide<-function(slide_num, title, txt)
{
  slide(slide_num, title, txt)
  
  file_name = sprintf("slide_%02d.jpg", slide_num)
  jpeg(file_name,  width = 800, height = 600)
  
  slide(slide_num, title, txt)
  
  dev.off()
}

start_plot<-function(slide_num)
{
  file_name = sprintf("slide_%02d.jpg", slide_num)
  jpeg(file_name,  width = 800, height = 600)
}

end_plot <-function()
{  dev.off()}
#slide_num <<- next_slide(slide_num)

## SLIDE -- TYTUL
slide_num = 0
show_slide(slide_num, "****", c('Text Mining','DataScience ed.4 2018-2019', 'Pawel Mergist' ))
slide_num <<- next_slide(slide_num)

if(TRUE)
{
  
  sou2010 = paste(scan(url("http://textuploader.com/a5vq4/raw"), what="character"),collapse=" ")
  sou2010=iconv(sou2010, "latin1", "ASCII", "")
  sou2010
  
  sou2011 = paste(scan(url("http://textuploader.com/a5vm0/raw"), what="character"),collapse=" ")
  sou2011=iconv(sou2011, "latin1", "ASCII", "")
  
  sou2012 = paste(scan(url("http://textuploader.com/a5vmp/raw"), what="character"),collapse=" ")
  sou2012=iconv(sou2012, "latin1", "ASCII", "")
  
  sou2013 = paste(scan(url("http://textuploader.com/a5vh0/raw"), what="character"),collapse=" ")
  sou2013=iconv(sou2013, "latin1", "ASCII", "")
  
  sou2014 = paste(scan(url("http://textuploader.com/a5vhp/raw"), what="character"),collapse=" ")
  sou2014=iconv(sou2014, "latin1", "ASCII", "")
  
  sou2015 = paste(scan(url("http://textuploader.com/a5vhb/raw"), what="character"),collapse=" ")
  sou2015=iconv(sou2015, "latin1", "ASCII", "")
  sou2015
  getwd()
  
  write.table(sou2010, file.path(getwd(), "TM", "text-data", "text-tm-2","sou2010.txt"))
  write.table(sou2011, file.path(getwd(), "TM", "text-data", "text-tm-2","sou2011.txt"))
  write.table(sou2012, file.path(getwd(), "TM", "text-data", "text-tm-2","sou2012.txt"))
  write.table(sou2013, file.path(getwd(), "TM", "text-data", "text-tm-2","sou2013.txt"))
  write.table(sou2014, file.path(getwd(), "TM", "text-data", "text-tm-2","sou2014.txt"))
  write.table(sou2015, file.path(getwd(), "TM", "text-data", "text-tm-2","sou2015.txt"))
}

## SLIDE AGENDA -CO
show_slide(slide_num, "Analiza tekstu", 
           c('Przemowienia Baracka Obamy z lat:', '2010-2015',
             'Przemowienia Donalda Trumpa z lat:', '2015-2019'  ))
slide_num <<- next_slide(slide_num)

# SLIDE AGENA - METODY
show_slide(slide_num, "Metody:", 
           c('Czestosc wystepowania slow',
             'Zbieznosc wystepowania',
             'Rozklad sentymentu',
             'Przeksztalcenie do postaci transakcyjej i ',
             'Sprawdzenie wsparc wystepowania slow jako regul'))
slide_num <<- next_slide(slide_num)

name = file.path(getwd(), "TM", "text-data", "text-tm-2")
length(dir(name))
dir(name)

name
docs = Corpus(DirSource(name)) 
docs

docs = tm_map(docs, tolower)
docs = tm_map(docs, removeNumbers)
docs = tm_map(docs, removePunctuation)
docs = tm_map(docs, removeWords, stopwords("english"))
docs = tm_map(docs, stripWhitespace)
docs = tm_map(docs, stemDocument)
docs = tm_map(docs, removeWords, c("applaus","can","cant","will","that","weve",
                                   "dont","wont")) 

#docs <- tm_map(docs, PlainTextDocument)
docs
tm::inspect(docs)

#docs[["content"]][["content"]]

tdm <- TermDocumentMatrix(docs)
dtm <- DocumentTermMatrix(docs)
dim(dtm)
dtm = removeSparseTerms(dtm, 0.21)
dim(dtm)
rownames(dtm) = c("2010","2011","2012","2013","2014","2015")
tm::inspect(dtm[1:6, 1:5])

m_dtm = as.matrix(dtm)
dtm

freq = colSums(as.matrix(dtm))
freq
ord = order(-freq) #order the frequency
freq[head(ord)]

s_out= toString(capture.output(freq[head(ord)]))
s_out
s_num = toString(as.numeric(freq[head(ord)]))
s_num

# SLIDE AGENA - WYSTEPOWANIE NAJCZESCIEJ
show_slide(slide_num, "Czestosc wystepowania - najczesciej", 
           c('',
             s_out,
             s_num))
slide_num <<- next_slide(slide_num)


# SLIDE - BARPLOT - LICZNOSC SLOW
par(mar = c(2,2,2,2))
barplot(freq[head(ord)], main='Licznosc najczesciej wystepujacych slow')

start_plot(slide_num)
# Bar plot ilosci slow
par(mar = c(2,2,2,2))

barplot(freq[head(ord)], main='Licznosc najczesciej wystepujacych slow')
end_plot()
slide_num <<- next_slide(slide_num)


# NAJRZADZIEJ
freq[tail(ord)]
head(table(freq))
tail(table(freq))

s_out= toString(capture.output(freq[tail(ord)]))
s_out
s_num = toString(as.numeric(freq[tail(ord)]))
s_num

# SLIDE AGENA - WYSTEPOWANIE NAJRZADZIEJ
show_slide(slide_num, "Czestosc wystepowania - najrzadziej", 
           c('',
             s_out,
             s_num))
slide_num <<- next_slide(slide_num)


# SLIDE - BARPLOT - LICZNOSC SLOW
par(mar = c(2,2,2,2))
barplot(freq[tail(ord)], main='Licznosc najrzadziej wystepujace slowa')

start_plot(slide_num)
# Bar plot ilosci slow
par(mar = c(2,2,2,2))

barplot(freq[head(ord)], main='Licznosc najrzadziej wystepujacych slow')
end_plot()
slide_num <<- next_slide(slide_num)

# Assocjacje - korelacje ktore slowa wystepuja z jakimi

# SLIDE - WORDS
slide_asos<-function(word_asos)
{
  
  findFreqTerms(dtm, 100)
  c = findAssocs(dtm, word_asos, corlimit=0.83)
  words = names(unlist(c))
  words  =  removeWords(words,sprintf("%s.", word_asos))
  
  # SLIDE AGENA -  korelacje ktore slowa wystepuja z jakimi
  show_slide(slide_num, sprintf("Assocjacje - dla slowa: %s", word_asos), 
             c('Metoda findFreqTerms corlimit = 0.83', words))
  slide_num <<- next_slide(slide_num)
}
slide_asos('god')
slide_asos('nation')
slide_asos('love')

## SLIDE WORD CLOUD
start_plot(slide_num)
wordcloud(names(freq), freq, min.freq=50,scale=c(3, .5), colors=brewer.pal(6, "Dark2"))
end_plot()
slide_num <<- next_slide(slide_num)


## SLIDE WORD CLOUD
start_plot(slide_num)
freq = sort(colSums(as.matrix(dtm)), decreasing=TRUE)
freq
wf = data.frame(word=names(freq), freq=freq)
wf = wf[1:10,]

wf
par(mar = c(3,3,3,3))
barplot(wf$freq, names=wf$word, main="Word Frequency",        xlab="Words", ylab="Counts", ylim=c(0,250))
end_plot()
slide_num <<- next_slide(slide_num)


##
## Spilit to sentences
## https://stackoverflow.com/questions/18712878/r-break-corpus-into-sentences
require(tm)
require(NLP)
require(openNLP)

convert_text_to_sentences <- function(text, lang = "en") {
  # Function to compute sentence annotations using the Apache OpenNLP Maxent sentence detector employing the default model for language 'en'. 
  sentence_token_annotator <- Maxent_Sent_Token_Annotator(language = lang)
  
  # Convert text to class String from package NLP
  text <- as.String(text)
  
  # Sentence boundaries in text
  sentence.boundaries <- annotate(text, sentence_token_annotator)
  
  # Extract sentences
  sentences <- text[sentence.boundaries]
  
  # return sentences
  return(sentences)
}
chunk_into_sentences <- function(text) {
  break_points <- c(1, as.numeric(gregexpr('[[:alnum:] ][.!?]', text)[[1]]) + 1)
  sentences <- NULL
  for(i in 1:length(break_points)) {
    res <- substr(text, break_points[i], break_points[i+1]) 
    if(i>1) { sentences[i] <- sub('. ', '', res) } else { sentences[i] <- res }
  }
  sentences <- sentences[sentences=!is.na(sentences)]
  return(sentences)
}


#
# ROZKALD SENTYMENTOW W POSZCZEGOLNYCH PRZEMOWIENIACH
#
#sou2010_sentences <- convert_text_to_sentences(sou2010)
##Sentiment Analysis
hu.liu.pos = scan(file.path(getwd(), "TM", "text-data", "opinion-lexicon-English","positive-words.txt"),
                  what='character', comment.char=';')
hu.liu.neg = scan(file.path(getwd(), "TM", "text-data", "opinion-lexicon-English","negative-words.txt"),
                  what='character', comment.char=';')
pos.words = c(hu.liu.pos, 'upgrade')
neg.words = c(hu.liu.neg, 'wtf', 'wait', 'waiting',
              'epicfail', 'mechanical')

pos.words
neg.words

score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  
  # we got a vector of sentences. plyr will handle a list
  # or a vector as an "l" for us
  # we want a simple array of scores back, so we use
  # "l" + "a" + "ply" = "laply":
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    # clean up sentences with R's regex-driven global substitute, gsub():
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
    
    # and convert to lower case:
    sentence = tolower(sentence)
    # split into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+')
    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}


y  = 2010
df_sou201X = data.frame(score =0, text="text", year=2010)
df_sou201X
for (sou201X in c(sou2010, sou2011, sou2012, sou2013, sou2014, sou2015))
{
  sou2010_set <- chunk_into_sentences(sou201X)
  head(sou2010_set)
  
  result = score.sentiment(sou2010_set, pos.words, neg.words)
  result$year = y
  head(result)
  
  min(result$score)
  hist(result$score)
  
  #boxplot(result$score)
  
  y = y + 1
  
  df_sou201X = rbind(df_sou201X, result)
}

# box plot sentymentu dla roznych przemowien


## SLIDE ROZKALD SENTYMENTU
boxplot(df_sou201X$score ~df_sou201X$year, main='Rozklad sentymetu w poszczegolnych latach')

start_plot(slide_num)
boxplot(df_sou201X$score ~df_sou201X$year,main='Rozklad sentymetu w poszczegolnych latach')
end_plot()
slide_num <<- next_slide(slide_num)
slide_num

library(arules) #regu??y asocjacyjne
library(arulesViz) # wizualizacja regu??


#vs = VectorSource(df_sou201X$text)
#vs[[10]]

text = df_sou201X$text

text

text = tolower(text)
text = removePunctuation(text)
text = removeNumbers(text)
text = stripWhitespace(text)
text = removeWords(text,c("applaus","can","cant","will","that","weve",
                          "dont","wont", "and", 'the', "of", 'a', 'we', 'some', 'are', 'it'))

text


dataTR_with_frequent <- as(strsplit(text, " "), "transactions")

# SLIDE - ANALIZA TRANSAKCJI - WSTEP
show_slide(slide_num, "Analiza przemowienia jako zbior transakcji", 
           c('',
             'Poszukiwanie slown ktore z pewna',
             'czestoscia wystepuja w zdaniach',
             '',
             'Przeglad transakcji',
              'Zdania jako transakcje',
             'Slowa - jako elementy transakcji',
             ''))

slide_num <<- next_slide(slide_num)

# SLIDE - ANALIZA TRANSAKCYJNA
image(head(dataTR_with_frequent,10000), main='Preglad wystepowania slow w zdaniach')

start_plot(slide_num)
image(head(dataTR_with_frequent,10000), main='Preglad wystepowania slow w zdaniach')
end_plot()
slide_num <<- next_slide(slide_num)

## sprawdzenie co jest czeste
aParam  = new("APparameter", "target" = "frequent items", "confidence" = 0.0001,"support" = 0.001, "minlen"= 1, "maxlen"=1, maxtime = 100000) 
print(aParam)

# wykrywanie regul z aktualnymi parametrami
asets <-apriori(dataTR_with_frequent,aParam)
inspect(asets)


tmp = head(sort(asets, by=c('support'), decreasing = TRUE), 10)
inspect(tmp)
str(tmp)
freq_words = tmp@items@itemInfo$labels[tmp@items@data@i+1]


#df_tmp <- as(tmp,'data.frame')
#freq_words = as(tmp, 'vector')
freq_words
#remove frequent and empty words
#freq_words = unlist(tmp)
freq_words = unique(freq_words)
freq_words = freq_words[freq_words != ""]

text_freq_cut = removeWords(text,freq_words)

text_freq_cut

dataTR <- as(strsplit(text_freq_cut, " "), "transactions")

# !!WALKAROUND TEXT
#dataTR <- as(strsplit(text, " "), "transactions")
# START ANALIZY

# Najczestrze sformulowania -licznosci -3-4
aParam  = new("APparameter", "target" = 'frequent itemsets', "confidence" = 0.1, "support" =0.001, "minlen"= 7)
asets <-apriori(dataTR,aParam)

top_best_support = head(sort(asets, by="support", decreasing = TRUE), 10)
inspect(top_best_support)


# SLIDE - GRUPY SLOW
plot(top_best_support, method = 'graph', main = 'Grupy slow')

start_plot(slide_num)
plot(top_best_support, method = 'graph', main = 'Grupy slow')
end_plot()
slide_num <<- next_slide(slide_num)
# END PLOT



# 3. Sprawdzam parametr wiarygodnosc, i sortuje dodatkowow po po podniesieniu

aParam  = new("APparameter", "target" = "rules",  "confidence" = 0.51, "support" = 0.001, "minlen"= 7, "maxlen"=200, maxtime = 10000) 
print(aParam)

# wykrywanie regul z aktualnymi parametrami
asets <-apriori(dataTR,aParam)

#asets<- subset(asets, subset = rhs %in% "nation" & lift >=0.1)

tmp = head(sort(asets, by=c('confidence','lift', 'support'), decreasing = TRUE), 15)
#tmp = head(sort(asets, by=c('support','confidence'), decreasing = TRUE), 25)
inspect(tmp)


plot(tmp, method = 'graph')

tmp = head(sort(asets, by="support", decreasing = TRUE), 15)
inspect(tmp)

plot(tmp, method = 'graph')

# SLIDE - Pewnosc - kt slowa wystapia jesli ma wystapic dane slowo
show_slide(slide_num, "Kt slowa wystapia jesli ma wystapic dane slowo", 
           c('lhs rhs         support     confidence lift     count',
             '{,businesses,create,here,jobs,overseas} => {ship}  ',
             '{,american,asian,latino,native,white}   => {black}',
             'Te razem:',
             '{,american,asian,latino,native,white,black}',
             'A takze:',
             '{family,made,strong,through,times,very} => {tightknit}',
             

             ''))

slide_num <<- next_slide(slide_num)


# SLIDE - GRUPY SLOW
plot(top_best_support, method = 'graph', main = 'Slowa ktore wystapia czesto razem')

start_plot(slide_num)
plot(top_best_support, method = 'graph', main = 'Slowa ktore wystapia czesto razem')

end_plot()
slide_num <<- next_slide(slide_num)
# END PLOT


#
# Slow warte uwagi
#

special_word <- function(word = 'nation', minlen=4, max_head = 10)
{
  
  aParam  = new("APparameter", "target" = "rules",  "confidence" = 0.1, "support" = 0.001, "minlen"= minlen, "maxlen"=200, maxtime = 10000) 
  print(aParam)
  
  # wykrywanie regul z aktualnymi parametrami
  asets <-apriori(dataTR,aParam)
  
  #tmp = head(sort(asets, by=c('support','confidence'), decreasing = TRUE), 10)
  #inspect(tmp)
  
  rulesInGivenConseq<- subset(asets, subset = rhs %in% word & lift >=0.1)
  set_sorted_limited <- head(sort(rulesInGivenConseq, by="support", decreasing = TRUE), max_head)
  inspect(set_sorted_limited)
  plot(set_sorted_limited, method = 'graph', main = word)
}


# SLIDE - Wybrane dla ktorych poprzedniki moga byc ciekawe
show_slide(slide_num, "Wybrane dla ktorych poprzedniki moga byc ciekawe", 
           c('nation', 'world', 'future', 'history', 'equal', 'education', 'kids',
             
             
             ''))

words = c('nation', 'world', 'future', 'history', 'equal', 'education', 'kids')
for (w in words)
{
  slide_num <<- next_slide(slide_num)
  
  special_word(w, max_head = 5)
  
  start_plot(slide_num)
  special_word(w, max_head = 5)
  end_plot()
  slide_num <<- next_slide(slide_num)
}


# END ##
special_word('world', minlen=5)

start_plot(slide_num)
special_word('future', minlen=3)

start_plot(slide_num)
special_word('history', minlen=3)
special_word('equal', minlen=3)
special_word('education', minlen=7)
special_word('kids', minlen=2)


# ANALIZA GSP - SPADE - WYLACZONE BO ZA DLUGO TRWA
if (FALSE)
{
#split to words
word.list = str_split(text, '\\s+')
words_vector = unlist(word.list)
# end split to words

#df_textTR <- as(df_textTR, "transactions")


## sekwencje
trans
text = text_freq_cut
df_text = data.frame(sequenceID = rep(1, length(text)), eventID = 1:length(text),SIZE = rep(1,length(text)), items=(text))  
head(df_text,2)

for (i in 1:length(text))
{
  #print(df_text[df_text$eventID == i, 'SIZE'])
  t = df_text[df_text$eventID == i, 'items']
  l = length(unlist((str_split(t, '\\s+'))))
  df_text[df_text$eventID == i, 'SIZE'] = l
  # print (l)
  
}

head(df_text,2)

df_text_100 = head(df_text, 5)
write.table(df_text_100, "mytxtout.txt", sep=" ", row.names = FALSE, col.names = FALSE, quote = FALSE)
trans_matrix <- read_baskets("mytxtout.txt", sep = " ", info = c("sequenceID","eventID", "SIZE",'items'))

#df_textTR

s1 <- cspade(trans_matrix, parameter = list(support = 0.01, maxsize = 3, maxlen=4, mingap=1, maxgap=10, maxwin=15), control = list(verbose = TRUE))
s1.df <- as(s1, "data.frame")
summary(s1)
s1.df
inspect(s1)
r1 <- as(ruleInduction(s1, confidence = 0.1, control = list(verbose = TRUE)), "data.frame")
inspect(ruleInduction(s1, confidence = 0.001, control = list(verbose = TRUE)))


#zaki

zaki = trans_matrix
s0 <- cspade(zaki, parameter = list(support = 0,
                                    maxsize = 1000, maxlen = 1000),
             control   = list(verbose = TRUE))
as(s0, "data.frame")
## mine frequent sequences
s1 <- cspade(zaki, parameter = list(support = 0.4), 
             control   = list(verbose = TRUE, tidLists = TRUE))
summary(s1)
as(s1, "data.frame")

}