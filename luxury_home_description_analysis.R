############ This is an analysis on differences between LA luxury and NYC luxury based on property listing descriptions
############ The listings are beyond $ 1MM in LA and NYC markets, the analysis was published on 
setwd('/Users/lguo/Documents/tm.analysis/')
library(tm)
library(SnowballC)
library(RWeka)
library(wordcloud)
library(stringr)
  # parse LA file 
  LA <- readLines('./LA/LALuxuryDesc_test1_042915.txt')
  text <- tolower(LA)
  LA.sub <- sapply(1:length(LA), function(i) {
    LA.s <- LA[i]
    LA.reg <-
      regexpr(
        'public[[:space:]]?remarks\\|\\^[a-z0-9|[:punct:]|[:space:]]{10,}\\|\\?\\|\\^',
        LA.s
      )
    LA.mat <- regmatches(LA.s, LA.reg)
    pos <- data.frame(str_locate_all(LA.mat, '\\|\\?\\|\\^'))
    end <- pos[1, 1] - 1
    LA.fin <- substr(LA.mat, 1, end)
    LA.fin
  })
  # create text corpus for processing
  LA <- Corpus(VectorSource(LA.sub))
  # process corpus
  LA.tm <-
    tm_map(LA, content_transformer(function(x)
      iconv(x, to = 'UTF-8-MAC', sub = 'byte')), mc.cores = 1)
  LA.tm <- tm_map(LA.tm, content_transformer(tolower), mc.cores = 1)
  ##LA.tm<-tm_map(LA.tm,removeNumbers,mc.cores=1)
  LA.tm <- tm_map(LA.tm, removePunctuation, mc.cores = 1)
  LA.tm <- tm_map(LA.tm, stripWhitespace, mc.cores = 1)
  sw <- stopwords('en')
  sw <-
    c(
      sw,
      'public remarksthis',
      'cdata',
      'baths',
      'bathrooms',
      'features',
      'bedrooms',
      'bedroom',
      'bathroom',
      'living',
      'bed',
      'bath',
      'kitchen',
      'bed',
      'live',
      'room',
      'square feet',
      'garage',
      'feature',
      'plan',
      'sq',
      'ft'
    )
  LA.tm <- tm_map(LA.tm, removeWords, sw, mc.cores = 1)
  # tokenize text corpus
  bigramTokenizer <-
    function(x)
      NGramTokenizer(x, Weka_control(min = 2, max = 4))
  options(mc.cores = 1)
  # create document matrix and find most 
  LA.mx <-
    TermDocumentMatrix(LA.tm, control = list(tokenize = bigramTokenizer))
  LAfreq <- findFreqTerms(LA.mx, 60, Inf)
  LA.dt <- data.frame(as.matrix(LA.mx[LAfreq, ]))
  LA.dt$count <- rowSums(LA.dt)
  LA.dt <- LA.dt[, c(1, ncol(LA.dt))]
  LA.dt <- LA.dt[with(LA.dt, order(count, decreasing = TRUE)), ]
  LA.dt
  # process NYC data
  NY <- readLines('./NYLuxuryDesc_test1_042915.txt')
  NY <- tolower(NY)
  # comment long
  NY.CL <- NY[grepl('commentslong', NY)]
  NY.sub1 <- sapply(1:length(NY.CL), function(i) {
    NY.s <- NY.CL[i]
    NY.reg <-
      regexpr('commentslong\\|\\^[a-z0-9|[:punct:]|[:space:]]{10,}\\|\\?mlsid',
              NY.s)
    NY.mat <- regmatches(NY.s, NY.reg)
    NY.mat
  })
  # ?descirption|
  NY.DS <- NY[grepl('\\?description\\|', NY)]
  NY.sub2 <- sapply(1:length(NY.DS), function(i) {
    NY.s <- NY.DS[i]
    NY.reg <-
      regexpr('\\?description\\|\\^[a-z0-9|[:punct:]|[:space:]]{10,}\\|\\?',
              NY.s)
    NY.mat <- regmatches(NY.s, NY.reg)
    pos <- data.frame(str_locate_all(NY.mat, '\\|\\?'))
    end <- pos[1, 1] - 1
    NY.fin <- substr(NY.mat, 1, end)
    NY.fin
  })
  # Markting Remarks
  NY.MR <- NY[grepl('\\?(agent|marketing) remark[s]\\|\\^', NY)]
  NY.sub3 <- sapply(1:length(NY.MR), function(i) {
    NY.s <- NY.MR[i]
    NY.reg <-
      regexpr(
        '\\?(agent|marketing) remark[s][a-z0-9|[:punct:]|[:space:]]{10,}\\|\\?',
        NY.s
      )
    NY.mat <- regmatches(NY.s, NY.reg)
    pos <- data.frame(str_locate_all(NY.mat, '\\|\\?'))
    end <- pos[1, 1] - 1
    NY.fin <- substr(NY.mat, 1, end)
    NY.fin
  })
  
  NY.sub <- c(NY.sub1, NY.sub2, NY.sub3)
  NY <- Corpus(VectorSource(NY.sub))
  
  NY.tm <-
    tm_map(NY, content_transformer(function(x)
      iconv(x, to = 'UTF-8-MAC', sub = 'byte')), mc.cores = 1)
  NY.tm <- tm_map(NY.tm, content_transformer(tolower), mc.cores = 1)
  ##LA.tm<-tm_map(LA.tm,removeNumbers,mc.cores=1)
  NY.tm <- tm_map(NY.tm, removePunctuation, mc.cores = 1)
  NY.tm <- tm_map(NY.tm, stripWhitespace, mc.cores = 1)
  sw <- stopwords('en')
  sw <-
    c(
      sw,
      'public remarksthis',
      'cdata',
      'baths',
      'bathrooms',
      'features',
      'bedrooms',
      'bedroom',
      'bathroom',
      'living',
      'bed',
      'bath',
      'kitchen',
      'bed',
      'live',
      'room',
      'square feet',
      'garage',
      'feature',
      'plan',
      'sq',
      'ft',
      'square foot',
      'new york',
      'amenities',
      'include'
    )
  NY.tm <- tm_map(NY.tm, removeWords, sw, mc.cores = 1)
  bigramTokenizer <-
    function(x)
      NGramTokenizer(x, Weka_control(min = 2, max = 4))
  options(mc.cores = 1)
  NY.mx <-
    TermDocumentMatrix(NY.tm, control = list(tokenize = bigramTokenizer))
  NYfreq <- findFreqTerms(NY.mx, 50, Inf)
  NYfreq
  NY.dt <- data.frame(as.matrix(NY.mx[NYfreq, ]))
  NY.dt$count <- rowSums(NY.dt)
  NY.dt <- NY.dt[, c(1, ncol(NY.dt))]
  NY.dt <- NY.dt[with(NY.dt, order(count, decreasing = TRUE)), ]
  write.csv(NY.dt, 'NY.csv')
  write.csv(LA.dt, 'LA.csv')
  