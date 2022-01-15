#installing packages if not done yet
install.packages("rvest")
install.packages("dplyr")

#loading libraries
library(rvest)
library(dplyr)
library(utf8)
library(spacyr)

##Webscraping the two relevant news articles
#create new variable for the link and get html document of this webpage
nypostlink = "https://nypost.com/2022/01/11/bidens-disgraceful-lies-on-filibuster-dems-power-grab-over-us-voting-laws/"
nypostpage = read_html(nypostlink)

#Scrape the article from the webpage using pipe operators with CSS tag from SelectorGadget
nypostarticle = nypostpage %>% html_nodes("p") %>% html_text()
nypostarticle

#repeat for the second article
thehilllink = "https://thehill.com/opinion/campaign/589286-bidens-georgia-speech-was-a-call-to-save-democracy-as-we-know-it?rl=1"
thehillpage = read_html(thehilllink)

thehillarticle = thehillpage %>% html_nodes("p") %>% html_text()
thehillarticle

#remove lines that are not part of the main article
nypostarticleQ = nypostarticle[2:13]
nypostarticleQ

thehillarticleQ = thehillarticle[2:17]
thehillarticleQ

##Some basic checks
#Check UTF-8 encoding: aiming for character(0)
nypostarticleQ[!utf8_valid(nypostarticleQ)] 
thehillarticleQ[!utf8_valid(thehillarticleQ)]


#Check character normalization. Specifically, the normalized composed form (NFC): 0 means all right. The text is in NFC.
nypostarticleQ_NFC <- utf8_normalize(nypostarticleQ)
sum(nypostarticleQ_NFC != nypostarticleQ)
thehillarticleQ_NFC <- utf8_normalize(thehillarticleQ)
sum(thehillarticleQ_NFC != thehillarticleQ)

#replace doubled or more spaces by unique spaces
nypostarticleQQ <- gsub("[ ]{2,}", " ", nypostarticleQ)
nypostarticleQQ
thehillarticleQQ <- gsub("[ ]{2,}", " ", thehillarticleQ)
thehillarticleQQ

spacy_install() 
spacy_download_langmodel('en') 

#Gets sentences from paragraphs
nypostarticlephrases <- spacy_tokenize(nypostarticleQQ, #If you use quanteda you can use
                                       # corpus_reshape(corpus, to = "sentences"))
                                       #Taks a while.
                                       #Returns a list with 138 elements, each one
                                       # is a string vector.
                                       what="sentence" #By default remove_separators = TRUE
                                       # (removes trailing spaces)
)

v_phrases <- unlist(nypostarticlephrases)
numphrases <- length(v_phrases) #8,975 sentences
sum(v_phrases=="") #1
v_phrases <- v_phrases[-which(v_phrases=="")] #8,974 sentences



