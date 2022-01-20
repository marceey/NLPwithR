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
nypostarticle = nypostarticle[2:13]
nypostarticle

thehillarticle = thehillarticle[2:16]
thehillarticle

##Some basic checks
#Check UTF-8 encoding: aiming for character(0)
nypostarticle[!utf8_valid(nypostarticle)] 
thehillarticle[!utf8_valid(thehillarticle)]

#Check character normalization. Specifically, the normalized composed form (NFC): 0 means all right. The text is in NFC.
nypostarticle_NFC <- utf8_normalize(nypostarticle)
sum(nypostarticle_NFC != nypostarticle)
thehillarticle_NFC <- utf8_normalize(thehillarticle)
sum(thehillarticle_NFC != thehillarticle)

#replace doubled or more spaces by unique spaces
nypostarticle <- gsub("[ ]{2,}", " ", nypostarticle)
nypostarticle
thehillarticle <- gsub("[ ]{2,}", " ", thehillarticle)
thehillarticle

#install spacy and english language model
spacy_install() 
spacy_download_langmodel('en') 

#Gets single non-empty sentences/phrases from the extracted paragraphs of NYPost article
nypostarticlephrases <- spacy_tokenize(nypostarticle, what="sentence")
v_nypostarticlephrases <- unlist(nypostarticlephrases)
numnypostarticlephrases <- length(v_nypostarticlephrases) #21
sum(v_nypostarticlephrases=="") #1 sentence empty
v_nypostarticlephrases <- v_nypostarticlephrases[-which(v_nypostarticlephrases=="")] #20 sentences
v_nypostarticlephrases

#repeat for TheHill article
thehillarticlephrases <- spacy_tokenize(thehillarticle, what ="sentence")
v_thehillarticlephrases <- unlist(thehillarticlephrases)
numthehillarticlephrases <- length(v_thehillarticlephrases) #48
sum(v_thehillarticlephrases=="") #17
v_thehillarticlephrases <- v_thehillarticlephrases[-which(v_thehillarticlephrases=="")]
v_thehillarticlephrases


