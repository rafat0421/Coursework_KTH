import numpy as np
import pandas as pd
# For visualizations
import matplotlib.pyplot as plt
# For regular expressions
import re
# For handling string
import string
# For performing mathematical operations
import math
# For cleaning dataset
import html
# Importing spacy
import spacy
## Use dictionary
# Dictionary of English Contractions
contractions_dict = { "ain't": "are not","'s":" is","aren't": "are not",
                     "can't": "cannot","can't've": "cannot have",
                     "'cause": "because","could've": "could have","couldn't": "could not",
                     "couldn't've": "could not have", "didn't": "did not","doesn't": "does not",
                     "don't": "do not","hadn't": "had not","hadn't've": "had not have",
                     "hasn't": "has not","haven't": "have not","he'd": "he would",
                     "he'd've": "he would have","he'll": "he will", "he'll've": "he will have",
                     "how'd": "how did","how'd'y": "how do you","how'll": "how will",
                     "I'd": "I would", "I'd've": "I would have","I'll": "I will",
                     "I'll've": "I will have","I'm": "I am","I've": "I have", "isn't": "is not",
                     "it'd": "it would","it'd've": "it would have","it'll": "it will",
                     "it'll've": "it will have", "let's": "let us","ma'am": "madam",
                     "mayn't": "may not","might've": "might have","mightn't": "might not", 
                     "mightn't've": "might not have","must've": "must have","mustn't": "must not",
                     "mustn't've": "must not have", "needn't": "need not",
                     "needn't've": "need not have","o'clock": "of the clock","oughtn't": "ought not",
                     "oughtn't've": "ought not have","shan't": "shall not","sha'n't": "shall not",
                     "shan't've": "shall not have","she'd": "she would","she'd've": "she would have",
                     "she'll": "she will", "she'll've": "she will have","should've": "should have",
                     "shouldn't": "should not", "shouldn't've": "should not have","so've": "so have",
                     "that'd": "that would","that'd've": "that would have", "there'd": "there would",
                     "there'd've": "there would have", "they'd": "they would",
                     "they'd've": "they would have","they'll": "they will",
                     "they'll've": "they will have", "they're": "they are","they've": "they have",
                     "to've": "to have","wasn't": "was not","we'd": "we would",
                     "we'd've": "we would have","we'll": "we will","we'll've": "we will have",
                     "we're": "we are","we've": "we have", "weren't": "were not","what'll": "what will",
                     "what'll've": "what will have","what're": "what are", "what've": "what have",
                     "when've": "when have","where'd": "where did", "where've": "where have",
                     "who'll": "who will","who'll've": "who will have","who've": "who have",
                     "why've": "why have","will've": "will have","won't": "will not",
                     "won't've": "will not have", "would've": "would have","wouldn't": "would not",
                     "wouldn't've": "would not have","y'all": "you all", "y'all'd": "you all would",
                     "y'all'd've": "you all would have","y'all're": "you all are",
                     "y'all've": "you all have", "you'd": "you would","you'd've": "you would have",
                     "you'll": "you will","you'll've": "you will have", "you're": "you are",
                     "you've": "you have"}
#Function
def showText():
    for index,text in enumerate(trainData['Clean'][35:40]):
        print('Review %d:\n'%(index+1),text)

def url_remove(text):
    url_pattern = re.compile(r'https?://\S+|www\.\S+')
    return url_pattern.sub(r'', text)

def html_remove(text):
    html_pattern = re.compile('<.*?>')
    return html_pattern.sub(r'', text)
    # Function for expanding contractions

def expand_contractions(text,contractions_dict=contractions_dict):
  def replace(match):
    return contractions_dict[match.group(0)]
  return contractions_re.sub(replace, text)


# Importing dataset
trainData=pd.read_csv('Corona_NLP_test.csv', encoding='latin-1') 
# print("Format Train data=>",trainData.shape)

# testData=pd.read_csv('Corona_NLP_test.csv', encoding='latin-1') 
# print("Test data=>",testData.shape)

trainData=trainData[['UserName','OriginalTweet','Sentiment']]

# print("Train Data Shape =>",trainData.shape)
# print(trainData.head(5))

# trainData.dropna(inplace=True)
# trainData.isnull().sum()

# for index,text in enumerate(trainData['OriginalTweet'][35:40]):
#   print('Review %d:\n'%(index+1),text)

# Regular expression for finding contractions
contractions_re=re.compile('(%s)' % '|'.join(contractions_dict.keys()))

# Expanding Contractions in the tweets
trainData['Clean']=trainData['OriginalTweet'].apply(lambda x:url_remove(x))
trainData['Clean']=trainData['Clean'].apply(lambda x: html.unescape(x))
trainData['Clean']=trainData['Clean'].apply(lambda x:html_remove(x))
trainData['Clean']=trainData['Clean'].apply(lambda x:expand_contractions(x))
# trainData['Clean']=trainData['Clean'].apply(lambda x: " ".join(re.findall('[A-Z][^A-Z]*(?!O)', x)))
# I want to split the words with multiple capitals except the word 'COVID' ... I tried the above line but it is not working
# trainData['Clean']=trainData['Clean'].apply(lambda x: " ".join(re.findall('[A-Z][^A-Z]*(?!(COVID|COVID19|COVID-19|COVID2019|COVID-2019|CoVid|CoViD|CoViD19|CoViD-19|CoViD2019|CoViD-2019))', x)))
# Split the words with multiple capitals
# trainData['Clean']=trainData['Clean'].apply(lambda x: " ".join(re.findall('[A-Z][^A-Z]*', x)))
trainData['Clean']=trainData['Clean'].apply(lambda x: x.lower())
trainData['Clean']=trainData['Clean'].apply(lambda x: re.sub('\w*\d\w*','', x))
trainData['Clean']=trainData['Clean'].apply(lambda x: re.sub('[%s]' % re.escape(string.punctuation), ' ', x))
trainData['Clean']=trainData['Clean'].apply(lambda x: re.sub(' +',' ',x))

# showText()
# Loading model
nlp = spacy.load('en_core_web_sm',disable=['parser', 'ner'])
# Lemmatization with stopwords removal
trainData['Lemmatized']=trainData['Clean'].apply(lambda x: ' '.join([token.lemma_ for token in list(nlp(x)) if (token.is_stop==False)]))
exportData = trainData[['UserName','Sentiment', "Lemmatized"]]
exportData.to_csv('Test_Clean.csv', index=False)
