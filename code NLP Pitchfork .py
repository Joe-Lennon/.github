# -*- coding: utf-8 -*-
"""
Created on Sat Apr 18 17:16:17 2020

@author: Joe
"""

# -*- coding: utf-8 -*-
"""
Created on Sun Mar 15 10:33:54 2020

@author: Joe https://www.kaggle.com/nolanbconaway/is-the-first-album-the-best
https://www.digitalocean.com/community/tutorials/how-to-perform-sentiment
-analysis-in-python-3-using-the-natural-language-toolkit-nltk

https://cmdlinetips.com/2018/02/how-to-subset-pandas-dataframe-based-on-values-of-a-column/

"""

import sqlite3, datetime
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
import nltk
from scipy.stats import linregress
from scipy.stats import ttest_rel

pd.set_option('precision', 2)
np.set_printoptions(precision=2)

con = sqlite3.connect('C:/Users/Joe/Documents/CCSU/Python Class/FInal/pitchfork-data/database.sqlite')
""" Tables to Query """
artists = pd.read_sql('SELECT * FROM artists', con)
content= pd.read_sql('SELECT * FROM content', con)
genres = pd.read_sql('SELECT * FROM genres', con)
labels = pd.read_sql('SELECT * FROM labels', con)
scores = pd.read_sql('SELECT reviewid, score FROM reviews', con)
years = pd.read_sql('SELECT * FROM years', con)
reviewdetail = pd.read_sql('SELECT * FROM reviews', con)
con.close()

artists = artists.sort_values('reviewid', ascending=True)
artists  = artists .drop_duplicates(subset='reviewid', keep='first')

scores= scores.sort_values('reviewid', ascending=True)
scores= scores.drop_duplicates(subset='reviewid', keep='first')

rev_master  = pd.merge(scores, artists, on = 'reviewid')

# combine into score-artist mapping
rev_master = rev_master.sort_values('reviewid', ascending=True)
rev_master  = rev_master .drop_duplicates(subset='reviewid', keep='first')



""" Combine content with review and rating """
content= content.sort_values('reviewid', ascending=True)
content= content.drop_duplicates(subset='reviewid', keep='first')

rev_master = pd.merge(rev_master,  content, on = 'reviewid')
rev_master = pd.merge(rev_master,  years, on = 'reviewid')
rev_master = pd.merge(rev_master,  genres, on = 'reviewid')



size, scale = 2000, 20

reveiwnum = rev_master['score']

reveiwnum.plot.hist(grid=True, bins=20, rwidth=0.9,
                   color='#607c8e')
plt.title('Reviews by Score, All Reviews')
plt.xlabel('Counts')
plt.ylabel('N of Reviews')
plt.grid(axis='y', alpha=0.75)

####### COunt Reviews by Genre ####
# libraries

rev_master.groupby('genre')['score'].mean().plot(kind='bar', figsize=(10,8));
plt.xlabel('Genre')
plt.ylabel('Mean Score')
plt.title('Mean Scores by Genre');

rev_master.groupby('genre')['score'].count().plot(kind='barh', figsize=(10,8));
plt.xlabel('Number of Reviews')
plt.ylabel('Genre')
plt.title('Number of Reviews by Genre');

#### Info by Year #####

rev_master.groupby('year')['score'].count().plot(kind='line', figsize=(10,8));
plt.xlabel('Number of Reviews')
plt.ylabel('year')
plt.title('Number of Reviews by Year');


rev_master.groupby('year')['score'].mean().plot(kind='line', figsize=(10,8));
plt.xlabel('Mean Score')
plt.ylabel('year')
plt.title('Mean Scores by Year');


#### Make Some Tables
rev_master.describe()

# remove various artists
rev_master  = rev_master[rev_master.artist != 'various artists']

# remove multi-year reviews [re-releases]
year_counts = years.groupby('reviewid').count().reset_index()
keepers = year_counts.loc[year_counts.year == 1, 'reviewid']
rev_master = rev_master.loc[rev_master.reviewid.isin(keepers)]
rev_master =  rev_master[rev_master['year'] > 1985]
rev_master =  rev_master[rev_master['genre'] =='rock']

descsumm = rev_master.groupby('year')["score"].mean()
descsumm2 = rev_master.groupby('genre')["score"].mean()

#### Make Some Tables
rev_master.describe()


"""### Make a dataframe of BAD reviews"""
""" ROCK and 5.6, 7.5 has 80% """

badreviews =  rev_master[rev_master['score'] <= 5.6]
badreviewcontent = badreviews['content']
""" Convert to a list """
badrev_list = badreviewcontent.tolist()

"""### Make a dataframe of positive reviews"""
goodreviews =  rev_master[rev_master['score'] >= 7.5]
goodreviewcontent = goodreviews['content']
""" Convert to a list """
goodrev_list = goodreviewcontent.tolist()

### Histogram of Reviews
import pandas as pd
import numpy as np

size, scale = 2000, 20

reveiwnum = rev_master['score']

reveiwnum.plot.hist(grid=True, bins=20, rwidth=0.9,
                   color='#607c8e')
plt.title('Reviews by Score, Rock Reviews, 1985 to 2017')
plt.xlabel('Counts')
plt.ylabel('N of Reviews')
plt.grid(axis='y', alpha=0.75)



### Run exampledatacamp code on those lists
### Scale up
"""Example Sentiment
https://www.digitalocean.com/community/tutorials/how-to-perform-sentiment-analysis-in-python-3-using-the-natural-language-toolkit-nltk
"""

from nltk.stem.wordnet import WordNetLemmatizer
from nltk.corpus import twitter_samples, stopwords
from nltk.tag import pos_tag
from nltk.tokenize import word_tokenize
from nltk import FreqDist, classify, NaiveBayesClassifier

import re, string, random

def remove_noise_revs(review_tokens, stop_words = ()):

    cleaned_tokens = []

    for token, tag in pos_tag(review_tokens):
        token = re.sub('http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+#]|[!*\(\),]|'\
                       '(?:%[0-9a-fA-F][0-9a-fA-F]))+','', token)
        token = re.sub("(@[A-Za-z0-9_]+)","", token)

        if tag.startswith("NN"):
            pos = 'n'
        elif tag.startswith('VB'):
            pos = 'v'
        else:
            pos = 'a'

        lemmatizer = WordNetLemmatizer()
        token = lemmatizer.lemmatize(token, pos)

        if len(token) > 0 and token not in string.punctuation and token.lower() not in stop_words:
            cleaned_tokens.append(token.lower())
    return cleaned_tokens

def get_all_words_revs(cleaned_tokens_lst):
    for tokens in cleaned_tokens_lst:
        for token in tokens:
            yield token

def get_revs_for_model(cleaned_tokens_lst):
    for review_tokens in cleaned_tokens_lst:
        yield dict([token, True] for token in review_tokens)

if __name__ == "__main__":
    positive_review_lst = goodrev_list
    negative_review_lst = badrev_list
    # Tokenizing strings in list of strings  
    positive_review_tokens = [rev.split() for rev in positive_review_lst] 
    negative_review_tokens = [rev.split() for rev in negative_review_lst]
    review_tokens = [rev.split() for rev in positive_review_lst][666]

    stop_words = stopwords.words('english')
    
    positive_cleaned_tokens_lst = []
    negative_cleaned_tokens_lst = []

    for tokens in positive_review_tokens:
        positive_cleaned_tokens_lst.append(remove_noise_revs(tokens, stop_words))

    for tokens in negative_review_tokens:
        negative_cleaned_tokens_lst.append(remove_noise_revs(tokens, stop_words))

    all_pos_words_revs = get_all_words_revs(positive_cleaned_tokens_lst)
    freq_dist_pos_revs = FreqDist(all_pos_words_revs)
    print(freq_dist_pos_revs.most_common(10))
    
    all_neg_words_revs = get_all_words_revs(negative_cleaned_tokens_lst)
    freq_dist_neg_revs = FreqDist(all_neg_words_revs)
    print(freq_dist_neg_revs.most_common(10))
    
    pos_tokens_for_mdl = get_revs_for_model(positive_cleaned_tokens_lst)
    neg_tokens_for_mdl = get_revs_for_model(negative_cleaned_tokens_lst)

    pos_dataset = [(review_dict, "Positive")
                         for review_dict in pos_tokens_for_mdl]

    neg_dataset = [(review_dict, "Negative")
                         for review_dict in neg_tokens_for_mdl]

    dataset_reviews = pos_dataset + neg_dataset

    random.shuffle(dataset_reviews)

    ### CHECK THIS PART #######
    train_data_reviews = dataset_reviews[:2800]
    test_data_reviews = dataset_reviews[2800:]

    classifierNB = NaiveBayesClassifier.train(train_data_reviews)

    print("Review Accuracy is:", classify.accuracy(classifierNB, test_data_reviews))

    print(classifierNB.show_most_informative_features(10))

    custom_review = "This album is the worst of all time. Terrible. Hate it!!"

    custom_reviews = remove_noise_revs(word_tokenize(custom_review))

    print(custom_review, classifierNB.classify(dict([token, True] for token in custom_reviews)))

test_result = []
gold_result = []

for i in range(len(test_data_reviews)):
    test_result.append(classifierNB.classify(test_data_reviews[i][0]))
    gold_result.append(test_data_reviews[i][1])
    
    CM = nltk.ConfusionMatrix(gold_result, test_result)
print(CM)


### Test regexes for example####

select_opeth = content.loc[content['reviewid'] == 16524]
print (select_opeth)

with pd.option_context('display.max_colwidth', 20000):
    print (select_opeth)

BPeg = "Åkerfeldt is as openly enthralled as me with that era of the 1960's into the 1970's when it seemed like all kinds of riffage would rule the earth, often in tandem with whatever reflective, dazed, sorrowful, or just plain confused routes progressive rock was creating. He's a stone-cold fanboy and doesn't hide it at all-- Blackwater Park" 
BP_tokens = BPeg.split()
stop_words = stopwords.words('english')
BP_nonoise = remove_noise_revs(BP_tokens, stop_words) 

st = stop_words = ('english')


#######################################################################################################
#END    ###################################################################################################
########################################################################################################
######################################################################################################
###SCIKIT LEARN BELOW###############################################################################
#######################################################################################################


#pip install wordcloud
#pip install --user spacy

# -*- coding: utf-8 -*-
#Load the libraries
import numpy as np
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt
import nltk
from sklearn.model_selection import train_test_split
from sklearn.feature_extraction.text import CountVectorizer
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.preprocessing import LabelBinarizer
from nltk.corpus import stopwords
from nltk.stem.porter import PorterStemmer
from wordcloud import WordCloud,STOPWORDS
from nltk.stem import WordNetLemmatizer
from nltk.tokenize import word_tokenize,sent_tokenize
from bs4 import BeautifulSoup
#import spacy
import re,string,unicodedata
from nltk.tokenize.toktok import ToktokTokenizer
from nltk.stem import LancasterStemmer,WordNetLemmatizer
from sklearn.linear_model import LogisticRegression,SGDClassifier
from sklearn.naive_bayes import MultinomialNB
from sklearn.svm import SVC
#from textblob import TextBlob
#from textblob import Word
from sklearn.metrics import classification_report,confusion_matrix,accuracy_score
from sklearn.model_selection import train_test_split
import os
print(os.listdir("C:/Users/Joe/Documents/CCSU/Python Class/FInal/"))
import warnings
warnings.filterwarnings('ignore')

import sqlite3, datetime
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns

from scipy.stats import linregress
from scipy.stats import ttest_rel

pd.set_option('precision', 2)
np.set_printoptions(precision=2)

con = sqlite3.connect('C:/Users/Joe/Documents/CCSU/Python Class/FInal/pitchfork-data/database.sqlite')
""" Tables to Query """
artists = pd.read_sql('SELECT * FROM artists', con)
content= pd.read_sql('SELECT * FROM content', con)
genres = pd.read_sql('SELECT * FROM genres', con)
labels = pd.read_sql('SELECT * FROM labels', con)
scores = pd.read_sql('SELECT reviewid, score FROM reviews', con)
years = pd.read_sql('SELECT * FROM years', con)
reviewdetail = pd.read_sql('SELECT * FROM reviews', con)
con.close()


# combine into score-artist mapping
rev_master  = pd.merge(scores, artists, on = 'reviewid')
""" Combine content with review and rating """
rev_master = pd.merge(rev_master,  content, on = 'reviewid')
rev_master = pd.merge(rev_master,  years, on = 'reviewid')
rev_master = pd.merge(rev_master,  genres, on = 'reviewid')

# remove various artists
rev_master  = rev_master[rev_master.artist != 'various artists']

# remove multi-year reviews [re-releases]
year_counts = years.groupby('reviewid').count().reset_index()
keepers = year_counts.loc[year_counts.year == 1, 'reviewid']
rev_master = rev_master.loc[rev_master.reviewid.isin(keepers)]
rev_master =  rev_master[rev_master['year'] > 1985]
rev_master =  rev_master[rev_master['genre'] =='rock']

descsumm = rev_master.groupby('year')["score"].mean()
descsumm2 = rev_master.groupby('genre')["score"].mean()

"""### Make a dataframe of BAD reviews"""
""" ROCK and 5.6, 7.5 has 80% """
"""  Was 8.1  """
""" lets try 7.5 """
""" I like 8 """
# add "FLAG" column: 1 represents having affairs, 0 represents not
rev_master['bad_score'] = (rev_master.score <= 5.6).astype(int)
rev_master['good_score'] = (rev_master.score >= 8.3).astype(int)

badreviews =  rev_master[rev_master['score'] <= 5.6]
badreviewcontent = badreviews['content']
""" Convert to a list """
badrev_list = badreviewcontent.tolist()

"""### Make a dataframe of positive reviews"""
goodreviews =  rev_master[rev_master['score'] >= 8.3]
goodreviewcontent = goodreviews['content']
""" Convert to a list """
goodrev_list = goodreviewcontent.tolist()


reviews_tr,reviews_te=train_test_split(rev_master,test_size=0.4)

### Sort to set the target - this dataet only, Pos then Neg 
#
#reviews_train = []
#for line in open('C:/Users/Joe/Documents/CCSU/Python Class/FInal/full_train.txt', 'r'):
#    reviews_train.append(line.strip())
#    
#reviews_test = []
#for line in open('C:/Users/Joe/Documents/CCSU/Python Class/FInal/full_test.txt', 'r'):
#    reviews_test.append(line.strip())

reviews_tr2 = reviews_tr['content']
""" Convert to a list """
reviews_tr2 = reviews_tr2.tolist()

reviews_te2 = reviews_te['content']
""" Convert to a list """
reviews_te2 = reviews_te2.tolist()

x=reviews_te.groupby('score').count()
###

### PREPROCESS TO HERE 3/16/20
import re

REPLACE_NO_SPACE = re.compile("[.;:!\'?,\"()\[\]]")
REPLACE_WITH_SPACE = re.compile("(<br\s*/><br\s*/>)|(\-)|(\/)")

def preprocess_reviews(reviews):
    reviews = [REPLACE_NO_SPACE.sub("", line.lower()) for line in reviews]
    reviews = [REPLACE_WITH_SPACE.sub(" ", line) for line in reviews]
    
    return reviews

reviews_train_clean = preprocess_reviews(reviews_tr2)
reviews_test_clean = preprocess_reviews(reviews_te2)

#### VECTORIZATION

from sklearn.feature_extraction.text import CountVectorizer

cv = CountVectorizer(binary=True)
cv.fit(reviews_train_clean)
X = cv.transform(reviews_train_clean)
X_test = cv.transform(reviews_test_clean)

##### BUILD CLASSIFIER
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import accuracy_score
from sklearn.model_selection import train_test_split

#### TO HERE on 3/17/20
#target = [1 if i < 12500 else 0 for i in range(25000)]
### TARGET BAD OR GOOD SCORES
##############################
target = reviews_tr['good_score'].tolist()
target_test = reviews_te['good_score'].tolist()
##############################################
##############################################
X_train, X_val, y_train, y_val = train_test_split(
    X, target, train_size = 0.75)

for c in [0.01, 0.025, 0.05, 0.25, 0.5, 1]:
    
    lr = LogisticRegression(C=c)
    lr.fit(X_train, y_train)
    print ("Accuracy for C=%s: %s" 
           % (c, accuracy_score(y_val, lr.predict(X_val))))
    
#Accuracy for C=0.01: 0.8612153038259565
#Accuracy for C=0.025: 0.8604651162790697
#Accuracy for C=0.05: 0.8604651162790697
#Accuracy for C=0.25: 0.8567141785446362
#Accuracy for C=0.5: 0.8559639909977494
#Accuracy for C=1: 0.8559639909977494
    
### TRAIN MODEL
final_model = LogisticRegression(C=0.01)
final_model.fit(X, target)
print ("Final Accuracy MODEL 1: %s" 
       % accuracy_score(target_test, final_model.predict(X_test)))

#Final Accuracy: 0.8706411698537683

feature_to_coef = {
    word: coef for word, coef in zip(
        cv.get_feature_names(), final_model.coef_[0]
    )
}
for best_positive in sorted(
    feature_to_coef.items(), 
    key=lambda x: x[1], 
    reverse=True)[:10]:
    print (best_positive)
        
for best_negative in sorted(
    feature_to_coef.items(), 
    key=lambda x: x[1])[:10]:
    print (best_negative)
            
### Add to Part 2 Enhance = Remove Stopwords
from nltk.corpus import stopwords

english_stop_words = stopwords.words('english')
def remove_stop_words(corpus):
    removed_stop_words = []
    for review in corpus:
        removed_stop_words.append(
            ' '.join([word for word in review.split() 
                      if word not in english_stop_words])
        )
    return removed_stop_words

no_stop_words_train = remove_stop_words(reviews_train_clean)    
no_stop_words_test = remove_stop_words(reviews_test_clean)

def get_stemmed_text(corpus):
    from nltk.stem.porter import PorterStemmer
    stemmer = PorterStemmer()
    return [' '.join([stemmer.stem(word) for word in review.split()]) for review in corpus]

stemmed_reviews_train = get_stemmed_text(no_stop_words_train)
stemmed_reviews_test = get_stemmed_text(no_stop_words_test)


def get_lemmatized_text(corpus):
    from nltk.stem import WordNetLemmatizer
    lemmatizer = WordNetLemmatizer()
    return [' '.join([lemmatizer.lemmatize(word) for word in review.split()]) for review in corpus]

lemmatized_reviews_train = get_lemmatized_text(stemmed_reviews_train)
lemmatized_reviews_test = get_lemmatized_text(stemmed_reviews_test)

### Rename training reveiws after clean up
reviews_train_clean = lemmatized_reviews_train
reviews_test_clean = lemmatized_reviews_test
   
    #### PART 2
    
from sklearn.feature_extraction.text import CountVectorizer
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import accuracy_score
from sklearn.model_selection import train_test_split

ngram_vectorizer = CountVectorizer(binary=True, ngram_range=(1, 2))
ngram_vectorizer.fit(reviews_train_clean)
X = ngram_vectorizer.transform(reviews_train_clean)
X_test = ngram_vectorizer.transform(reviews_test_clean)

X_train, X_val, y_train, y_val = train_test_split(
    X, target, train_size = 0.75
)

for c in [0.01, 0.025, 0.05, 0.25, 0.5, 1]:
    
    lr = LogisticRegression(C=c)
    lr.fit(X_train, y_train)
    print ("Accuracy for C=%s: %s" 
           % (c, accuracy_score(y_val, lr.predict(X_val))))
    
#Accuracy for C=0.01: 0.8537134283570893
#Accuracy for C=0.025: 0.8559639909977494
#Accuracy for C=0.05: 0.8567141785446362
#Accuracy for C=0.25: 0.8567141785446362
#Accuracy for C=0.5: 0.8567141785446362
#Accuracy for C=1: 0.8567141785446362
    
final_ngram = LogisticRegression(C=0.5)
final_ngram.fit(X, target)
print ("Final Accuracy MODEL 2: %s" 
       % accuracy_score(target_test, final_ngram.predict(X_test)))

#Final Accuracy: 0.8675478065241845
""" Word Counts

Instead of simply noting whether a word appears in the review or not, we can 
include the number of times a given word appears. This can give our sentiment
 classifier a lot more predictive power. For example, if a movie reviewer says
 ‘amazing’ or ‘terrible’ multiple times in a review it is considerably more 
 probable that the review is positive or negative, respectively.   
 """
 
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import accuracy_score
from sklearn.model_selection import train_test_split

wc_vectorizer = CountVectorizer(binary=False)
wc_vectorizer.fit(reviews_train_clean)
X = wc_vectorizer.transform(reviews_train_clean)
X_test = wc_vectorizer.transform(reviews_test_clean)

X_train, X_val, y_train, y_val = train_test_split(
    X, target, train_size = 0.75, 
)

for c in [0.01, 0.025, 0.05, 0.25, 0.5, 1]:
    
    lr = LogisticRegression(C=c)
    lr.fit(X_train, y_train)
    print ("Accuracy for C=%s: %s" 
           % (c, accuracy_score(y_val, lr.predict(X_val))))
        
final_wc = LogisticRegression(C=0.025)
final_wc.fit(X, target)
print ("Final Accuracy MODEL3 WC: %s" 
       % accuracy_score(target_test, final_wc.predict(X_test)))

## Accuracy for C=0.01: 0.8574643660915229
## Accuracy for C=0.025: 0.8604651162790697
## Accuracy for C=0.05: 0.8582145536384096
## Accuracy for C=0.25: 0.8582145536384096
## Accuracy for C=0.5: 0.8582145536384096
## Accuracy for C=1: 0.8574643660915229
## Final Accuracy: 0.8661417322834646 

from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import accuracy_score
from sklearn.model_selection import train_test_split

tfidf_vectorizer = TfidfVectorizer()
tfidf_vectorizer.fit(reviews_train_clean)
X = tfidf_vectorizer.transform(reviews_train_clean)
X_test = tfidf_vectorizer.transform(reviews_test_clean)

X_train, X_val, y_train, y_val = train_test_split(
    X, target, train_size = 0.75
)

for c in [0.01, 0.025, 0.05, 0.25, 0.5, 1]:
    
    lr = LogisticRegression(C=c)
    lr.fit(X_train, y_train)
    print ("Accuracy for C=%s: %s" 
           % (c, accuracy_score(y_val, lr.predict(X_val))))

    
final_tfidf = LogisticRegression(C=1)
final_tfidf.fit(X, target)
print ("Final Accuracy TFIDF: %s" 
       % accuracy_score(target_test, final_tfidf.predict(X_test)))

#Accuracy for C=0.01: 0.8559639909977494
#Accuracy for C=0.025: 0.8559639909977494
#Accuracy for C=0.05: 0.8559639909977494
#Accuracy for C=0.25: 0.8559639909977494
#Accuracy for C=0.5: 0.8559639909977494
#Accuracy for C=1: 0.8574643660915229
#Final Accuracy: 0.8692350956130483


from sklearn.feature_extraction.text import CountVectorizer
from sklearn.svm import LinearSVC
from sklearn.metrics import accuracy_score
from sklearn.model_selection import train_test_split

ngram_vectorizer = CountVectorizer(binary=True, ngram_range=(1, 2))
ngram_vectorizer.fit(reviews_train_clean)
X = ngram_vectorizer.transform(reviews_train_clean)
X_test = ngram_vectorizer.transform(reviews_test_clean)

X_train, X_val, y_train, y_val = train_test_split(
    X, target, train_size = 0.75
)

for c in [0.01,0.025, 0.05, 0.25, 0.5, 1]:
    
    svm = LinearSVC(C=c)
    svm.fit(X_train, y_train)
    print ("Accuracy for C=%s: %s" 
           % (c, accuracy_score(y_val, svm.predict(X_val))))
    
#Accuracy for C=0.01: 0.8747186796699175
#Accuracy for C=0.025: 0.8754688672168042
#Accuracy for C=0.05: 0.8762190547636909
#Accuracy for C=0.25: 0.8762190547636909
#Accuracy for C=0.5: 0.8769692423105776
#Accuracy for C=1: 0.8762190547636909
    
final_svm_ngram = LinearSVC(C=0.5)
final_svm_ngram.fit(X, target)
print ("Final Accuracy SVM: %s" 
       % accuracy_score(target_test, final_svm_ngram.predict(X_test)))

# Final Accuracy: 0.8689538807649044


#############################################################################
#############################################################################

