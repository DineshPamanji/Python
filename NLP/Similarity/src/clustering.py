####################################################################################################################
####################################################
# Clustering
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.cluster import KMeans
from sklearn.metrics import silhouette_score
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

# Import helpers
from src.helpers import *
from src.DataPreparation import *
# import stopwords

# Load config
config = open("src/Config.txt")
exec(config.read(), globals())

# Create df
df = create_df(path, file, _JOB_TITLE_COL, _DIV_COL, top10)

# Clean the text
df[_JOB_TITLE_COL + 'str'] = df[_JOB_TITLE_COL].astype(str)
df = clean_column(df, _JOB_TITLE_COL + 'str')


# import nltk

# df['nouns'] = pd.Series()
# nouns_all = []
# for i in df[_JOB_TITLE_COL + 'str'].str.split():
#     nouns = set()
#     for word, pos in nltk.pos_tag(i):  # remove the call to nltk.pos_tag if `sentence` is a list of tuples as described above
#         if pos in ['NN']:  # feel free to add any other noun tags
#             nouns.add(word)
#     nouns_all.append(list(nouns))
# df['nouns'] = nouns_all


# Create vectorizer
vectorizer = TfidfVectorizer(stop_words='english')

# X = vectorizer.fit_transform(df_new[_JOB_TITLE_COL])
X = vectorizer.fit_transform(df[_JOB_TITLE_COL])

# Plot elbow curve
sse = {}
for k in range(15000, 21000, 2000):
    print(k)
    model = KMeans(n_clusters=k, init='k-means++', max_iter=100, n_init=1)
    model.fit(X)
    sse[k] = model.inertia_  # Inertia: Sum of distances of samples to their closest cluster center
plt.figure()
plt.plot(list(sse.keys()), list(sse.values()))
plt.xlabel("Number of cluster")
plt.ylabel("SSE")
plt.show()


true_k = 10000
model = KMeans(n_clusters=true_k, init='k-means++', max_iter=100, n_init=1)
model.fit(X)

df['Cluster'] = model.predict(X)

print(model.inertia_)
order_centroids = model.cluster_centers_.argsort()[:, ::-1]
terms = df[_JOB_TITLE_COL]  #vectorizer.get_feature_names()

for i in range(true_k):
    print("Cluster %d:" % i),
    for ind in order_centroids[i, :10]:
        print(" %s" % terms[ind])

# Save output
df.to_excel(path+'/Output/EMSI Cluster 0820.xlsx', index=False)

# df_new = pd.read_excel('C:/Projects/OrgBuilder/Codes/RNN_Test\Output\TD Bank Predicted Cluster .xlsx')
# df_new_13 = df_new[df_new['Cluster']==13]
# df_new_12 = df_new[df_new['Cluster']==12]


## DBSCAN

from sklearn.cluster import DBSCAN

# cluster the data into five clusters
dbscan = DBSCAN(eps=0.55, min_samples = 2)
clusters = dbscan.fit_predict(X)
df['Cluster'] = clusters

# Save output
df.to_excel(path+'/Output/EMSI Cluster DBSCAN 0830.xlsx', index=False)


from sklearn.cluster import OPTICS, cluster_optics_dbscan
clust = OPTICS(min_samples=50, xi=.05, min_cluster_size=.05)

# Run the fit
clust.fit(X.toarray())

df['Cluster'] = clust.labels_

