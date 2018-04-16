import pandas as pd
import tensorflow as tf
import tensorflow_hub as hub
import math

djson = pd.read_json('allcards.json').transpose()
dbest = pd.read_csv(
            'best-cards-rix-standard.tsv',
            delimiter='\t',
            index_col='card'
        )
del dbest['count_avg']

# TODO Then remove unsed columns: count_avg, manaCost(? might be less confusing
# fo the model to just use cmc
# card_columns = ['power', 'toughness', 'cmc', 'manaCost', 'type', 'text']
card_columns = ['power', 'toughness', 'cmc', 'type', 'text']

djoin = pd.merge(dbest, djson[card_columns], left_on='card', right_index=True)

embedded_text_feature_column = hub.text_embedding_column(
    key="sentence",
    module_spec="https://tfhub.dev/google/nnlm-en-dim128/1")

# check the join
print(dbest.shape)
print(djoin.shape)
for i in dbest.card:
    if i not in set(djoin.card):
        print(i)

# TODO handle aftermath cards:
# Eg. Commit / Memory in dbest is called Commit in djson

# free up some memory before moving on
del djson
del dbest

# create train and test dataframes from the joined dataframe
# shuffle the data frame
djoin = djoin.sample(frac=1).reset_index(drop=True)


# TODO move name to the index of dbest
# (which becomes the base dataframe for the
# training and test frames


num_training_rows = int(math.ceil(djoin.shape[0]*.8))
num_test_rows = int(math.floor(djoin.shape[0]*.2))

dtrain = djoin.iloc[0:num_training_rows]
dtest = djoin.iloc[num_training_rows: num_training_rows + num_test_rows]

# Training input on the whole training set with no limit on training epochs.
train_input_fn = tf.estimator.inputs.pandas_input_fn(
    dtrain, dtrain["deck_pct"], num_epochs=None, shuffle=True)

# Prediction on the whole training set.
predict_train_input_fn = tf.estimator.inputs.pandas_input_fn(
    dtrain, dtrain["deck_pct"], shuffle=False)
# Prediction on the test set.
predict_test_input_fn = tf.estimator.inputs.pandas_input_fn(
    dtest, dtest["deck_pct"], shuffle=False)


# use regressor! add dropout to ada optimizer
