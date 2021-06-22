#!/usr/bin/env python
# coding: utf-8



import sys
import numpy as np
import pandas as pd
import tensorflow as tf
import scipy
from IPython.display import clear_output
from tensorflow.keras import activations, backend
get_ipython().run_line_magic('matplotlib', 'inline')
import matplotlib.pyplot as plt
import os.path
import tensorflow.keras as keras
from tensorflow.keras import backend as K
from tensorflow.keras import Model, Input
from tensorflow.keras.layers import Dense, Conv2D, MaxPooling2D, Dropout, Flatten,LSTM, TimeDistributed, Masking, Reshape, Lambda, RepeatVector, Permute, multiply
from tensorflow.keras.models import Sequential
from tensorflow.keras.callbacks import EarlyStopping
from sklearn.model_selection import StratifiedKFold, GridSearchCV, RepeatedKFold
from sklearn.utils import resample
from sklearn.metrics import roc_curve,roc_auc_score, confusion_matrix
from tensorflow.keras.wrappers.scikit_learn import KerasClassifier
from sklearn.model_selection import KFold
import shap as sh
from keras import backend as K

tf.compat.v1.disable_eager_execution()

# should be 2.1.0
tf.__version__



# function for attention layer

def attention(inputs, SHAPE):
    
    n_steps = int(inputs.shape[1]) 
    a = Permute((1, 2))(inputs) 
    a = Reshape((n_steps, SHAPE))(a) 
    a = Dense(SHAPE, activation='softmax', name='attention_vec')(a)
    output_attention_mul = multiply([inputs, a])
    return output_attention_mul



# function to extract activation weights

def get_activations(model, inputs, print_shape_only=False, layer_name=None, verbose=False):

    activations = []
    inp = model.input
    if layer_name is None:
        outputs = [layer.output for layer in model.layers]
    else:
        outputs = [layer.output for layer in model.layers if layer.name == layer_name]  
    funcs = [K.function([inp] + [K.learning_phase()], [out]) for out in outputs]  
    layer_outputs = [func([inputs, 1.])[0] for func in funcs]
    for layer_activations in layer_outputs:
        activations.append(layer_activations)
        if verbose:
            if print_shape_only:
                print(layer_activations.shape)
            else:
                print(layer_activations)
    return activations



# Function that creates the model based on parameters

def create_model(optimizer="adam", dropout=0.2, init='uniform', dense_nparams1=128, lr=0.001, n_wind=10):
    
    input_layer = Input(shape=(n_wind, n_features)) 
    x = attention(input_layer, n_features)
    x = LSTM(dense_nparams1, activation='tanh', return_sequences=False, recurrent_dropout = dropout)(x)
    preds = Dense(1, activation="sigmoid")(x)
    model = Model(inputs=input_layer, outputs=preds)

    RMS = keras.optimizers.RMSprop(lr=lr, rho=0.9, epsilon=1e-08)
    model.compile(optimizer=RMS, loss='binary_crossentropy', metrics=['acc'])
    return model



# Read training and test sets

train_x_path = "CardioICURisk/output/o4.x_train.csv"
test_x_path = "CardioICURisk/output/o4.x_test.csv"
train_y_path = "CardioICURisk/output/o4.y_train.csv"
test_y_path = "CardioICURisk/output/o4.y_test.csv"

x_train=np.loadtxt(open(train_x_path, 'rt'), delimiter=",", skiprows=1)
y_train=np.loadtxt(open(train_y_path, 'rt'), delimiter=",", skiprows=1, usecols = 1)
x_test=np.loadtxt(open(test_x_path, 'rt'), delimiter=",", skiprows=1)
y_test=np.loadtxt(open(test_y_path, 'rt'), delimiter=",", skiprows=1, usecols = 1)



# reshape from [samples, timesteps] into [samples, timesteps, features]

n_features = x_train.shape[1]
n_wind = 10
n_ind_train = int(x_train.shape[0]/n_wind)
n_ind_test = int(x_test.shape[0]/n_wind)
x_train = x_train.reshape((n_ind_train, 10, n_features))
x_test = x_test.reshape((n_ind_test, 10, n_features))
x_train.shape, y_train.shape, x_test.shape, y_train.shape



# select model's parameters based on best performance of 10-fold cross-validation

cv_res = pd.read_csv("CardioICURisk/output/o5.models_params.csv")
cv_res=cv_res.sort_values(by=['auc'], ascending=False)

dropout1= cv_res['dropout'].iloc[0]
unit_n1 = cv_res['unit_n'].iloc[0]
epoch_n1 = cv_res['epoch_n'].iloc[0]
lr1 = cv_res['lr'].iloc[0]
batch_n1 = cv_res['batch_n'].iloc[0]


# Create and train the model

K.clear_session()
model=create_model(optimizer="adam", dropout=dropout1, init='uniform', dense_nparams1=unit_n1,  lr=lr1, n_wind=10)
model.fit(x_train, y_train, batch_size=batch_n1, epochs=epoch_n1,
                                      validation_split=0.2, verbose=0)



# save output files

model.save('CardioICURisk/output/o5.fin_model.h5')

y_test_prob=model.predict(x_test)
np.savetxt("CardioICURisk/output/o5.fin_model_pred.csv", y_test_prob, delimiter=',')
    
activations = get_activations(model, x_test, print_shape_only=True, layer_name='attention_vec', verbose=True)[0]
act_2d=activations.transpose(0,2,1).reshape(x_test.shape[0], x_test.shape[2]*10)
np.savetxt("CardioICURisk/output/o5.fin_model_act.csv", act_2d, delimiter=',')



