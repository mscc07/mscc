#!/usr/bin/python

from numpy import vstack, loadtxt, argsort
import math
from pandas import read_csv
#from sklearn.preprocessing import LabelEncoder
from sklearn.metrics import accuracy_score,mean_squared_error
from torch.utils.data import Dataset
from torch.utils.data import DataLoader
from torch.utils.data import random_split
from torch import Tensor
import torch
from torch.nn import Sigmoid,Softmax,ReLU,Linear,Tanh
from torch.nn import Module
from torch.optim import SGD,Adam
from torch.nn import BCELoss,NLLLoss,CrossEntropyLoss,MSELoss
from torch.nn.init import kaiming_uniform_,xavier_uniform_
from sklearn.model_selection import train_test_split

from bitstring import Bits, BitStream, BitArray, ConstBitStream

from setup import readInput

model, nSite, subSpace, nStates, s2Target, maxItr, startSpinTargetItr, energyTola, spinTola, beta, jVal, det, Ms,  posibleDet, bondOrder, outputfile, restart, saveBasis = readInput()

'''
nSite = 18
ciCutoff = 0.001
subSpace = 500
s2Target = 0
outputfile = "s0-6666-sub2000.in.out"
'''



H = nSite
nCycle = 500


testSize = 0.10
fTrain = outputfile + ".accVsPreTrain.dat"
fTest = outputfile + ".accVsPreTest.dat"
errorFile = outputfile +".error.dat" 


################ For Error Calculations   ###########
def error(data):

    actual = []
    prediction = []
    for i in range(len(data)):
        actual.append(data[i][0])
        prediction.append(data[i][1])


    acc = mean_squared_error(prediction, actual)

    return acc




##############################################################
class CSVDataset(Dataset):
    def __init__(self, path):
        df  = read_csv(path, header=None)
        self.X = df.values[:, :-1]
        self.y = df.values[:, -1]

        self.X = self.X.astype('float32')
        self.y = self.y.astype('float32')

        self.y = self.y.reshape((len(self.y), 1))
    
    def __len__(self):
        return len(self.X)

    def __getitem__(self, idx):
        return [self.X[idx], self.y[idx]]




    def get_splits(self, n_test = testSize):                              # spliting of dataset 
        test_size = int(round(n_test * len(self.X)))
        train_size = int(len(self.X) - test_size)
        return random_split(self, [train_size, test_size])


class CSVDatasetPredict(Dataset):
    def __init__(self, path):
        df  = read_csv(path, header=None)
        self.X = df.values

        self.X = self.X.astype('float32')

    def __len__(self):
        return len(self.X)

    def __getitem__(self, idx):
        return self.X[idx]

#*********************************************************************#


#################################################
class Network(Module):                                      
    def __init__(self,nSite):
        super(Network,self).__init__()

        #input descriptor
        
        self.hidden1 = Linear(nSite, H)
        kaiming_uniform_(self.hidden1.weight, nonlinearity='relu')
        self.relu = ReLU()
        
        self.output = Linear(H, 1)                                  # 2nd hidden layer to output
        xavier_uniform_(self.output.weight)
        
        # Define sigmoid activation and softmax output 

        self.relu = ReLU()

    def forward(self, X):
        # Pass the input tensor through each of our operations
        X = self.hidden1(X)
        X = self.relu(X)
        X = self.output(X)
        X = self.relu(X)
        return X

#*********************************************************************#

######################################### validation ############################
def validation(test_dl, model):                                          #send the test dataset through the network
    predictions, actuals = list(), list()
    for i, (inputs, targets) in enumerate(test_dl):
        yhat = model(inputs)
        yhat = yhat.detach().numpy()
        actual = targets.numpy()
        actual = actual.reshape((len(actual), 1))
        predictions.append(yhat)
        actuals.append(actual)
    predictions, actuals = vstack(predictions), vstack(actuals)
    # calculate accuracy
    acc = mean_squared_error(actuals, predictions)
    return acc

#######################TRAINING ################################
def train_model(train_dl, test_dl, model):
    criterion = MSELoss()
    optimizer = Adam(model.parameters(), lr=0.001, betas=(0.9, 0.999))

    for epoch in range(nCycle):
        running_loss = 0.0
        for i, (inputs, targets) in enumerate(train_dl):
            optimizer.zero_grad()
            yhat = model(inputs)
            loss = criterion(yhat, targets)
            loss.backward()
            optimizer.step()
            running_loss +=loss.item()

        epoch_loss = running_loss/len(train_dl)
        validation_error = validation(test_dl, model)
        #print("epoch", epoch, "epoch_loss", epoch_loss, "validation_error", validation_error)
        #print(epoch, epoch_loss)#, validation_error)

###**************************************************###############
#################################################
def evaluate_model(test_dl, model, fl):
    predictions, actuals = list(), list()
    
    f = open(fl,"w")

    for i, (inputs, targets) in enumerate(test_dl):
        
        yhat = model(inputs)
        yhat = yhat.detach().numpy()
        
        actual = targets.numpy()
        actual = actual.reshape((len(actual), 1))
        for j in range(len(actual)):
            newline = ("%f %f\n")% (10.0 ** (actual[j][0] *- 1), 10.0 ** (yhat[j][0] *- 1))
            f.write(newline)
        
        predictions.append(yhat)
        actuals.append(actual)
    predictions, actuals = vstack(predictions), vstack(actuals)

    # calculate accuracy
    acc = mean_squared_error(actuals, predictions)
    return acc

def predict_model(test_dl, model):
    detList = []
    predictValue = []
    for inputs in test_dl:
        yhat = model(inputs)
        yhat = yhat.detach().numpy()
        
        for j in range(len(yhat)):
            predictValue.append(yhat[j][0])
        sort_index = argsort(predictValue)
        for j in range (len(sort_index)):
            inputList = inputs[sort_index[j]].tolist()
                
            inputStr = BitArray('0b'+''.join([str(int((elem + 1)/2)) for elem in inputList]))

            if inputStr not in detList:
                detList.append(inputStr)
                if (Ms[0] == 0):
                    detList.append(~inputStr)

    return detList



def enrich_model(test_dl, model):
    allDet = []
    ci = []
    for inputs in test_dl:
        yhat = model(inputs)
        yhat = yhat.detach().numpy()

        for j in range(len(yhat)):
            
            inputList = inputs[j].tolist()
            inputStr = BitArray('0b'+''.join([str(int((elem + 1)/2)) for elem in inputList]))

            if inputStr not in allDet:
                allDet.append(inputStr)
                ci.append(10.0 ** (yhat[j][0] *- 1))
    
    return allDet, ci


#####################################
# prepare the dataset

def prepare_data(path):
    dataset = CSVDataset(path)
    train, test = dataset.get_splits()
    train_dl = DataLoader(train, batch_size=500, shuffle=True)
    test_dl = DataLoader(test, batch_size=500, shuffle=True)
    return train_dl, test_dl


def prepare_predict_data(path, size_x):
    dataset = CSVDatasetPredict(path)
    train_dl = DataLoader(dataset,  batch_size = size_x, shuffle = False)
    return train_dl

#**************************************#

##########################################
def ann_train(dataFile, predictDataFile):
    # prepare the data
    path = dataFile
    train_dl, test_dl = prepare_data(path)
    # define the network

    model = Network(nSite)

    # train the models

    train_model (train_dl, test_dl, model)
    torch.save(model.state_dict(), outputfile+".model.pth")


    acc1 = evaluate_model(train_dl, model, fTrain)
    acc2 = evaluate_model(test_dl, model, fTest)
    
    data1 =  loadtxt(fTrain, usecols = [0, 1], dtype = float)
    data2 =  loadtxt(fTest, usecols = [0, 1], dtype = float)
    
    with open (errorFile, "a") as fout:
        newline = ("Train Error- %lf\t Test Error - %lf\n")%(error(data1), error(data2))
        fout.write(newline)
    #print("Train Error", error(data1))
    #print("Test Error", error(data2))
    
    path = predictDataFile
    with open(predictDataFile, 'r') as fp:   # to get line no of predictDataFile
        size_x = len(fp.readlines())

    predict_dl = prepare_predict_data(path, size_x)
    detList = predict_model(predict_dl, model)

    #print (impDet)
    return detList


def ann_enrich(enrichDataFile):
    path = enrichDataFile
    with open(path, 'r') as fp:   # to get line no of enrichDataFile
        size_x = len(fp.readlines())
    
    
    enrich_dl = prepare_predict_data(path, size_x)
    model = Network(nSite)
    model.load_state_dict(torch.load(outputfile+".model.pth"))
    
    allDet, ci = enrich_model(enrich_dl, model)

    return allDet, ci


#ann_train("TrainData_subSpace500_spin0.0.csv", "PredictData_subSpace500_spin0.0.csv")
#ann_enrich("PredictData_subSpace500_spin0.0.csv")
