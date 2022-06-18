## Nonlinear Autoregressive with Exogenous Input (NARX) Neural Network

## About The Data
The data used in this study are historical data of precipitation and Niño 3.4 data in Jakarta, the capital city of Indonesia. These time-series data are obtained from the APCC website (www.apcc21.org accessed on 28 March 2022) and the UCSB CHRIPS website (www.iridl.ldeo.columbia.edu accessed  on  28  March  2022). Specifically, the precipitation data are the main variables, and the Niño 3.4 data are the exogenous variables. The time range for the rainfall data and Nino 3.4 starts from January 2001 to December 2021. The nonlinear autoregressive exogenous neural network (NARX NN) analysis was carried out using R software to forecast the precipitation data.

## Definition of NARX NN Method
The Nonlinear Autoregressive with Exogenous Input (NARX) Neural Network, a subtype or improvement of the Recurrent Neural Network (RNN), is specifically designed to predict time series. Because it uses the additional information included in the series of interest that has already been output before period t and both the primary and unrolled structures of a generic NARX NN, the model generally outperforms the general input-output RNN model.

## Analysis Stage
1. Splitting the data according to the Rolling Origin Cross-Validation (ROCV) series to form a network architecture by dividing the data into two parts, namely data training and data testing.
2. Creating a network architecture include determining the number of input units, hidden layers (by trial and error method), units, and output units used in the network.
3. Select the activation function to convert the input signal of each unit in the hidden and output layers into an output signal without any specific rules for selecting the activation function.
4. Normalize the data using the min-max scaling technique because input data must be in the interval of the used activation function before performing the training process, particularly in the run-up to any data normalization.
5. Training and testing process implementation to find the best specification model with the fewest errors.
6. Training and testing process is repeated for different network architectures with various hidden units.
7. Predicting the exogenous variables using a forecasting method appropriate for the variables' characteristics, with the forecasting method that was suitable in this study being FFNN.
8. Predicting the main variables using The NARX NN.

