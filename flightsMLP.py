import tensorflow as tf 
import numpy as np 
import pandas as pd 
import math

Flights_DataSet = pd.read_csv("./Processed_Flights_DataSet.csv")
Flights_Target = pd.read_csv("./Processed_Flights_Target.csv")

#flights = np.array(Flights_DataSet.iloc[:,0:4])
flights = np.array(Flights_DataSet)
delayed = np.array(Flights_Target)

sample_size = 80000

train_sample = flights[:sample_size]
train_target = delayed[:sample_size]

test_sample = flights[sample_size:]
test_target = delayed[sample_size:]

input_units = 43
hidden1_units = 6
output_units = 2

W1 = tf.Variable(tf.truncated_normal([input_units, hidden1_units]))
b1 = tf.Variable(tf.zeros([hidden1_units]))
W2 = tf.Variable(tf.truncated_normal([hidden1_units, output_units]))
b2 = tf.Variable(tf.zeros([output_units]))

x = tf.placeholder(tf.float32, shape = [None, input_units])
y = tf.placeholder(tf.float32, shape = [None, output_units])

hidden1 = tf.sigmoid(tf.matmul(x, W1) + b1)
y_ = tf.sigmoid(tf.matmul(hidden1, W2) + b2)

learning_rate = 0.1

loss = tf.reduce_mean(tf.reduce_sum(tf.square(y_ - y)))
optimizer = tf.train.GradientDescentOptimizer(learning_rate).minimize(loss)

init = tf.global_variables_initializer()
sess = tf.Session()
sess.run(init)

Iterations = 25000

batch_size = 100
start = 0
end = batch_size

for i in range(Iterations + 1):
    sess.run(optimizer, feed_dict={x: train_sample[start:end], y: train_target[start:end]})
    if i % 100 == 0:
        correct_prediction = tf.equal(tf.argmax(y_, 1), tf.argmax(y, 1))
        accuracy = tf.reduce_mean(tf.cast(correct_prediction, tf.float32))
        test_accuracy = sess.run(accuracy, feed_dict={x: test_sample, y: test_target})
        print("EPOCH: {0}  Test Accuracy: {1}".format(i, test_accuracy))
    start = end if end < sample_size else 0
    end = start + batch_size

sess.close()