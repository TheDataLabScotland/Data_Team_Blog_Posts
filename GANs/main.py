from io import BytesIO
from PIL import Image as PIL_Image
import numpy as np
from IPython.display import display, Image
import myTools
import matplotlib.pyplot as plt
import numpy as np
import theano
import theano.tensor as T
import lasagne
from lasagne.layers import DenseLayer, DropoutLayer, ReshapeLayer, InputLayer, FlattenLayer, Upscale2DLayer, LocalResponseNormalization2DLayer
floatX = theano.config.floatX
from lasagne.layers.dnn import MaxPool3DDNNLayer, Conv3DDNNLayer, MaxPool2DDNNLayer, Conv2DDNNLayer
from lasagne.layers import Deconv2DLayer
from lasagne.layers import batch_norm
from lasagne.objectives import categorical_crossentropy, binary_crossentropy, categorical_accuracy, binary_accuracy
import sys
import time
import random
from random import randint

#number of epochs defined by the user
argEpochs=int(sys.argv[3])

random.seed(123)

train_set=myTools.loadImages('images/all64x64', 64, 64, 3).reshape(-1, 3, 64, 64)

batch_size=128
NU=64
input_var = T.tensor4()
noise_var = T.matrix()
# Create neural network model
print('--------------------')
print("Building model and compiling Theano functions...")
generator = myTools.createGenerator2(noise_var)
discriminator = myTools.createDiscriminator2(input_var)

# avoiding errors when computing logs in the costs functions
TINY=1e-8
def clip(x):
    return T.clip(x, 1e-8, 1-1e-8)

# Create expressions for getting the network outputs
output_discriminator = lasagne.layers.get_output(discriminator)
output_generator_deterministic = lasagne.layers.get_output(generator, deterministic=True)
output_generator = lasagne.layers.get_output(generator)
# Expression to get discriminator output, given generator output
output_discriminator_fake = lasagne.layers.get_output(discriminator, inputs=output_generator)

# Discriminator total loss (from original and fake data)
loss_discriminator = -T.log(output_discriminator + TINY).mean() -  T.log(1. - output_discriminator_fake + TINY).mean()
# Discriminator loss on fake data only
loss_discriminator_fake = -T.log(output_discriminator_fake + TINY).mean()

# Discriminator accuracy on fake data
accuracy_discriminator_fake = binary_accuracy(output_discriminator_fake, T.zeros_like(output_discriminator_fake)).mean()
# Discriminator accuracy on real data
accuracy_discriminator = binary_accuracy(output_discriminator, T.ones_like(output_discriminator)).mean()
# Overall discriminator accuracy
accuracy_discriminator = (accuracy_discriminator + accuracy_discriminator_fake)/2

# Get discriminator's parameters
params_discriminator = lasagne.layers.get_all_params(discriminator, trainable=True)
# Get generator's parameters
params_generator = lasagne.layers.get_all_params(generator, trainable=True)

# Discriminator and generator optimisers
updates_generator = lasagne.updates.adam(loss_discriminator_fake, params_generator, learning_rate=1e-3,  beta1=0.5)
updates_discriminator = lasagne.updates.adam(loss_discriminator, params_discriminator, learning_rate=1e-4,  beta1=0.5)

# Compile theano functions to train the two networks
train_generator_fn = theano.function([noise_var], (loss_discriminator_fake, accuracy_discriminator_fake), updates=updates_generator)
train_discriminator_fn = theano.function([input_var, noise_var], (loss_discriminator, accuracy_discriminator), updates=updates_discriminator)

# Compile another theano function to get some fake images from the generator
generator_fn = theano.function([noise_var], output_generator_deterministic)


# Finally, launch the training loop.
print('--------------------')
print("Starting training...")

X=train_set

for j in range(argEpochs):
    # start timing
    start_time = time.time()
    #print("discriminator phase")
    for i in range(1):
        # get a random batch of real images
        x = X[np.random.randint(0, X.shape[0], size=batch_size)]
        # get some random noise to feed the generator
        random_input = (np.random.uniform(low=-1, size=(batch_size, NU))).astype('float32')
        # train the discriminator with the real and the fake images
        total_discriminator_loss, total_discriminator_accuracy = train_discriminator_fn(x, random_input)

    #print("generator phase")
    tot_fake_discr_loss = []
    tot_fake_discr_acc = []
    for i in range(5):
        # get some random noise to feed the generator
        random_input = (np.random.uniform(low=-1, size=(batch_size, NU))).astype('float32')
        # train the generator
        fake_discriminator_loss, fake_discriminator_accuracy = train_generator_fn(random_input)
        tot_fake_discr_loss.append(fake_discriminator_loss)
        tot_fake_discr_acc.append(fake_discriminator_accuracy)

    # Then we print the results for this epoch:
    print("Epoch {} of {} took {:.3f}s - Generator loss:{:.5f} - Discriminator loss:{:.5f} - Discriminator Total_Acc:{:.2f} - Discriminator Fake_Acc:{:.2f}".format(j + 1, argEpochs, time.time() - start_time, np.mean(tot_fake_discr_loss), total_discriminator_loss, total_discriminator_accuracy, np.mean(tot_fake_discr_acc)))


    if j%50==0:
        imgs = generator_fn(random_input)
        show(imgs[:32], j)
