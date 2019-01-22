import theano
import theano.tensor as T
import lasagne
from lasagne.layers import DenseLayer, DropoutLayer, ReshapeLayer, InputLayer, FlattenLayer, Upscale2DLayer
from lasagne.layers.dnn import MaxPool2DDNNLayer, Conv2DDNNLayer, Pool2DDNNLayer
from lasagne.layers import batch_norm


#building the generator network
def createGenerator2(input_var=None):

	_ = InputLayer(shape=(None, 64), input_var=input_var)
	_ = batch_norm(DenseLayer(_, num_units=1000, nonlinearity=lasagne.nonlinearities.rectify))
	_ = batch_norm(DenseLayer(_, num_units=64*16*16, nonlinearity=lasagne.nonlinearities.rectify))
	_ = ReshapeLayer(_, ([0], 64, 16, 16))
	_ = batch_norm(Conv2DDNNLayer(_, 128, 3, pad='same'))
	_ = Upscale2DLayer(_, 2)
	_ = batch_norm(Conv2DDNNLayer(_, 128, 3, pad='same'))
	_ = Upscale2DLayer(_, 2)
	_ = batch_norm(Conv2DDNNLayer(_, 256, 3, pad='same'))
	_ = batch_norm(Conv2DDNNLayer(_, 256, 3, pad='same'))
	l_generator = batch_norm(Conv2DDNNLayer(_, 3, 3, pad='same', nonlinearity=lasagne.nonlinearities.sigmoid))


	print('--------------------')
	print('Generator architecture: \n')

	#get all layers
	allLayers=lasagne.layers.get_all_layers(l_generator)
	#for each layer print its shape information
	for l in allLayers:
		print(lasagne.layers.get_output_shape(l))

	print ("Generator output:", l_generator.output_shape)
	return l_generator

#building the discriminator network
def createDiscriminator2(input_var=None):

	_ = InputLayer(shape=(None, 3, 64, 64), input_var=input_var)
	_ = batch_norm(Conv2DDNNLayer(_, 64, 3, pad='same'))
	_ = batch_norm(Conv2DDNNLayer(_, 64, 3, pad='same'))
	_ = MaxPool2DDNNLayer(_, 2)
	_ = batch_norm(Conv2DDNNLayer(_, 64, 3, pad='same'))
	_ = MaxPool2DDNNLayer(_, 2)
	_ = batch_norm(Conv2DDNNLayer(_, 128, 3, pad='same'))
	_ = batch_norm(Conv2DDNNLayer(_, 128, 3, pad='same'))
	_ = FlattenLayer(_)
	_ = DenseLayer(_, num_units=1000, nonlinearity=lasagne.nonlinearities.rectify)
	l_discriminator = DenseLayer(_, num_units=1, nonlinearity=lasagne.nonlinearities.sigmoid)

	print('--------------------')
	print('Discriminator architecture: \n')

	#get all layers
	allLayers=lasagne.layers.get_all_layers(l_discriminator)
	#for each layer print its shape information
	for l in allLayers:
		print(lasagne.layers.get_output_shape(l))

	print ("Discriminator output:", l_discriminator.output_shape)
	return l_discriminator

#visualising and saving sample generator images
def show(x, j):
    printImg=x.swapaxes(1, 3).swapaxes(1, 2)
    map(lambda x: plt.imsave('solo/'+str(j)+str(randint(1,1000))+'image.png', x), printImg)
    w = min(x.shape[0]*100, 800)
    x = x.swapaxes(0, 1).swapaxes(1,2)
    x = x.reshape(3, 64, -1)
    img = np.swapaxes(x, 0, 1)
    img = np.swapaxes(img, 1, 2)
    plt.imsave('reel/'+str(j)+'image.png', img)
    plt.show(plt.imshow(img))

#imports all image names from a specific path and turns them into numpy arrays,
#so that they can be fed to theano models

def loadImages(path, imageHeight, imageWidth, imageChannels):
	#get all filenames
	filenames = [f for f in listdir(path) if isfile(join(path, f))]

	print('--------------------')
	print('Loading images...')
	start_time = time.time()

	#define the size of the image
	ImageSize = (imageHeight, imageWidth)
	#define the number of channels of the image
	NChannelsPerImage = imageChannels

	#the code below loads all the images
	imagesData = [ Image.open(path + '/' + f, 'r') for f in filenames ]

	#extracts the id of the imageimport math
	imageIDs=list()
	for f in filenames:
		id=int(filter(str.isdigit, f))
		imageIDs.append(id)

	imageIDs=np.array(imageIDs)

	allImages = np.array(map(threeDimensions, map(np.asarray, imagesData)))

	nImages = len(filenames)

	allImages = np.rollaxis(allImages, 3, 1)
	#sort the images according to their ID
	allImages=allImages[imageIDs.argsort()]

	end_time = time.time()

	#convert to float32
	allImages = allImages.astype(np.float32)

	print('Loaded and reshaped %d images in %.2f seconds' % (len(filenames), end_time-start_time))

	return allImages

#takes images as input
#returns the first three channels of the image
def threeDimensions(images):
	images=images[:, :, 0:3]

	return images
