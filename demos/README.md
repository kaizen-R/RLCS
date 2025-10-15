# What this is

RLCS Examples on demo datasets.

Current focus of the demos is on Data Mining and Supervised Learning.

----

# The Examples
## Dummy Binary Operations: Data Mining

See the code in "demo_data_mining.R". It is very simple and meant to show in a 
simple way the power of a Rule-based model.

----

## Iris Dataset: Supervised Learning

The complications of the RLCS sometimes stem from not seeing how to transform
the input data into compatible, binary input strings for the "state".

A basic "Rosetta Stone" function is included with the package ONLY MEANT for this
particular demo with the Iris dataset.

See ??iris for details on the dataset used for this example. It is well known.

The Example code in "demo_sl_iris.R" goes through:

- Showing a proposed encoding
- An example set of RLCS hyperparameters with minimal explanatory comments
- Training a Supervised Learning RLCS model
- Getting and checking RLCS Predictions made by the above Model
- Glancing at the model
- Hinting at work required to better interpret such a model

----

## MNIST (much simplified): Supervised Learning & Hints of Parallel Processing!

### First part: Data and visualizations

The MNIST data is a very common data-set for ML applied to visual classifiers.
(See note below.) In this example, we make the case for a more generic application of the RLCS. 
The dataset provided with RLCS is pre-encoded so that it already has (compressed)
binary states as valid input for training/testing.

Following the example, you might notice the data is in fact already simplified
from the original MNIST images, which are grey-scale, 28*28 pixels images. 
Our data was generated with a "kernel" of sorts, making sure the outcome was
binary data and scaled down to 7x7 matrices. This implies of course a data loss.

### Second Part: Training

With the 49 bits vectors as input states, we can train a model in a Supervised Learning scenario.

From there, the example's code second section is almost identical to that of the IRIS example above.
Upfront, you might also notice this example trains on ONLY 800 sample data points.
And tests on many more!

This is not the usual setup in Supervised Learning. And yet, the results are quite good!


### Third part: Parallel Processing

HOWEVER, a third section is provided.
As the input strings (49 bits) are larger than with the Iris dataset (16 bits),
the search space is potentially much larger.

Only as a hint of what COULD be done, I include some code for parallel 
processing applied to the training.
Using dopar, I split the training dataset and train separately on each subset, creating
"sub-models".

Then you might notice I put together the different "sub-models" by simply concatenating them!
A bit of post-processing, and you get a (bigger) model that performs just as well but takes a fraction of the time for training.
Parallel processing (on CPU) for RLCS training is currently my area of "research".

### Thanks note:

The "simplified MNIST" data-set included with the RLCS package is a very limited 
subset of compressed versions of the images in the original MNIST data.

This simplified example data set was created by leveraging the useful {dslabs} package by 
Rafael A. Irizarry, Amy Gill. Said {dslabs} package is available on CRAN.
(The package is in fact using the original MNIST dataset from source. But it 
does facilitate access from R. :))
I simply applied a few transformations on a subset of the example data.
I also provide the code that was used to do so for reference
(see "demo_sl_mnist_ExplainingDataset.R").

This could help you in turn extend the example for your own investigations.

----

# NOTE: Future examples.

A functional RL demo exists and all its functionality comes included/packaged
within the RLCS package.
But is currently complex as it isn't well separated from the corresponding Demo
World. The EXAMPLE code is therefore not included with the package per-se.
Even if the underlying functionality is.

Instead, said demo is only meant for live demonstrations.
