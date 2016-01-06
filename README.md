# Custom K means code in R

The aim of this project is to form a simple, understandable k means code for beginners in R. I am sure people who are from other programming languages will find this useful as its flavour is not completely R. I have introduced certain good elements of R like data frames, but the computations are mostly of a Pythonic flavour.


The core k means code is in kmeans.R . The function should be called as below:

					CustomKMeans(< Data Frame of Data without label > , < distance function > , < Average function > , < l matching centroids > , < threshold value to stop > , < Maximum number of iterations > )

As you can see I have also provided several additions to control the result of your k means analysis.

# KL Means

You can specify a l number of centroids to be associated with each block of data. So in sense you can build a 5 means analysis with each of the 5 blocks mapped to the nearest 2 centroids. This does not make sense for k = l as all the blocks are going to be mapped with all centroids. But in cases where finding the principal component proves to be difficult, we can calculate the determining features easily by finding those points which are similar in many ways.

# Threshold and Iterate

By setting the right number of iterations and threshold value for restricting convergence, we can come to faster result times. If ratio of the distance between current centroid of some position and next centroid of same position to the distance of previous iteration is lesser than the threshold, we make it that the algorithm has converged to a result. 

					threshold < (next[i] - current[i]) / previous_difference

If there is no convergence this was, we quit when the number of iterations has equalled the iteration limit.

# Distance and Average functions

distance and average functions should be built custom and provided as strings. The average function is the one which determines the new centroids in each iteration. It finds the distance between each data point in a block to the centroid of that block, takes the mean value in it and assigns that as the new centroid. This is the average function I thought of, at least :) . This is very crucial for convergence of results.

# K means analysis on Breast Cancer Data

With this k means algorithms I have done a sample analysis on Breast Cancer Data from Wisconsin Breast Cancer Database. A simple data which classifies patients as having "Benign" or "Malignant" cancer based on various features. I have not done any Principal Component Analysis as I wanted a barebones result of my K means code. I have used Euclidean distance and the average function is as described above. I have also provided ways to plot data and attached the results of the plot and cluster plot with KL means along with the code for your amusement (lets face it, plots in R are mostly for that when we are mining data on our own :) :) )

I have also done a sample Positive Predictive Value (PPV) calculation for the computed labels. If you are going to do a k equals number of labels sort of classification.