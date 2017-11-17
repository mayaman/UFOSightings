
from __future__ import print_function
from pyspark import SparkContext, SparkConf
from pyspark.mllib.clustering import KMeans, KMeansModel
import math
from pyspark.mllib.feature import StandardScaler, StandardScalerModel
from collections import Counter


def parsePoint(line):
	values = line.split(',')
	values = [0 if e == '' else int(e) for e in values]
	return values[0], values[1:]

def mostCommon(L):
    L = [ (L.count(x), x) for x in L]
    L.sort()
    return L[:2]

if __name__ == "__main__":

    conf = SparkConf()
    conf.set("spark.executor.memory", "8g")
    sc = SparkContext(appName="MNIST_KMEANS", conf=conf)
    # every row has multiple columns
    data = sc.textFile('../lat_long_dat.csv')  # ingest the comma delimited file
	# remove header
    header = data.first() # extract header
    data = data.filter(lambda x: x != header)  # remove the header
    data = data.map(parsePoint)  # parse file to generate an RDD
    # trainingData, testData = data.randomSplit([0.7, 0.3])
    # don't have to do bc Kmeans unsupervised
    # trainingData_wo_labels = trainingData.map(lambda x: x[1]) # remove label
    # trainingData_w_labels = trainingData.map(lambda x: x[0]) # remove label

    # normalize vector
    scaler = StandardScaler(withMean=True, withStd=True).fit(data)
    # trainingData_wo_labels = scaler.transform(trainingData_wo_labels)

	# training data, num of clusters, etc.
    model = KMeans.train(data,
                        10, maxIterations=350, initializationMode="random")

    # Evaluate clustering by computing Within Set Sum of Squared Errors
    #def error(point):
    #    center = model.centers[model.predict(point)]    # get centroid for cluster
    #    return math.sqrt(sum([x**2 for x in (point - center)]))

    #WSSSE = trainingData_wo_labels.map(lambda point: error(point)).reduce(lambda x, y: x + y)
    #print("Within Set Sum of Squared Error = " + str(WSSSE))

    # Need to map clusters to labels

	# giving a data point, figuring out what cluster it belongs to.
    def getClusterNum(point):
        clusterNum = model.predict(point)
        return clusterNum


    clusterRDD = data.map(lambda x: (getClusterNum(x), 1)).reduceByKey(lambda a, b: a + b)
	clusterRDD.saveAsTextFile("lat_long_output.txt")
    # zipRDD = trainingData_w_labels.zip(clusterRDD)
    # mappingRDD = zipRDD.groupByKey().map(lambda x : (x[0], mostCommon(list(x[1]))))
    # mappingRDD.foreach(print)

    sc.stop()
