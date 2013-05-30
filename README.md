# Nak

Nak is a Scala/Java library for machine learning and related tasks, with a focus on having an easy to use API for some standard algorithms. It is formed from [Breeze](https://github.com/scalanlp/breeze), [Liblinear Java](http://liblinear.bwaldvogel.de/), and [Scalabha](https://github.com/utcompling/Scalabha). It is currently undergoing a pretty massive evolution, so be prepared for quite big changes in the API for this and probably several future versions. 

We'd love to have some more contributors: if you are interested in helping out, please see [the #helpwanted issues](https://github.com/scalanlp/nak/issues/search?q=%23helpwanted) or suggest your own ideas.

## What's inside

Nak currently provides implementations for k-means clustering and supervised learning with logistic regression and support vector machines. Other algorithms will be added soon, especially due to the planned merger of [breeze.learn](https://github.com/scalanlp/breeze/tree/master/learn) with Nak. (See [the ScalaNLP roadmap](https://github.com/scalanlp/breeze/wiki/ScalaNLP-Roadmap) for details.)

See [the Nak wiki](https://github.com/scalanlp/nak/wiki) for (some preliminary) documentation.

The latest stable release of Nak is 1.1.3. Changes from the previous release include:

* Enabled the hashing trick to be used for linear models. See nak.data.HashedExampleIndex and nak.data.HashedFeatureMap.
* PCA now supported directly in Nak for dimensionality reduction w/ kmeans.
* All OpenNLP Maxent code purged from Nak.

See the [CHANGELOG](https://github.com/scalanlp/nak/wiki/CHANGELOG) for changes in previous versions.

## Using Nak

In SBT:

    libraryDependencies += "org.scalanlp" % "nak" % "1.1.3"

In Maven:

    <dependency>
       <groupId>org.scalanlp</groupId>
       <artifactId>nak</artifactId>
       <version>1.1.3</version>
    </dependency>


## Example

Here's an example of how easy it is to train and evaluate a text classifier using Nak. See [TwentyNewsGroups.scala](https://github.com/scalanlp/nak/blob/master/src/main/scala/nak/example/TwentyNewsGroups.scala) for more details.


```scala
def main(args: Array[String]) {
  val newsgroupsDir = new File(args(0))
  implicit val isoCodec = scala.io.Codec("ISO-8859-1")
  val stopwords = Set("the","a","an","of","in","for","by","on")

  val trainDir = new File(newsgroupsDir, "20news-bydate-train")
  val trainingExamples = fromLabeledDirs(trainDir).toList
  val config = LiblinearConfig(cost=5.0)
  val featurizer = new BowFeaturizer(stopwords)
  val classifier = trainClassifier(config, featurizer, trainingExamples)

  val evalDir = new File(newsgroupsDir, "20news-bydate-test")
  val maxLabelNews = maxLabel(classifier.labels) _
  val comparisons = for (ex <- fromLabeledDirs(evalDir).toList) yield 
    (ex.label, maxLabelNews(classifier.evalRaw(ex.features)), ex.features)
  val (goldLabels, predictions, inputs) = comparisons.unzip3
  println(ConfusionMatrix(goldLabels, predictions, inputs))
}
```


## Questions or suggestions?

Post a message to the [scalanlp-discuss](https://groups.google.com/forum/?fromgroups#!forum/scalanlp-discuss) mailing list or create [an issue](https://github.com/scalanlp/nak/issues).


