# Nak

Nak is a Scala/Java library for machine learning and related tasks, with a focus on having an easy to use API for some standard algorithms. It is formed from [Breeze](https://github.com/scalanlp/breeze), [Liblinear Java](http://liblinear.bwaldvogel.de/), [the OpenNLP Maxent package](http://opennlp.apache.org/) and [Scalabha](https://github.com/utcompling/Scalabha). It is currently undergoing a pretty massive evolution, so be prepared for quite big changes in the API for this and probably several future versions. 

We'd love to have some more contributors: if you are interested in helping out, please see [the #helpwanted issues](https://github.com/scalanlp/nak/issues/search?q=%23helpwanted) or suggest your own ideas.

## What's inside

Nak currently provides implementations for k-means clustering and supervised learning with logistic regression and support vector machines. Other algorithms will be added soon, especially due to the planned merger of [breeze.learn](https://github.com/scalanlp/breeze/tree/master/learn) with Nak. (See [the ScalaNLP roadmap](https://github.com/scalanlp/breeze/wiki/ScalaNLP-Roadmap) for details.)

See [the Nak wiki](https://github.com/scalanlp/nak/wiki) for (some preliminary) documentation.

The latest stable release of Nak is 1.1.2. Changes from the previous release include:

* Incorporated breeze.data classes into nak.data and updated liblinear training and model use to work with these rather than the legacy opennlp classes.
* Added nak.data.Featurizer and nak.core.FeaturizedClassifier, which handle turning raw data into collections of features. When combined with IndexedClassifier, the indexation of these features is handled automatically so that a user of the API doesn't need to ever worry about the low level and can focus on the data and feature extraction. 
* Added NakContext object (inspired by SparkContext), which provides a number of utility methods for getting classifiers up and running.
* Added nak.example package, with example implementations for prepositional phrase attachment (PpaExample) and text classification (TwentyNewsGroupsExample).
* Added nak.util.ConfusionMatrix class, which provides detailed error output.
* Added ScalaTest and started writing BDD tests.
* Refactored a lot of code to get rid of duplication.
* Added code documentation to many classes and functions.

See the [CHANGELOG](https://github.com/scalanlp/nak/wiki/CHANGELOG) for changes in previous versions.

## Using Nak

In SBT:

    libraryDependencies += "org.scalanlp" % "nak" % "1.1.2"

In Maven:

    <dependency>
       <groupId>org.scalanlp</groupId>
       <artifactId>nak</artifactId>
       <version>1.1.2</version>
    </dependency>

**Note**: There is one dependency that won't get pulled along: pca_transform-0.7.2.jar in the lib directory is not available on any repository, so you'll need to add that to your classpath by hand if (and only if) you want to be able to use PCA transformations for input to k-means.

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


# Questions or suggestions?

Post a message to the [scalanlp-discuss](https://groups.google.com/forum/?fromgroups#!forum/scalanlp-discuss) mailing list or create [an issue](https://github.com/scalanlp/nak/issues).


