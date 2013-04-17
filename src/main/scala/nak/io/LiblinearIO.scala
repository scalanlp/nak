package nak.io

import nak.core._
import nak.data._
import nak.liblinear._
import java.io._ 

class LiblinearModelWriter(linearModel: LinearModel, output: DataOutputStream) 
extends nak.io.AbstractModelWriter {

  import java.util.zip.GZIPOutputStream

  // Safe to ignore these since we handle everything in the persist method.
  def writeUTF(s: String) {}
  def writeInt(i: Int) {}
  def writeDouble(d: Double) {}
  def close { output.flush; output.close }

  def persist {
    val classifier = linearModel.asInstanceOf[LiblinearClassifier]
    val model = classifier.model
    val features = classifier.fmap.toSeq.sortBy(_._2).unzip._1
    val parameters = model.getFeatureWeights
    assert(features.length == parameters.length)

    output.writeUTF("Liblinear")
    output.writeUTF(model.getSolverType.name)
    output.writeInt(classifier.labels.length)
    output.writeInt(features.length)
    output.writeDouble(model.getBias)
    
    classifier.labels.foreach(output.writeUTF)
    features.foreach(output.writeUTF)
    parameters.foreach(output.writeDouble)

    output.flush
    output.close
  }
}

class LiblinearModelReader(dataReader: nak.data.DataReader) 
extends nak.io.AbstractModelReader(dataReader) {

  import java.io._ 
  import java.util.zip.GZIPInputStream

  def checkModelType = assert(dataReader.readUTF == "Liblinear")

  def constructModel = {
    val input = dataReader

    val solverType = SolverType.valueOf(input.readUTF)
    val numLabels = input.readInt
    val numFeatures = input.readInt
    val bias = input.readDouble

    val labels = new Array[String](numLabels)
    var index = 0
    while (index < numLabels) {
      labels(index) = input.readUTF
      index += 1
    }

    val features = new Array[String](numFeatures)
    index = 0
    while (index < numFeatures) {
      features(index) = input.readUTF
      index += 1
    }

    val parameters = new Array[Double](numFeatures)
    index = 0
    while (index < numFeatures) {
      parameters(index) = input.readDouble
      index += 1
    }

    val liblinearLabels = (0 until numLabels).toArray

    val model = new Model(solverType, numLabels, liblinearLabels, numFeatures, bias, parameters)

    Classifier.createLegacy(model, labels, features)
  }
}
