package nak.data

;

import java.net.URL;
import breeze.linalg.DenseVector

import scala.io.Source;

/*
 Copyright 2009 David Hall, Daniel Ramage
 
 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at 
 
 http://www.apache.org/licenses/LICENSE-2.0
 
 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License. 
*/

/**
 * A DataMatrix stores a double-valued label along with the double-valued features that go with it.
 *
 * TODO: change to DenseVector
 */
trait DataMatrix[L] {
  def rows: Seq[Example[L, DenseVector[Double]]]

  def row(i: Int): Example[L, DenseVector[Double]] = rows(i)

  def partition(f: Example[L, DenseVector[Double]] => Boolean) = rows.partition(f)
}

object DataMatrix {
  /**
   * Downloads a DataMatrix from a URL. The DataMatrix format is a space-separated values file of doubles
   * with one column a label column.
   * @param url: where
   * @param labelColumn which column (starting at 0) is the label. May be negative, in which case it starts from the end.
   * @param separator: a regex for delimeters. Defaults to \\s+
   * @param dropRow: delete the first row
   */
  def fromURL[L](url: URL, labelColumn: Int = 0, separator: String = "\\s+", dropRow: Boolean = false,
                 labelReader: String => L = identity[String] _): DataMatrix[L] = {
    fromSource[L](Source.fromURL(url), labelColumn, separator, dropRow, labelReader)
  }

  /**
   * Reads a DataMatrix from a Source. The DataMatrix format is a space-separated values file of doubles
   * with one column a label column.
   * @param src: where
   * @param labelColumn which column (starting at 0) is the label. May be negative, in which case it starts from the end.
   * @param separator: a regex for delimeters. Defaults to \\s+
   * @param dropRow: delete the first row
   */
  def fromSource[L](src: Source, labelColumn: Int = 0, separator: String = "\\s+", dropRow: Boolean = false,
                    labelReader: String => L = identity[String] _): DataMatrix[L] = {
    val rowsIterator = for {
      (line, i) <- src.getLines().zipWithIndex
      if !dropRow || i != 0
      allCols = line.split(separator) //map (_.toDouble)
      lbl = labelReader(allCols(if (labelColumn < 0) allCols.length + labelColumn else labelColumn))
      dataCols = DenseVector(allCols.patch(labelColumn, Seq.empty, 1).map(_.toDouble))
    } yield Example[L, DenseVector[Double]](label = lbl, features = dataCols, id = i.toString)

    new DataMatrix[L] {
      val rows = rowsIterator.toSeq
    }
  }
}
