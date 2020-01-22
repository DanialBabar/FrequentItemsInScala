package Assignment

import Assignment.items.Itemset

import scala.collection.immutable.SortedMap
import scala.io.Source

object Utility {

  var replicateNTimes: Int = 1

  def Support(minSupport: Double, numTransactions: Int) = (numTransactions * minSupport + 0.5).toInt

  def transactionParse(lines: List[String], separator: String): List[Itemset] = {
    lines.filter(l => !l.startsWith("#"))
      .filter(!_.trim.isEmpty)
      .map(l => l.split(separator + "+"))
      .map(l => l.map(item => item.trim).toList)
  }

  def transactionParse(fileName: String, separator: String = ","): List[Itemset] = {

    transactionParse(
      (1 to replicateNTimes).flatMap(_ => {
        val file = Source.fromFile(fileName, "UTF-8")
        file.getLines
      }).toList, separator)
  }

  def transactionParseByText(text: String): List[Itemset] = {
    transactionParse(text.split("\n").toList, ",")
  }

  def printItemsets(items: List[Itemset]) = {
    println(s"Found ${items.size} items")
    SortedMap(items.groupBy(itemset => itemset.size).toSeq: _*)
      .mapValues(i => i.map(set => s"{${set.sorted.mkString(", ")}}").sorted.mkString(", "))
      .foreach(t => println(s"[${t._1}] ${t._2}"))
  }
}
