package Assignment

import Assignment.items
.Itemset
import Assignment.Utility.printItemsets

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object items
{

  type Itemset = List[String]

  def main(args: Array[String]): Unit = {
    val minimumsuprot = 0.7
    val transactions: List[Itemset] = Utility.transactionParse("src/dataitems.csv")
    val frequentItemsets = new Assignment().execute(transactions, minimumsuprot)
    printItemsets(frequentItemsets)
  }
}

class Assignment {

  var single = List[String]()

  def execute(transactions: List[Itemset], minimumsuprot: Double): List[Itemset] = {
    val items
    = frequentItemset("", "", transactions, minimumsuprot)

    items

  }

  def frequentItemset(transactions: List[Itemset], minimumsuprot: Double): List[Itemset] = {
    val support = Utility.Support(minimumsuprot, transactions.size)

    single = findSingletons(transactions, support)

    val frequentItemsets = mutable.Map(1 -> single.map(i => List(i)))

    var k = 1
    while (frequentItemsets.get(k).nonEmpty) {
      k += 1
      val candidateKItemsets = findKItemsets(frequentItemsets(k - 1))
      val frequents = filterFrequentItemsets(candidateKItemsets, transactions, support)
      if (frequents.nonEmpty) {
        frequentItemsets.update(k, frequents)
      }
    }
    frequentItemsets.values.flatten.toList
  }

  def filterFrequentItemsets(possibleItemsets: List[Itemset], transactions: List[Itemset], minimumsuprot: Int) = {
    val map = mutable.Map() ++ possibleItemsets.map((_, 0)).toMap
    for (t <- transactions) {
      for ((itemset, count) <- map) {
        if (existInTransaction(itemset, t)) {
          map.update(itemset, count + 1)
        }
      }
    }
    map.filter(_._2 >= minimumsuprot).keySet.toList
  }

  private def findSingletons(transactions: List[Itemset], minimumsuprot: Int) = {
    transactions.flatten
      .groupBy(identity)
      .map(t => (t._1, t._2.size))
      .filter(_._2 >= minimumsuprot)
      .keySet.toList.sorted
  }


  def findKItemsets(items: List[Itemset]) = {
    val nextKItemsets = ArrayBuffer[Itemset]()
    for (i <- items.indices) {
      for (j <- i + 1 until items.size) {
        if (items(i).size == 1 || allElementsEqualButLast(items(i), items(j))) {
          val newItemset = (items(i) :+ items(j).last).sorted

          if (isItemsetValid(newItemset, items))
            nextKItemsets += newItemset
        }
      }
    }
    nextKItemsets.toList
  }

  def isItemsetValid(itemset: List[String], previousItemsets: List[Itemset]): Boolean = {
    for (i <- itemset.indices) {
      val subset = itemset.diff(List(itemset(i)))
      val found = previousItemsets.contains(subset)
      if (!found) {
        return false
      }
    }
    true
  }

  def allElementsEqualButLast(a: List[String], b: List[String]): Boolean = {
    for (i <- 0 until a.size - 1) {
      if (a(i) != b(i))
        return false
    }
    if (a.last == b.last) {
      return false
    }
    true
  }

  def existInTransaction(candidate: Itemset, transaction: Itemset): Boolean = {
    var result = true
    for (elem <- candidate) {
      if (!transaction.contains(elem))
        result = false
    }
    result
  }

  def frequentItemset(fileName: String, separator: String, transactions: List[Itemset], minimumsuprot: Double): List[Itemset] = {
    if (fileName.isEmpty) {
      frequentItemset(transactions, minimumsuprot)
    }
    else {
      frequentItemset(Utility.transactionParse(fileName, separator), minimumsuprot)
    }
  }

}