package edu.berkeley.nlp.assignments.assign1.student
{

  import java.util.List
  import java.util.ArrayList
  import edu.berkeley.nlp.langmodel.EnglishWordIndexer
  import edu.berkeley.nlp.langmodel.LanguageModelFactory
  import edu.berkeley.nlp.langmodel.NgramLanguageModel
  import edu.berkeley.nlp.util.CollectionUtils
  import scala.collection.JavaConversions._

  class LmFactory extends LanguageModelFactory
  {
    def newLanguageModel(trainingData: java.lang.Iterable[List[String]]) : NgramLanguageModel =
    {
      println ("constructing lm language model")
      new KNTrigramLanguageModel (trainingData)
    }
  }

  class KNTrigramLanguageModel(sentenceCollection: java.lang.Iterable[List[String]]) extends NgramLanguageModel
  {

    var wordCounter: Array[Long] = Array (10)

    println ("Start building KNTrigramLanguageModel..")

    sentenceCollection.view.zipWithIndex foreach { case (sentence, i) =>
      println("On sentence " + i)
      val stoppedSentence = new ArrayList[String](sentence)
      stoppedSentence.add(0, NgramLanguageModel.START)
      stoppedSentence.add(NgramLanguageModel.STOP)
      for (word <- stoppedSentence) {
        val index = EnglishWordIndexer.getIndexer.addAndGetIndex(word)
        if (index >= wordCounter.length) wordCounter = CollectionUtils.copyOf(wordCounter, wordCounter.length * 2)
        wordCounter(index) += 1
      }
    }

    wordCounter = CollectionUtils.copyOf(wordCounter, EnglishWordIndexer.getIndexer.size)

    val total = CollectionUtils.sum(wordCounter)

    def getOrder(): Int = 1

    def getCount(ngram: Array[Int]): Long = {
      println ("getting count")
      if (ngram.length > 1) return 0
      val word = ngram(0)
      if (word < 0 || word >= wordCounter.length) return 0
      wordCounter(word)
    }

    def getNgramLogProbability(ngram: Array[Int], from: Int, to: Int): Double = {
      println ("getting ngram")
      var logProb = 0.0
      for (i <- from until to) {
        val word = ngram(i)
        val count = if (word < 0 || word >= wordCounter.length) 1.0 else wordCounter(word)
        println (s"count: $count")
        println (s"total: $total")
        logProb += Math.log(count / (total + 1.0))
      }
      logProb
    }
  }
}
