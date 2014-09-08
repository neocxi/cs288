import edu.berkeley.nlp.langmodel.EmpiricalUnigramLanguageModel
import java.io.File
import java.lang.Runtime
import java.lang.UnsupportedOperationException
import edu.berkeley.nlp.io.SentenceCollection
import edu.berkeley.nlp.langmodel.EmpiricalUnigramLanguageModel.EmpiricalUnigramLanguageModelFactory
import edu.berkeley.nlp.langmodel.EnglishWordIndexer
import scala.collection.JavaConversions._
import scala.collection.BitSet

//import java.util.List
//import java.util.ArrayList
import java.util.HashSet
import edu.berkeley.nlp.langmodel.EnglishWordIndexer
import edu.berkeley.nlp.langmodel.LanguageModelFactory
import edu.berkeley.nlp.langmodel.NgramLanguageModel
import edu.berkeley.nlp.util.CollectionUtils
import scala.collection.JavaConversions._

import scala.math

//package com.officedepot.cdap2.collection

//package edu.berkeley.nlp.assignments.assign1.student {

  object BitUtils {
    def getBits(bytes: Array[Byte], bitFrom: Long, bitTo: Long) : Long = {
      var result = 0L
      val byteFrom = math.round(math.floor(bitFrom * 1.0 / 8)).toLong
      val byteTo = math.round(math.floor(bitTo * 1.0 / 8)).toLong

      for (i <- byteFrom to byteTo) {
        //println("i: " + i)
        //println("byte from: " + byteFrom)
        //println("byte to: " + byteTo)
        val currentBitFrom = (if (i * 8 >= bitFrom) 0 else bitFrom % 8).toInt
        val currentBitTo = (if (i * 8 + 7 <= bitTo) 7 else bitTo % 8).toInt
        val currentBits = (bytes(i.toInt) & ((1 << (8 - currentBitFrom)) - 1)) >> (7 - currentBitTo)
        result = (result << (currentBitTo - currentBitFrom + 1)) | currentBits
      }
      result
    }

    // to: inclusive

    def setBits(bytes: Array[Byte], bitFrom: Long, bitTo: Long, value: Long) : Unit = {
      //var result = 0L
      var valueLeft = value
      val byteFrom = math.round(math.floor(bitFrom * 1.0 / 8)).toLong
      val byteTo = math.round(math.floor(bitTo * 1.0 / 8)).toLong
      for (i <- byteTo to byteFrom by -1L) {
        val currentBitFrom = (if (i * 8 >= bitFrom) 0 else bitFrom % 8).toInt
        val currentBitTo = (if (i * 8 + 7 <= bitTo) 7 else bitTo % 8).toInt
        val currentValue = (valueLeft & ((1 << (currentBitTo - currentBitFrom + 1)) - 1)).toByte
        val liftedCurrentValue = ((currentValue << (7 - currentBitTo)) & 0xff).toByte
        val mask = (0xff - ((1 << (8 - currentBitFrom)) - 1) + ((1 << (7 - currentBitTo)) - 1)).toByte
        bytes(i.toInt) = (bytes(i.toInt) & mask).toByte
        bytes(i.toInt) = (bytes(i.toInt) | liftedCurrentValue).toByte
        valueLeft = valueLeft >> (currentBitTo - currentBitFrom + 1)
        //val currentBits = (bytes(i) & ((1 << (8 - currentBitFrom)) - 1)) >> (7 - currentBitTo)
        //result = (result << (currentBitTo - currentBitFrom + 1)) | currentBits
      }
    }



  }
  // It is recommended to use a prime number for the capacity
  // When using this class, one needs to make sure that ceil(log(maxKey / capacity)) + countBits <= 64
  // and that the actual count will never exceed 2^countBits - 1
  class HashCounter(capacity: Int, keyBits: Int, countBits: Int)
  {
    // use big-endian encoding here
    val entryBits = keyBits + countBits

    val totalBytes = math.round(math.ceil(entryBits.toLong * capacity.toLong * 1.0 / 8)).toInt

    //println ("total bytes: " + totalBytes)

    val data = new Array[Byte](totalBytes)//Array.fill[Byte](totalBytes)(-1)

    def incCount(key: Long) : Unit = {
      var index = ((key.toString.hashCode % capacity + capacity) % capacity).toInt
      //println ("index: " + index)
      var inc = 1
      while (this.getCountAtIndex(index) != 0) {//((1 << countBits) - 1)) {
        val keyAtIndex = this.getKeyAtIndex(index)
        if (keyAtIndex == key) {
          // note the catastrophic consequence here if there's overflow in count bits
          this.setCountAtIndex(index, this.getCountAtIndex(index) + 1)
          //if (inc > 10) println ("collsion: inc to " + inc)
          //println("setting index " + index + " to " + this.getCountAtIndex(index))
          return
        }
        //println ("incrementing because of collision")
        index = (index + inc * inc) % capacity
        inc = inc + 1
      }
      //if (inc > 10) println ("collsion: inc to " + inc)
      
      // if empty, simply set the count to 1
      //println("setting index " + index + " to 1")
      this.setKeyAtIndex(index, key)
      this.setCountAtIndex(index, 1L)
    }

    def getCount(key: Long) : Long = {
      var index = ((key.toString.hashCode % capacity + capacity) % capacity).toInt
      //var index = (key.toString.hashCode % capacity).toInt
      var inc = 1
      while (this.getCountAtIndex(index) != 0) {
        val keyAtIndex = this.getKeyAtIndex(index)
        if (keyAtIndex == key) {
          //println ("getting index" + index)
          return this.getCountAtIndex(index)
        }
        index = (index + inc * inc) % capacity
        inc = inc + 1
      }
      0
    }

    //def 
    
    def getKeyAtIndex(index: Int) : Long = {
      BitUtils.getBits(data, index.toLong * entryBits.toLong, index.toLong * entryBits.toLong + keyBits - 1L)
    }

    def getCountAtIndex(index: Int) : Long = {
      BitUtils.getBits(data, index.toLong * entryBits.toLong + keyBits, index.toLong * entryBits.toLong + keyBits + countBits - 1L)
    }

    def setKeyAtIndex(index: Int, value: Long) : Unit = {
      BitUtils.setBits(data, index.toLong * entryBits.toLong, index.toLong * entryBits.toLong + keyBits - 1L, value)
    }

    def setCountAtIndex(index: Int, value: Long) : Unit = {
      BitUtils.setBits(data, index.toLong * entryBits.toLong + keyBits, index.toLong * entryBits.toLong + keyBits + countBits - 1L, value)
    }


  }

  class LmFactory extends LanguageModelFactory
  {
    def newLanguageModel(trainingData: java.lang.Iterable[java.util.List[String]]) : NgramLanguageModel =
    {
      new KNTrigramLanguageModel (trainingData)
    }
  }

  class NgramCounter
  {
    //val indexer = EnglishWordIndexer.getIndexer

    //def getCount(ngram: Seq[String]) {
    //  val ngramIndex = ngram.map{word => indexer.addAndGetIndex(word)}
    //  this.getCount(ngramIndex)
    //}

    //def incCount(ngram: Seq[String]) {
    //  val ngramIndex = ngram.map{word => indexer.addAndGetIndex(word)}
    //  this.getCount(ngramIndex)
    //}
    //
    //
    //val unigramSet = new HashSet[Int]()
    //val bigramSet = new HashSet[Long]()
    //val trigramSet = new HashSet[Long]()

    val hashCounter = new HashCounter(80000023, 60, 25)

    //def getNgramSet = ngramSet
    //

    //def unigramSize = unigramSet.size

    //def bigramSize = bigramSet.size
   
    //def trigramSize = trigramSet.size
    //

    def getKey(ngram: Array[Int]) = {
      if (ngram.length == 1) {
        ngram(0) | 0xffffffffff00000L
      } else if (ngram.length == 2) {
        ngram(0).toLong | (ngram(1).toLong << 20) | 0xfffff0000000000L
      } else if (ngram.length == 3) {
        ngram(0).toLong | (ngram(1).toLong << 20) | (ngram(2).toLong << 40)
      } else {
        val len = ngram.length
        throw new UnsupportedOperationException(s"must be between 1 and 3: $len")
      }
    }
    
    def getCount(ngram: Array[Int]) = {
      hashCounter.getCount(this.getKey(ngram))
    }

    def getFertility(ngram: Array[Int]) = 0

    def getSummedFertility(ngram: Array[Int]) = 0

    var total = 0

    def getTotal = total

    def incCount(ngram: Array[Int]) = {
      if (ngram.length == 1) total += 1
      hashCounter.incCount(this.getKey(ngram))
    }


  }


  class KNTrigramLanguageModel(sentenceCollection: java.lang.Iterable[java.util.List[String]]) extends NgramLanguageModel
  {

    val ngramCounter = new NgramCounter()

    val indexer = EnglishWordIndexer.getIndexer

    def index (wordList: Array[String]) : Array[Int] = {
      wordList.map(word => indexer.addAndGetIndex(word))
    }

    def getNgramCounter = ngramCounter

    sentenceCollection.view.zipWithIndex foreach { case (sentence, sid) =>

      if (sid % 100000 == 0) {
        println (s"On sentence $sid")

        //System.gc()
        val mem = Runtime.getRuntime.totalMemory / 1024 / 1024
        println (s"total memory: $mem")
      }

      val stoppedSentence = NgramLanguageModel.START +: sentence :+ NgramLanguageModel.STOP

      for (wid <- 1 until stoppedSentence.length - 1) {
        ngramCounter.incCount(index(stoppedSentence.slice(wid, wid + 1).toArray))
        if (wid - 1 >= 0) {
          // wid - 1, wid
          ngramCounter.incCount(index(stoppedSentence.slice(wid - 1, wid + 1).toArray))
        }
        if (wid - 2 >= 0) {
          // (wid - 2, wid - 1, wid)
          ngramCounter.incCount(index(stoppedSentence.slice(wid - 2, wid + 1).toArray))
        }
      }
    }

    def getOrder(): Int = 1

    def getCount(ngram: Array[Int]): Long = ngramCounter.getCount(ngram)

    //def getFertility(ngram: Array[Int]): Long = ngramCounter.getFertility(ngram)

    //def getSummedFertility(ngram: Array[Int]): Long = ngramCounter.getSummedFertility(ngram)

    val smoothingFactor = 1

    val d = 0.75

    def getNgramLogProbability(ngram: Array[Int], from: Int, to: Int): Double = {
      Math.log (this.getNgramProbability(ngram, from, to))
    }

    def getNgramProbability(ngram: Array[Int], from: Int, to: Int): Double = {
      math.max(this.getCount(ngram), 1) * 1.0 / (ngramCounter.getTotal + 1)

      //if (to - from == 1) {
      //  val unigramFertility = Math.max(this.getFertility(ngram.slice(from, to)), smoothingFactor)
      //  val summedFertility = this.getSummedFertility(Array[Int]()) + smoothingFactor
      //  unigramFertility * 1.0 / summedFertility // Note: no discount here
      //} else if (to - from == 2) {
      //  val bigramFertility = Math.max(this.getFertility(ngram.slice(from, to)), smoothingFactor)
      //  val summedFertility = this.getSummedFertility(ngram.slice(from, to - 1)) + smoothingFactor
      //  val alphaPrev = d * ngramCounter.totalWords / summedFertility
      //  (bigramFertility - d) / summedFertility + alphaPrev * this.getNgramProbability(ngram, from + 1, to)
      //} else if (to - from == 3) {
      //  val trigramCount = Math.max(this.getCount(ngram.slice(from, to)) - d, 0)
      //  val prefixCount = this.getCount(ngram.slice(from, to - 1))
      //  val alphaPrev = d * ngramCounter.totalWords / prefixCount
      //  trigramCount - d) / prefixCount + alphaPrev * this.getNgramProbability(ngram, from + 1, to)
      //} else {
      //  throw new UnsupportedOperationException(s"Constraint violated: 1 <= to - from <= 3")
      //}
    }
  }

//}

//import edu.berkeley.nlp.assignments.assign1.student._


object Main {

  //val indexer = EnglishWordIndexer.getIndexer

  //def index (wordList: Array[String]) : Array[Int] = {
  //  wordList.map(word => indexer.addAndGetIndex(word))
  //}

  def main {
    val prefix = ""//"sanity_"
    val basePath = "data"
    // Read in all the assignment data
    val trainingSentencesFile = new File(basePath, prefix + "training.en.gz")
    val phraseTableFile = new File(basePath, prefix + "phrasetable.txt.gz")
    
    val testFrench = new File(basePath, prefix + "test.fr")
    val testEnglish = new File(basePath, prefix + "test.en")
    val weightsFile = new File(basePath, "weights.txt")
    
    val trainingSentenceCollection = SentenceCollection.Reader.readSentenceCollection(trainingSentencesFile.getPath)
    
    //val uni = new EmpiricalUnigramLanguageModelFactory().newLanguageModel(trainingSentenceCollection)
    val tri = new LmFactory().newLanguageModel(trainingSentenceCollection).asInstanceOf[KNTrigramLanguageModel]

    val mem = Runtime.getRuntime.totalMemory / 1024 / 1024
    println (s"total memory: $mem M")

    //
    //
    //val unigramSize = tri.getNgramCounter.unigramSize
    //val bigramSize = tri.getNgramCounter.bigramSize
    //val trigramSize = tri.getNgramCounter.trigramSize

    //println (s"total number of unigrams: $unigramSize")
    //println (s"total number of bigrams: $bigramSize")
    //println (s"total number of trigrams: $trigramSize")
    //

    println ("should be 19880264: " + tri.getCount(tri.index(Array("the"))))
    println ("should be 31257: " + tri.getCount(tri.index(Array("in", "terms", "of"))))
    println ("should be 30: " + tri.getCount(tri.index(Array("romanian", "independent", "society"))))
    println ("should be 0: " + tri.getCount(tri.index(Array("XXXtotally", "XXXunseen", "XXXtrigram"))))
    //println ("count of the: " + uni.getCount(tri.index(Array("the"))))
    
    //println (uni.getNgramLogProbability(index(Array("the")), 0, 1))
    //println (tri.getNgramLogProbability(index(Array("the")), 0, 1))
    
    //println ("lala")
  }

  def testHashCounter {
    //val hashCounter = new Array[BitSet](1000000)//Array.fill[BitSet](80000023)(BitSet.empty)
    //for (i <- 0 until hashCounter.length) {
    //  hashCounter(i) = BitSet((1<<96)-1)
    //}
      
    
    // (123)10 = (01111011)2
    // (12345)10 = (00110000 00111001)2 = Array[Byte](48, 57)
    println ("should be 1: " + BitUtils.getBits(Array[Byte](123), 7, 7))
    println ("should be 3: " + BitUtils.getBits(Array[Byte](123), 6, 7))
    println ("should be 3: " + BitUtils.getBits(Array[Byte](123), 5, 7))
    println ("should be 11: " + BitUtils.getBits(Array[Byte](123), 4, 7))
    println ("should be 123: " + BitUtils.getBits(Array[Byte](123), 0, 7))
    println ("should be 12345: " + BitUtils.getBits(Array[Byte](48, 57), 0, 15))
    println ("should be 1: " + BitUtils.getBits(Array[Byte](48, 57), 4, 10))

    val arr = Array[Byte](0,0,0)

    BitUtils.setBits(arr, 3, 6, 12)
    println ("should be 12: "+ BitUtils.getBits(arr, 3, 6))

    val hashCounter = new HashCounter(80000023, 60, 25)
    println ("should be 0: " + hashCounter.getCountAtIndex(0))
    hashCounter.incCount(10)
    hashCounter.incCount(10)
    hashCounter.incCount(10 + 80000023L * 100)
    hashCounter.incCount(11)
    println ("should be 2: " + hashCounter.getCount(10))
    println ("should be 1: " + hashCounter.getCount(11))
    println ("should be 1: " + hashCounter.getCount(10 + 80000023L * 100))


    val mem = Runtime.getRuntime.totalMemory / 1024 / 1024
    println (s"total memory: $mem M")
  }
}


//Main.main


Main.main//testHashCounter
