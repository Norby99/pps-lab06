package ex2

import scala.annotation.targetName

object ConferenceReviewing:
  enum Question:
    case RELEVANCE, SIGNIFICANCE, CONFIDENCE, FINAL
  
  opaque type Article = Int
  opaque type Score = Int | Double
  
  def article(id: Int): Article = id
  def score(value: Int | Double): Score = value
  
  extension (x: Score)
    def toDouble: Double = x match
      case i: Int => i.toDouble
      case d: Double => d
    def >(y: Score): Boolean = x.toDouble > y.toDouble
    def <(y: Score): Boolean = x.toDouble < y.toDouble
    def >=(y: Score): Boolean = x.toDouble >= y.toDouble
    def <=(y: Score): Boolean = x.toDouble <= y.toDouble
    def +(y: Score): Score = x.toDouble + y.toDouble
    def *(y: Score): Score = x.toDouble * y.toDouble
    def /(y: Int): Score = x.toDouble / y

trait ConferenceReviewing:
  import ConferenceReviewing.*
  
  /**
   * @param article
   * @param scores
   * loads a review for the specified article, with complete scores as a map
   */
  def loadReview(article: Article, scores: Map[Question, Score]): Unit
  
  /**
   * @param article
   * @param relevance
   * @param significance
   * @param confidence
   * @param fin
   * loads a review for the specified article, with the 4 explicit scores
   */
  def loadReview(article: Article, relevance: Score, significance: Score, confidence: Score, fin: Score): Unit
  
  /**
   * @param article
   * @param question
   * @return the scores given to the specified article and specified question, as an (ascending-ordered) list
   */
  def orderedScores(article: Article, question: Question): List[Score]
  
  /**
   * @param article
   * @return the average score to question FINAL taken by the specified article
   */
  def averageFinalScore (article: Article): Score
  
  /**
   * An article is considered accept if its averageFinalScore (not weighted) is > 5,
   * and at least one RELEVANCE score that is >= 8.
   *
   * @return the set of accepted articles
   */
  def acceptedArticles(): Set[Article]
  
  /**
   * @return accepted articles as a list of pairs article+averageFinalScore, ordered from worst to best based on averageFinalScore
   */
  def sortedAcceptedArticles(): List[(Integer, Double)]
  
  /**
   * @return a map from articles to their average "weighted final score", namely,
   *         the average value of CONFIDENCE*FINAL/10
   *         Note: this method is optional in this exam
   */
  def averageWeightedFinalScoreMap(): Map[Integer, Double]

class ConferenceReviewingImpl extends ConferenceReviewing:
  ???

/**
 * Interfaccia ConferenceReviewing: modella i risultati del processo di revisione degli articoli di una conferenza.
 * Ogni articolo revisionato da 1 o + revisori anonimi, ognuno fornisce una valutazione (score) da 0-10 per 4 diverse "domande", modellate da Question.
 * Un articolo viene accettato se il valore medio della valutazione alla domanda "FINAL" è >5 e se ha almeno una valutazione "RELEVANCE" >= 8.
 * ConferenceReviewingImpl con costruttore senza argomenti.
 * credits: copiato spudoratamente da Giosuè
 */
@main def testConferenceReviewing(): Unit =
  import ConferenceReviewing.*
  import Question.*
  
  val cr: ConferenceReviewing = new ConferenceReviewingImpl()
  val article1 = article(1)
  val article2 = article(2)
  val article3 = article(3)
  val article4 = article(4)
  val article5 = article(5)
  
  def init(): Unit =
    // Ordine domande: relevance, significance, confidence, final
    cr.loadReview(article1, score(8), score(8), score(6), score(8))  // 4.8 -> voto finale pesato (usato da averageWeightedFinalScoreMap)
    cr.loadReview(article1, score(9), score(9), score(6), score(9))  // 5.4
    cr.loadReview(article2, score(9), score(9), score(10), score(9)) // 9.0
    cr.loadReview(article2, score(4), score(6), score(10), score(6)) // 6.0
    cr.loadReview(article3, score(3), score(3), score(3), score(3))  // 0.9
    cr.loadReview(article3, score(4), score(4), score(4), score(4))  // 1.6
    cr.loadReview(article4, score(6), score(6), score(6), score(6))  // 3.6
    cr.loadReview(article4, score(7), score(7), score(8), score(7))  // 5.6
    cr.loadReview(article5, score(6), score(6), score(6), score(10)) // 6.0
    cr.loadReview(article5, score(7), score(7), score(7), score(10)) // 7.0
    
    val map: Map[Question, Score] = Map(
      (RELEVANCE, score(8)),
      (SIGNIFICANCE, score(8)),
      (CONFIDENCE, score(7)),
      (FINAL, score(8))
    )
    cr.loadReview(article4, map)
  
  def testOrderedScores(): Unit =
    println(cr.orderedScores(article2, RELEVANCE))  // List(score(4), score(9))
    println(cr.orderedScores(article4, CONFIDENCE)) // List(score(6), score(7), score(8))
    println(cr.orderedScores(article5, FINAL))      // List(score(10), score(10))
  
  def testAverageFinalScore(): Unit =
    println(cr.averageFinalScore(article1)) // score(8.5)
    println(cr.averageFinalScore(article2)) // score(7.5)
    println(cr.averageFinalScore(article3)) // score(3.5)
    println(cr.averageFinalScore(article4)) // score(7.0)
    println(cr.averageFinalScore(article5)) // score(10.0)
  
  def testAcceptedArticles(): Unit =
    // solo gli articoli 1,2,4 vanno accettati, avendo media finale >=5 e almeno un voto su RELEVANCE >= 8
    println(cr.acceptedArticles) // Set(article1, article2, article4)
  
  def testSortedAcceptedArticles(): Unit =
    // articoli accettati, e loro voto finale medio
    println(cr.sortedAcceptedArticles) // List((article4, 7.0), (article2, 7.5), (article1, 8.5))
  
  def optionalTestAverageWeightedFinalScore(): Unit =
    // l'articolo 1 ha media pesata finale pari a (4.8+5.4)/2 = 5,1
    println(cr.averageWeightedFinalScoreMap.get(article1).contains((4.8 + 5.4) / 2))
    println(cr.averageWeightedFinalScoreMap.get(article2).contains((9.0 + 6.0) / 2))
    println(cr.averageWeightedFinalScoreMap.get(article3).contains((0.9 + 1.6) / 2))
    println(cr.averageWeightedFinalScoreMap.get(article4).contains((3.6 + 5.6 + 5.6) / 3))
    println(cr.averageWeightedFinalScoreMap.get(article5).contains((6.0 + 7.0) / 2))
    println(cr.averageWeightedFinalScoreMap.size) // 5
  
  println("------init------")
  init()
  println("------testOrderedScores------")
  testOrderedScores()
  println("------testAverageFinalScore------")
  testAverageFinalScore()
  println("------testAcceptedArticles------")
  testAcceptedArticles()
  println("------testSortedAcceptedArticles------")
  testSortedAcceptedArticles()
  println("------optionalTestAverageWeightedFinalScore------")
  optionalTestAverageWeightedFinalScore()
