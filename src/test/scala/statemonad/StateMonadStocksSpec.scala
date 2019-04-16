package statemonad

import com.typesafe.scalalogging.LazyLogging
import org.scalatest.{FlatSpec, Matchers}

class StateMonadStocksSpec extends FlatSpec with Matchers with LazyLogging {

  "a state monad" should "handle stock trading" in {

    import StocksApp._

    val portfolio: Map[String, Int] = Map("AMZN" -> 1600, "GOOG" -> 1000, "TSLA" -> 300)

    {
      val txn1 = buy("GOOG",1037.0) // buy one share of GOOG
      val ((unusedFunds, sharesPurchased), portfolioAfter) = txn1(portfolio)
      unusedFunds should be (0.95)
      sharesPurchased should be (1)
      (get("GOOG")(portfolioAfter)._1 - get("GOOG")(portfolio)._1) should be (1)

      val txn2 = sell("GOOG",1) // sell one share of GOOG
      val (revenue, portfolioAfter2) = txn2(portfolioAfter)
      revenue should be (1036.05)
      (get("GOOG")(portfolioAfter2)._1 - get("GOOG")(portfolio)._1) should be (0)

    }
    {
      // move all GOOG shares to TSLA
      val ((unusedFunds, sharesSold, sharesBought), portfolioAfter) = move("GOOG", "TSLA")(portfolio)
      unusedFunds should be(126.0)
      sharesSold should be(1000)
      sharesBought should be(2994)
    }

    {
      // should also be possible with moveFunc
      val ((unusedFunds1, sharesSold, sharesBought), portfolioAfter) = moveFunc("GOOG", "TSLA")(portfolio)
      unusedFunds1 should be (126.0)
      sharesSold should be (1000)
      sharesBought should be (2994)
      // logger.info(s"t is ${t}")
    }

    {
      // should also be possible with moveImper
      val sm = moveImper("GOOG", "TSLA")
      val (unusedFunds, sharesSold, sharesBought) = sm.eval(portfolio)
      unusedFunds should be (126.0)
      sharesSold should be (1000)
      sharesBought should be (2994)
      // logger.info(s"t is ${t}")
    }



  }

}

object StocksApp {

  val prices: Map[String, BigDecimal] = Map("AMZN" -> 1631.17, "GOOG" -> 1036.05, "TSLA" -> 346.00)

  // Definition of the State Monad
  type State[S, A] = S => (A, S)
  def unit[S, A](a: A): State[S, A] = state => {
    (a, state)
  }
  def map[S, A, B](sm: State[S, A])(f: A => B): State[S, B] = state => {
    val (value, newState) = sm(state)
    (f(value), newState)
  }
  def flatMap[S, A, B](sm: State[S, A])(f: A => State[S, B]): State[S, B] = state => {
    val (value, newState) = sm(state)
    f(value)(newState)
  }
  // End of definition of the State Monad

  /**
    * A stocks portfolio, which associate a stock name to the quantity owned
    */
  type Stocks = Map[String, Int]
  type Transaction[A] = State[Stocks, A]

  /**
    * Returns the quantity of stocks owned for `name`.
    * @param name Name of the stock
    * @return The quantity of stocks owned for `name`.
    */
  def get(name: String): Transaction[Int] = portfolio => {
    (portfolio(name), portfolio)
  }

  /**
    * Buys an amount (dollars) of the stock with given `name`. Returns the number
    * of purchased stocks.
    *
    * @param name The name of the stocks to buy
    * @param amount The amount in dollars to buy
    * @return The quantity of stocks purchased
    */
  def buy(name: String, amount: BigDecimal): Transaction[(BigDecimal,Int)] = portfolio => {
    val purchased = (amount / prices(name)).toInt
    val unused = amount - purchased*prices(name)
    val owned = portfolio(name)
    ((unused, purchased), portfolio + (name -> (owned + purchased)))
  }

  /**
    * Sells a `quantity` of stocks of the given `name`. Returns the amount of
    * dollars earned by the selling operation.
    *
    * @param name The name of the stocks to sell
    * @param quantity The quantity of stocks to sell
    * @return The earned amount
    */
  def sell(name: String, quantity: Int): Transaction[BigDecimal] = portfolio => {
    val revenue = quantity * prices(name)
    val owned = portfolio(name)
    (revenue, portfolio + (name -> (owned - quantity)))
  }

  /**
    * Sells all stocks called `from`, and with the revenue buys stocks called `to`.
    * Returns the quantity of stock sold and the quantity of stocks purchased.
    *
    * @param from Stocks to be sold
    * @param to Stocks to be purchased
    * @return The quantity of stock sold and the quantity of stocks purchased
    */
  def move(from: String, to: String): Transaction[(BigDecimal, Int, Int)] = portfolio => {
    val (originallyOwned, _) = get(from)(portfolio)
    val (revenue, newPortfolio) = sell(from, originallyOwned)(portfolio)
    val ((unused, purchased), veryNewPortfolio) = buy(to, revenue)(newPortfolio)
    ((unused, originallyOwned, purchased), veryNewPortfolio)
  }

  /**
    * Uses the flatMap and the map functions to implement the same use case of the `move` function,
    * but without passing the updated portfolio explicitly.
    */
  def moveFunc(from: String, to: String): Transaction[(BigDecimal,Int, Int)] =
    flatMap(get(from))(
      originallyOwned => flatMap(sell(from, originallyOwned))(
        revenue => map(buy(to, revenue))(
          purchased => (purchased._1, originallyOwned, purchased._2)
        )
      )
    )

  // StateMonad as trait
  object StateMonad {
    def unit[S,A](a: A): StateMonad[S, A] = StateMonad(s => (a, s))
  }

  case class StateMonad[S,A](f:S => (A,S)) {

    def map[B](g: A => B): StateMonad[S, B] =
      StateMonad[S,B](s => {
        val (value, s1) = f(s)
        (g(value), s1)
      })

    def flatMap[B](g: A => StateMonad[S, B]): StateMonad[S, B] =
      StateMonad[S,B](s => {
        val (value, s1) = f(s)
        g(value)f(s1)
      })

    def withFilter(p:S => Boolean):StateMonad[S,A] = {
      StateMonad[S,A](f) // todo apply p(s)
    }
    def eval(s: S): A =
      f(s)._1
  }

  type MonadicTransaction[A] = StateMonad[Stocks, A]

  def getImper(name: String): MonadicTransaction[Int] =  {
    StateMonad[Stocks, Int](portfolio => (portfolio(name), portfolio))
  }

  def buyImper(name: String, amount: BigDecimal): MonadicTransaction[(BigDecimal,Int)] =  {
    StateMonad[Stocks, (BigDecimal, Int)](portfolio => {
      val purchased = (amount / prices(name)).toInt
      val unused = amount - purchased*prices(name)
      val owned = portfolio(name)
      ((unused, purchased), portfolio + (name -> (owned + purchased)))
    })
  }

  def sellImper(name: String, quantity: Int): MonadicTransaction[BigDecimal] = {
    StateMonad[Stocks, BigDecimal](portfolio => {
      val revenue = quantity * prices(name)
      val owned = portfolio(name)
      (revenue, portfolio + (name -> (owned - quantity)))
    })
  }

  /**
    * Rewrites the `moveFunc` function, using the `for-yield` syntactic sugar.
    */
  def moveImper(from: String, to: String): MonadicTransaction[(BigDecimal, Int, Int)] = {
    val sm:StateMonad[Stocks, (BigDecimal, Int, Int)] = for {
      originallyOwned <- getImper(from)
      revenue <- sellImper(from, originallyOwned)
      //(unused:BigDecimal, purchased:Int) <- buyImper(to, revenue)
    } yield {
      // (unused,originallyOwned, purchased)
      (0,originallyOwned, 0)
    }
    sm
  }
}