package chapter04

/**
  * Created by lexun on 6/29/2016.
  */
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

sealed trait Option[+A] {

  // exercise 4.1
  def map[B](f: A => B): Option[B] = this match {
    case Some(x) => Some(f(x))
    case None => None
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case Some(x) => f(x)
    case None => None
  }

  def flatMap_1[B](f: A => Option[B]): Option[B] =
    map(f) getOrElse None

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(x) => x
    case None => default
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case Some(x) => this
    case None => ob
  }

  def orElse_1[B >: A](ob: => Option[B]): Option[B] =
    this.map(Some(_)) getOrElse ob

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(x) => if (f(x)) this else None
    case None => None
  }

  def filter_1(f: A => Boolean): Option[A] =
    flatMap(x => if(f(x)) Some(x) else None)

}

