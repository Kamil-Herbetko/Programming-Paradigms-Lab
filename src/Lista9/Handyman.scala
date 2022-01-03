package Lista9

import Lista9.Electrician.count

class EmptyName extends Exception
class EmptySurname extends Exception
class AgeTooSmall extends Exception

class Handyman(val name: String, val surname: String, private var ag: Short):
  require(name != "", throw new EmptyName)
  require(surname != "", throw new EmptySurname)
  require(age >= 18, throw new AgeTooSmall)

  def age: Short = ag
  def age_=(newAge: Short) =
    require(newAge >= 18, throw new AgeTooSmall)
    ag = newAge


trait Painter:
  Painter.count += 1
  def paint() = println("Painting...")

object Painter:
  private var count = 0
  def getNumberOfPainters() = count

trait Electrician:
  Electrician.count += 1
  def fixElectricity() = println("Fixing electricity...")

object Electrician:
  private var count = 0
  def getNumberOfElectricians() = count


trait Plumber:
  Plumber.count += 1
  def plumbing() = println("Plumbing...")

object Plumber:
  private var count = 0
  def getNumberOfPlumbers() = count

object app extends App:
  var handyman = new Handyman("Jan", "Kowalski", 25) with Electrician
  handyman.fixElectricity()
  var handyman2 = new Handyman("Piotr", "Nowak", 28) with Electrician with Plumber
  handyman2.fixElectricity()
  handyman2.plumbing()

  var handyman4 = new Handyman("Kamil", "Nowak", 19) with Painter with Electrician with Plumber
  handyman4.paint()

  //var handyman3 = new Handyman("", "", 8) with Plumber
  //var handyman3 = new Handyman("Jan", "", 8) with Plumber
  //var handyman3 = new Handyman("Jan", "Kowalski", 8) with Plumber

  println("Electricians: ")
  println(Electrician.getNumberOfElectricians())
  println("Plumbers: ")
  println(Plumber.getNumberOfPlumbers())
  println("Painters: ")
  println(Painter.getNumberOfPainters())
