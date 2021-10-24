object MyTypeur {
  println("Welcome to my typeur")
	import scala.collection.mutable.ListBuffer

	
	
	
	sealed trait Pterm
	case class Var( x : String) extends Pterm
	case class Abs( x : String, y:Pterm) extends Pterm
	case class App( x : Pterm, y:Pterm) extends Pterm
	
	
	sealed trait Ptype
	case class Vari( x : String) extends Ptype
	case class Arr( x : Ptype, y:Ptype)  extends Ptype

	
	case class equa( x : Ptype, y:Ptype)
	
	
	
	var env:Map[String,Ptype] = Map()
	
	var equas:ListBuffer[equa] = new ListBuffer[equa]()
	var gcpt = 0
	class varNotFoundException(s:String) extends Exception(s){}
	
	
	
	
	def print_term(t:Pterm):String = t match {
			case Var(x) => x
			case Abs(x,y) => "lambda "+x+". " + print_term(y)
			case App(x,y) => print_term(x)+" " + print_term(y)
	}


	def print_type(t:Ptype):String = t match {
			case Vari(x) => x
			case Arr(x,y) =>"(" + print_type(x)+" -> " + print_type(y) +")"
	}


	
	def newVar():String ={
		var p:Int = gcpt
		gcpt = gcpt+1
		
		return "T"+p
	}
	
	
	@throws(classOf[varNotFoundException])
	def findType(v:String):Ptype = {
		if(env.contains(v)){
			var x =env.get(v)
			return x.get
		}
		else{
			throw new varNotFoundException(v+" Not Found" )
		}
	}
	
	
	def isInType(v:String, t:Ptype):Boolean = t match{
		case Vari(x) if x == v => true
		case Arr(x,y) => isInType(v,x)||isInType(v,y)
		case _ => false
	}
	
	
	def substitueType (oldT:Ptype, v:String, newT:Ptype):Ptype= oldT match{
		case Vari(x) if x == v => newT
		case Vari(y) => oldT
		case Arr(x,y) => Arr(substitueType(x,v,newT),substitueType(y,v,newT))
	}
		
	def substitueTypeAll (v:String, newT:Ptype):ListBuffer[equa] = {
		 return equas.map( e => equa(substitueType(e.x,v,newT),substitueType(e.y,v,newT)))
	}



  def genere_equa (te : Pterm, ty : Ptype) : Unit = te match{
  
    case Var(v) => equas += (equa( ty, findType(v) ))
    case App(t1, t2) =>
      var s = newVar()
    	genere_equa(t1, Arr(Vari( s ), ty))
    	genere_equa(t2 , Vari( newVar() ) )

    case Abs(x, t) =>
			var s1 = newVar()
      var s2 = newVar()
      equas += equa(ty, Arr (Vari(s1), Vari(s2)))
      env +=  (x -> Vari(s1))
      genere_equa(t, Vari(s2))
   }
   
   
   
 
 print_term(Abs("x", App(Var("x"), Var("y") )))
 print_type(Arr(Vari("x"), Arr(Vari("x"), Vari("y") )))
  
  def main(args: Array[String]): Unit = {
    	print_term(Abs("x", App(Var("x"), Var("y") )))
  		print_type(Arr(Vari("x"), Arr(Vari("x"), Vari("y") )))
  }

}