object MyTypeur {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(51); 
  println("Welcome to my typeur")
	import scala.collection.mutable.ListBuffer

	
	
	
	sealed trait Pterm
	case class Var( x : String) extends Pterm
	case class Abs( x : String, y:Pterm) extends Pterm
	case class App( x : Pterm, y:Pterm) extends Pterm
	
	
	sealed trait Ptype
	case class Vari( x : String) extends Ptype
	case class Arr( x : Ptype, y:Ptype)  extends Ptype

	
	case class equa( x : Ptype, y:Ptype);$skip(419); 
	
	
	
	var env:Map[String,Ptype] = Map();System.out.println("""env  : Map[String,MyTypeur.Ptype] = """ + $show(env ));$skip(55); 
	
	var equas:ListBuffer[equa] = new ListBuffer[equa]();System.out.println("""equas  : scala.collection.mutable.ListBuffer[MyTypeur.equa] = """ + $show(equas ));$skip(14); 
	var gcpt = 0
	class varNotFoundException(s:String) extends Exception(s){};System.out.println("""gcpt  : Int = """ + $show(gcpt ));$skip(243); 
	
	
	
	
	def print_term(t:Pterm):String = t match {
			case Var(x) => x
			case Abs(x,y) => "lambda "+x+". " + print_term(y)
			case App(x,y) => print_term(x)+" " + print_term(y)
	};System.out.println("""print_term: (t: MyTypeur.Pterm)String""");$skip(137); 


	def print_type(t:Ptype):String = t match {
			case Vari(x) => x
			case Arr(x,y) =>"(" + print_type(x)+" -> " + print_type(y) +")"
	};System.out.println("""print_type: (t: MyTypeur.Ptype)String""");$skip(84); 


	
	def newVar():String ={
		var p:Int = gcpt
		gcpt = gcpt+1
		
		return "T"+p
	}
	
	
	@throws(classOf[varNotFoundException]);System.out.println("""newVar: ()String""");$skip(209); 
	def findType(v:String):Ptype = {
		if(env.contains(v)){
			var x =env.get(v)
			return x.get
		}
		else{
			throw new varNotFoundException(v+" Not Found" )
		}
	};System.out.println("""findType: (v: String)MyTypeur.Ptype""");$skip(158); 
	
	
	def isInType(v:String, t:Ptype):Boolean = t match{
		case Vari(x) if x == v => true
		case Arr(x,y) => isInType(v,x)||isInType(v,y)
		case _ => false
	};System.out.println("""isInType: (v: String, t: MyTypeur.Ptype)Boolean""");$skip(208); 
	
	
	def substitueType (oldT:Ptype, v:String, newT:Ptype):Ptype= oldT match{
		case Vari(x) if x == v => newT
		case Vari(y) => oldT
		case Arr(x,y) => Arr(substitueType(x,v,newT),substitueType(y,v,newT))
	};System.out.println("""substitueType: (oldT: MyTypeur.Ptype, v: String, newT: MyTypeur.Ptype)MyTypeur.Ptype""");$skip(157); 
		
	def substitueTypeAll (v:String, newT:Ptype):ListBuffer[equa] = {
		 return equas.map( e => equa(substitueType(e.x,v,newT),substitueType(e.y,v,newT)))
	};System.out.println("""substitueTypeAll: (v: String, newT: MyTypeur.Ptype)scala.collection.mutable.ListBuffer[MyTypeur.equa]""");$skip(435); 



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
   };System.out.println("""genere_equa: (te: MyTypeur.Pterm, ty: MyTypeur.Ptype)Unit""");$skip(62); val res$0 = 
   
   
   
 
 print_term(Abs("x", App(Var("x"), Var("y") )));System.out.println("""res0: String = """ + $show(res$0));$skip(56); val res$1 = 
 print_type(Arr(Vari("x"), Arr(Vari("x"), Vari("y") )));System.out.println("""res1: String = """ + $show(res$1));$skip(160); 
  
  def main(args: Array[String]): Unit = {
    	print_term(Abs("x", App(Var("x"), Var("y") )))
  		print_type(Arr(Vari("x"), Arr(Vari("x"), Vari("y") )))
  };System.out.println("""main: (args: Array[String])Unit""")}

}
