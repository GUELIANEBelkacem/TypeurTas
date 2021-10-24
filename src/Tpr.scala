

object Tpr { 
  println("Welcome to my typeur")
	import scala.collection.mutable.ListBuffer

	
	
	
	sealed trait Pterm
	case class Var( x : String) extends Pterm
	case class Abs( x : String, y:Pterm) extends Pterm
	case class App( x : Pterm, y:Pterm) extends Pterm
	case class N(i : Int) extends Pterm
	case class Add( x : Pterm, y:Pterm) extends Pterm
	case class Sub( x : Pterm, y:Pterm) extends Pterm
	
	sealed trait Ptype
	case class Vari( x : String) extends Ptype
	case class Arr( x : Ptype, y:Ptype)  extends Ptype
	case object Nat extends Ptype

	
	case class equa( x : Ptype, y:Ptype)
	
	
	
	var env:Map[String,Ptype] = Map()
	
	var equas:ListBuffer[equa] = new ListBuffer[equa]()
	var gcpt = 0
	
	class varNotFoundException(s:String) extends Exception(s){}
  class unificationException(s:String) extends Exception(s){}
	
	
	
	
	def print_term(t:Pterm):String = t match {
			case Var(x) => x
			case Abs(x,y) => "lambda "+x+". " + print_term(y)
			case App(x,y) => print_term(x)+" " + print_term(y)
			case N(x) => x.toString()
			case Sub(x,y) => "("+print_term(x)+" - " + print_term(y)+")"
			case Add(x,y) => "("+print_term(x)+" + " + print_term(y)+")"
	}


	def print_type(t:Ptype):String = t match {
			case Vari(x) => x
			case Arr(x,y) =>"(" + print_type(x)+" -> " + print_type(y) +")"
			case Nat => "Nat"
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
		case Nat => Nat
	}
		
	
	def substitueTypeAll (v:String, newT:Ptype):Unit = {
		 equas =  equas.map( e => equa(substitueType(e.x,v,newT),substitueType(e.y,v,newT)))
	}



  def genere_equa (te : Pterm, ty : Ptype) : Unit = te match{
  
    case Var(v) => equas += (equa( ty, findType(v) ))
    case App(t1, t2) =>
      var s = newVar()
    	genere_equa(t1, Arr(Vari( s ), ty))
    	genere_equa(t2 , Vari( s ) )

    case Abs(x, t) =>
			var s1 = newVar()
      var s2 = newVar()
      equas += equa(ty, Arr (Vari(s1), Vari(s2)))
      env +=  (x -> Vari(s1))
      genere_equa(t, Vari(s2))
      
    case N(x) => equas += (equa( ty, Nat ))
    case Add(x,y) => 
      genere_equa (x, Nat)
      genere_equa (y, Nat)
      equas += (equa( ty, Nat ))
      
    case Sub(x,y) => 
      genere_equa (x, Nat)
      genere_equa (y, Nat)
      equas += (equa( ty, Nat ))
      
   }
  
  
  def trouve_but(but:String):Ptype = {
    var target:Ptype = null
    equas.foreach(
       e =>
         e match{
           case equa(Vari(v), t) if v == but => target = t 
           case equa(t, Vari(v)) if v == but => target = t
         }
    )
    if(target == null) throw new unificationException("Couldn't Find But")
    return target
  }
   
   
   
  @throws(classOf[unificationException])
  def unification( but:String, idx:Int ) : Ptype ={
     if(idx >= equas.size){
       var ret:Ptype = null
       try{
         ret = trouve_but(but)
       }catch{
         case _ : Throwable=> throw new unificationException("But Was Not Found")
       }
       return ret
     }
     
     var eq:equa = equas.apply(idx)
     
     return eq match{
       case equa(Vari(v1), t2) if v1 == but => unification(but, idx+1)
       case equa(Vari(v1), Vari(v2)) => 
         substitueTypeAll(v2, Vari(v1))
         equas = equas.slice(0, idx)  ++ equas.slice(idx+1, equas.size)
         unification(but, 0)
         
       case equa(Vari(v), t) => 
         if(isInType(v, t)) throw new unificationException("In Type Problem")
         else{
           substitueTypeAll(v,t)
           equas = equas.slice(0, idx)  ++ equas.slice(idx+1, equas.size)
           unification(but, 0)
         }
         
       case equa(t, Vari(v)) => 
         if(isInType(v, t)) throw new unificationException("In Type Problem")
         else{
           substitueTypeAll(v,t)
           equas = equas.slice(0, idx)  ++ equas.slice(idx+1, equas.size)
           unification(but, 0)
         }
         
         
       case equa(Arr(t1, t2), Arr(t3,t4)) =>  
         var eq1 = equa(t1,t3)
         var eq2 = equa(t2,t4)
         var equ:ListBuffer[equa] = new ListBuffer[equa]()
         equ+=eq1
         equ+=eq2
         equas = equas.slice(0, idx) ++ equ ++ equas.slice(idx+1, equas.size)
         unification(but, idx)
         
       case equa(Arr(_, _), _) | equa(_, Arr(_, _))=> throw new unificationException("Arr to Var matching")
         
       case equa(Nat, Nat) =>  
         equas = equas.slice(0, idx)  ++ equas.slice(idx+1, equas.size)
         unification(but, idx)
     
       case equa(Nat, _) | equa(_, Nat)  =>  throw new unificationException("Nat Type Error ")
          
      
     }
     
     
  } 
  
  def inference(t:Pterm) : String = {
    var but = "but"
    genere_equa(t, Vari(but))
    var pt:Ptype  = null
    
    
    pt = unification(but, 0)
   
    
    return "Term: "+ print_term(t)+"\nType: " +print_type(pt)+"\n"
    
  }
  
  def main(args: Array[String]): Unit = {
    	//println(print_term(Abs("x", App(Var("x"), Var("y") ))))
  		//println(print_type(Arr(Vari("x"), Arr(Vari("x"), Vari("y") )     )))
      //println( inference(     Abs("x",   App(Var("x"), Var("y") )) )   )
      println( inference(     App (Abs ("x", Add(Var("x"), N(1))), N(3))           ))
      println( inference(     Abs ("x", Add(Var("x"), N(1)))))
  }

}