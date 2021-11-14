

object Tpr { 
  println("Welcome to my typeur")
	import scala.collection.mutable.ListBuffer

	
	
	// Pterm--------------------------------------------------------------------------------
	sealed trait Pterm
	case class Var( x : String) extends Pterm
	case class Abs( x : String, y:Pterm) extends Pterm
	case class App( x : Pterm, y:Pterm) extends Pterm
	case class N(i : Int) extends Pterm
	case class Add( x : Pterm, y:Pterm) extends Pterm
	case class Sub( x : Pterm, y:Pterm) extends Pterm
	case class Tete(x:Pterm) extends Pterm
	case class Queue(x:Pterm) extends Pterm
	case class LetIn( x : String, e1:Pterm, e2:Pterm) extends Pterm
	case class PF(x:Pterm, f:Pterm) extends Pterm
	case class IfZero(x:Pterm, y:Pterm, z:Pterm) extends Pterm
	case class IfEmpty(x:Pterm, y:Pterm, z:Pterm) extends Pterm
	case class Ref( x : Pterm) extends Pterm
	case class Deref( x : Pterm) extends Pterm
	case class Assign ( x : Pterm, y : Pterm) extends Pterm
	case class PUnit() extends Pterm
	case class Rho(x:Int) extends Pterm
    	//List--------------------------------------------------
      case object EmptyL extends Pterm
      case object NilL extends Pterm
      case class  ConsL(h:Pterm, t:Pterm) extends Pterm{
          override def toString() : String = {
                  
                return print_term(h)+"," + t.toString()
          }
      }
      
      //------------------------------------------------------
  //--------------------------------------------------------------------------------------
	
	
	
	
  
  // Ptype--------------------------------------------------------------------------------

	sealed trait Ptype
	case class Vari( x : String) extends Ptype
	case class Arr( x : Ptype, y:Ptype)  extends Ptype
	case object Nat extends Ptype
	case class Lst(e:Ptype) extends Ptype
	case object EmptyT extends Ptype
	case object Unit extends Ptype
	case class RefT( x : Ptype) extends Ptype
	//--------------------------------------------------------------------------------------
	
	
	
	
	
	
	
	
	
	//Global Declarations--------------------------------------------------------------------
	case class equa( x : Ptype, y:Ptype)

	
	
	var env:Map[String,Ptype] = Map()
	var remp:Map[String,String] = Map()
	var equas:ListBuffer[equa] = new ListBuffer[equa]()
	var gcpt = 0
	var vcpt = 0
  var tab:Map[Int,Pterm] = Map()
	var tabcpt = 0
	
	class varNotFoundException(s:String) extends Exception(s){}
  class unificationException(s:String) extends Exception(s){}
  class evaluationException(s:String) extends Exception(s){}
	
  //--------------------------------------------------------------------------------------

	

	//Prints--------------------------------------------------------------------------------
	def print_term(t:Pterm):String = t match {
			case Var(x) => x
			case Abs(x,y) => "lambda "+x+". " + print_term(y)
			case App(x,y) => print_term(x)+" " + print_term(y)
			case N(x) => x.toString()
			case Sub(x,y) => "("+print_term(x)+" - " + print_term(y)+")"
			case Add(x,y) => "("+print_term(x)+" + " + print_term(y)+")"
			case NilL => "Nil"
			case x@ConsL(a,b) => "["+x.toString()+"]"
			case EmptyL => "[]"
			case Tete(x) => "Tete("+print_term(x)+")"
			case Queue(x) => "Queue("+print_term(x)+")"
			case IfZero(x,y,z) => "(if(" + print_term(x)+" == 0) then: (" + print_term(y) + ")  else: (" + print_term(z) +"))"
			case IfEmpty(x,y,z) => "(if(" + print_term(x)+" == []) then: (" + print_term(y) + ")  else: (" + print_term(z) +"))"
			case PF(x,f) => "(def x=f(x), x: "+print_term(x)+", f: " + print_term(f) +")"
			case LetIn(x,e1,e2) => "(let "+x+"= "+print_term(e1)+" in " + print_term(e2) +")"
			case Ref(x) => "(ref "+(print_term(x))+")"
			case Deref(x) => "!("+print_term(x)+")"
			case Assign(x, y) => print_term(x)+" := "+print_term(y)
			case PUnit() => "()"
	}


	def print_type(t:Ptype):String = t match {
			case Vari(x) => x
			case Arr(x,y) =>"(" + print_type(x)+" -> " + print_type(y) +")"
			case Nat => "Nat"
			case Lst(a) => "["+print_type(a)+"]"
			case EmptyT => "[]"
			case RefT(x) => "( RefT("+ print_type(x) + "))"
			case Unit => "Unit"
		
	}

	//Prints--------------------------------------------------------------------------------

	
	
	
	
	
	
	
	//--------------------------------------------------------------------------------
	//|||||||||||||||||||||||||||||||| SEMANTIQUE ||||||||||||||||||||||||||||||||||||
	//--------------------------------------------------------------------------------
	
	
	def freshVar():String = {
	  var p:Int = vcpt
		vcpt = vcpt+1
		
		return "v"+p
	}
	
	def barendregt(l : Pterm) : Pterm = l match {
	    case Var(x) => if(remp.contains(x)) Var(remp.get(x).get) else Var(x)
			case Abs(x,y) => 
			  var s = freshVar()
			  remp += (x -> s)
			  Abs(s, barendregt(y)) 
			case App(x,y) => App(barendregt(x), barendregt(y))
			case N(x) => N(x)
			case Sub(x,y) => Sub(barendregt(x), barendregt(y))
			case Add(x,y) => Add(barendregt(x), barendregt(y))
			case NilL => NilL
			case ConsL(a,b) => ConsL(barendregt(a), barendregt(b) )
			case EmptyL => EmptyL
			case Tete(x) => Tete(barendregt(x))
			case Queue(x) => Queue(barendregt(x))
			case IfZero(x,y,z) => IfZero(barendregt(x), barendregt(y), barendregt(z) )
			case IfEmpty(x,y,z) => IfEmpty(barendregt(x), barendregt(y), barendregt(z) )
			case PF(x,f) => PF(barendregt(x),barendregt(f))
			case LetIn(x,e1,e2) => 
			  //if(remp.contains(x)) throw new evaluationException("Var Already Exists")
			  remp += (x ->x)
			  LetIn(x,barendregt(e1),barendregt(e2))
			case Ref(x) => Ref(barendregt(x))
			case Deref(x) => Deref(barendregt(x))
			case Assign(x, y) => Assign(barendregt(x), barendregt(y))
			case PUnit() => PUnit()
	}
	
	
	 def instantie(l:Pterm , vn:String , a:Pterm) : Pterm = l match {
	   	case Var(x) => if(x == vn) a else Var(x)
			case Abs(x,y) => Abs(x, instantie(y, vn, a)) 
			case App(x,y) => App(instantie(x, vn, a), instantie(y, vn, a))
			case N(x) => N(x)
			case Sub(x,y) => Sub(instantie(x, vn, a), instantie(y, vn, a))
			case Add(x,y) => Add(instantie(x, vn, a), instantie(y, vn, a))
			case NilL => NilL
			case ConsL(a,b) => ConsL(instantie(a, vn, a), instantie(b, vn, a) )
			case EmptyL => EmptyL
			case Tete(x) => Tete(instantie(x, vn, a))
			case Queue(x) => Queue(instantie(x, vn, a))
			case IfZero(x,y,z) => IfZero(instantie(x, vn, a), instantie(y, vn, a), instantie(z, vn, a) )
			case IfEmpty(x,y,z) => IfEmpty(instantie(x, vn, a), instantie(y, vn, a), instantie(z, vn, a) )
			case PF(x,f) => PF(instantie(x, vn, a),instantie(f, vn, a))
			case LetIn(x,e1,e2) => LetIn(x,instantie(e1, vn, a),instantie(e2, vn, a))
			case Ref(x) => Ref(instantie(x, vn, a))
			case Deref(x) => Deref(instantie(x, vn, a))
			case Assign(x, y) => Assign(instantie(x, vn, a), instantie(y, vn, a))
			case PUnit() => PUnit()
	 }
	 
	 
	 // make the pattern matchings stricter by only considering the Var(x) and throwing errors otherwise 
	 def red(l: Pterm) : Pterm = {
	    var ll = barendregt(l)
	    
	    
	    return ll match{
  	   	case Var(x) => Var(x)
  			case Abs(x,y) => Abs(x,red(y))
  			
  			case App(x,y) => 
  			  var af = App(red(x),red(y))
  			  af match{
  			    case App(Abs(v, ap),t) => red(instantie(ap,v,t))
  			    case _ => af
  			  }
  			case N(x) => N(x)
  			case Sub(x,y) => 
  			  var af = Sub(red(x),red(y))
  			  af match{
  			    case Sub(N(e1),N(e2)) => N(e1-e2)
  			    case _ => af
  			  }
  			case Add(x,y) => 
  			var af = Add(red(x),red(y))
  			  af match{
  			    case Add(N(e1),N(e2)) => N(e1+e2)
  			    case _ => af
  			  }
  			case NilL => NilL
  			case ConsL(a,b) => 
  			  if(!listCheck(ConsL(a,b))) throw new evaluationException("List Construcor Error")
  			  ConsL(red(a),red(b))
  			case EmptyL => EmptyL
  			case Tete(x) => 
  			  var af = red(x)
  			  af match {
  			    case EmptyL => throw new evaluationException("Empty List Has No Head")
  			    case NilL => throw new evaluationException("Nil Has No Head")
  			    case ConsL(a,b) => a
  			    case _ => Tete(af)
  			  }
  			case Queue(x) => 
          var af = red(x)
  			  af match {
  			    case EmptyL => throw new evaluationException("Empty List Has No Tail")
  			    case NilL => throw new evaluationException("Nil Has No Tail")
  			    case ConsL(a,b) => b
  			    case _ => Queue(af)
  			  }
  			case IfZero(x,y,z) => 
  			  var af = IfZero(red(x), red(y), red(z))
  			  af match {
  			    case IfZero(N(xx),yy,zz) => if(xx==0) yy else zz
  			    case _ => af
  			  }
  			  
  			case IfEmpty(x,y,z) => 
          var af = IfEmpty(red(x), red(y), red(z))
  			  af match {
  			    case IfEmpty(NilL,yy,zz) => yy
  			    case IfEmpty(EmptyL,yy,zz) => yy
  			    case IfEmpty(ConsL(a,b),yy,zz) => zz
  			    case _ => af
  			  }
  			case PF(x,f) => NilL
  			case LetIn(x,e1,e2) => 
  			  var pf = LetIn(x,red(e1),red(e2))
  			  red(instantie(e2,x,e1))
  			  
  			case Ref(x) =>
  			  tabcpt = tabcpt + 1
  			  tab = tab + (tabcpt->red(x))
  			  Rho(tabcpt)
  			  
  			case Deref(x) =>
  			  var af = red(x)
  			  af match{
  			    case Rho(i) => tab(i)
  			    case _ => Deref(af)
  			  }
  			  
  			case Assign(x,y) =>
  			  var af = Assign(red(x), red(y))
  			  af match{
  			    case Assign(Rho(i), yy) => 
  			      tab = tab + (i->yy)
  			      PUnit()
  			    case _ => af
  			  }
	    }
	 }
	
	def listCheck(l:Pterm) : Boolean = l match {
	  case NilL => true
	  case EmptyL => true
	  case ConsL(a,b) => listCheck(b)
	  case _ => false
	}
	
	def print_sem(l : Pterm) : String ={
	  return "Term: "+ print_term(l)+"\nRedu: "+print_term(red(l))
	}
	//--------------------------------------------------------------------------------
	//||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
	//--------------------------------------------------------------------------------
	
	
	
	
	
	
	
	
	
	//--------------------------------------------------------------------------------
	//|||||||||||||||||||||||||||||||||| TYPEUR ||||||||||||||||||||||||||||||||||||||
	//--------------------------------------------------------------------------------
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
		case Lst(a) => isInType(v,a)
		case _ => false
	}
	
	
	def substitueType (oldT:Ptype, v:String, newT:Ptype):Ptype= oldT match{
		case Vari(x) if x == v => newT
		case Vari(y) => oldT
		case Arr(x,y) => Arr(substitueType(x,v,newT),substitueType(y,v,newT))
		case Nat => Nat
		case Lst(a) => Lst(substitueType(a,v,newT))
		case EmptyT=>EmptyT
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
      
    case ConsL(h,t) =>
      var s = newVar()
      genere_equa (h,Vari(s))
      genere_equa (t,Lst(Vari(s)))
      equas += (equa( ty, Lst(Vari(s)) ))
      
    case NilL => 
    
    case EmptyL =>
      
      equas += (equa( ty, EmptyT ))
      
      
    case Tete(x) =>
      var s = newVar()
      genere_equa (x,Lst(Vari(s)))
      equas += (equa( ty, Vari(s) ))
      
    case Queue(x) =>
      var s = newVar()
      genere_equa (x,Lst(Vari(s)))
      equas += (equa( ty, Lst(Vari(s)) ))
          
    case IfZero(x,y,z) =>
      var s = newVar()
      genere_equa (x,Nat)
      genere_equa (y,Vari(s))
      genere_equa (z,Vari(s))
      equas += (equa( ty, Vari(s) ))
      
    case IfEmpty(x,y,z) =>
      var s1 = newVar()
      var s2 = newVar()
      genere_equa (x,Lst(Vari(s1)) )
      genere_equa (y,Vari(s2))
      genere_equa (z,Vari(s2))
      equas += (equa( ty, Vari(s2) ))
  
    case PF(x,f) =>
      var s = newVar()
      genere_equa (x,Vari(s))
      genere_equa (f,Arr(Vari(s), Vari(s)))
      equas += (equa( ty, Vari(s) ))
      
    case LetIn(x,e1,e2) =>
      var s1 = newVar()
      var s2 = newVar()
      env +=  (x -> Vari(s1))
      genere_equa (e1,Vari(s1)) 
      genere_equa (e2,Vari(s2))
      equas += (equa( ty, Vari(s2) ))
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
          
       case equa(Lst(a), Lst(b)) => 
         var eq = equa(a,b)
         var equ:ListBuffer[equa] = new ListBuffer[equa]()
         equ+=eq
         equas = equas.slice(0, idx) ++ equ ++ equas.slice(idx+1, equas.size)
         unification(but, idx)
       case equa(EmptyT, EmptyT) =>
         equas = equas.slice(0, idx)  ++ equas.slice(idx+1, equas.size)
         unification(but, idx)
         
       case equa(_, EmptyT) | equa(EmptyT, _) => throw new unificationException("Empty List Error")
         
       
       
     }
     
     
  } 
  
  def inference(t:Pterm) : String = {
    var but = "but"
    genere_equa(t, Vari(but))
    var pt:Ptype  = null
    
    
    pt = unification(but, 0)
   
    
    return "Term: "+ print_term(t)+"\nType: " +print_type(pt)+"\nRedu: " + print_term(red(t)) +"\n"
    
  }
  
  
  //--------------------------------------------------------------------------------
	//||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
	//--------------------------------------------------------------------------------
  
  def main(args: Array[String]): Unit = {
    	//println(print_term(Abs("x", App(Var("x"), Var("y") ))))
  		//println(print_type(Arr(Vari("x"), Arr(Vari("x"), Vari("y") )     )))
      //println( inference(     Abs("x",   App(Var("x"), Var("y") )) )   )
    
      
    
      println( inference(     App (Abs ("x", Add(Var("x"), N(1))), N(3))           ))
      println( inference(     Abs ("x", Add(Var("x"), N(1)))))
      println( inference(     Abs ("x", Sub(Var("x"), N(2)))))
      println( inference(     Abs ("x",  ConsL(N(5) , NilL )  )))
      println( inference(     Abs ("x",  ConsL(Var("x") , NilL )  )))
      println( inference(     Abs ("x",  NilL   )))
      
      
      println( inference(     Abs ("x",  ConsL(ConsL(N(5),NilL) , NilL )  )))
      println( inference(     Abs ("x",  ConsL(N(5) , ConsL(N(6),NilL) )  )))
      println( inference(     Abs ("x",  ConsL(Abs("y", Var("y") ), NilL )         )))
      
      println( inference(     Abs ("x",  Tete(ConsL(N(3), NilL ))         )))
      println( inference(     Abs ("x",  Queue(ConsL(N(3), NilL ))         )))
      println( inference(     Abs ("x",  Tete(Var("x"))         )))
      println( inference(     Abs ("x",  Queue(Var("x"))         )))
      println( inference(     Abs ("x",  ConsL(N(5) , ConsL( N(5) ,NilL) )  )))
      
      println( inference(     Abs ("x", Abs("y", Abs("z", IfZero(Var("x"), Var("y"),Abs("a",Var("a") ) ) ) ))    ))
      println( inference(     Abs ("x", Abs("y", Abs("z", IfEmpty(Var("x"), Var("y"),N(3)  ) ) ))    ))
      println( inference(     Abs ("x", Abs("y", Abs("z", IfEmpty(Var("x"), Var("y"),Var("z")  ) ) ))    ))

      println( inference(     Abs ("x",Abs("y",  ConsL(N(5) , ConsL(Var("x"), ConsL(App(Var("y"), Var("x")),  NilL) )  )))))
      
      println( inference(     Abs ("x",Abs("y",   PF(Var("x"),Abs("z",Add(N(1), Var("z") )))  ))))
      
      println( inference(     Abs ("x",   LetIn("aa", N(1), Add(Var("x"), Var("aa")      ))     )))
      println( inference(     Abs ("x",   LetIn("bb", N(1), LetIn("cc", N(2), Add(Var("bb"), Var("cc")))   )     )))
      println( inference(  App(   Abs ("x",   LetIn("ee", N(1), LetIn("dd", N(2), Add(Var("dd"), Var("x")))   )     ), N(3))))

      println( inference(     Abs ("x",  ConsL(N(5) , ConsL( N(5) ,NilL) )  )))
      println( inference(     Abs ("x", EmptyL )))
      
      
      println(inference( App(Abs ("x", Add(Var("x"), N(32))), N(3))   ))
      println(inference( App(Abs ("x", Tete(Var("x"))), ConsL(N(3), ConsL(N(4), NilL))    )  ))
      println(inference(App(Abs ("x", Tete(Queue(Var("x")))), ConsL(N(3), ConsL(N(4), NilL))    )  ))
      println(inference( App(Abs("x", Add(Var("x"), N(44))),  N(2)  )  ))
      println(inference( App(Abs ("x", IfZero(Var("x"), N(4), N(99) )), N(0))   ))
      println(inference( App(Abs ("x", IfZero(Var("x"), N(4), N(99) )), N(1))   ))
      
      println(inference( LetIn("a", Ref(N(1)), Add(Deref(Var("a")), Deref(Var("a"))))))
      
      
      
      //println( inference(     Abs ("x",  NilL )))
      
      // check with others: here i am just infering the type recursively, the other method was 
      //to look at the first element and then tell the type with locking the constructor  
      
      // what type is PF
  
  }
/*
	sealed trait L[+A <: Pterm] extends Pterm
  case class  ConsL[A<: Pterm] (h:A, t:L[Pterm]) extends L[A]{
      override def toString() : String = {
              
            return print_term(h)+"," + t.toString()
      }
  }
  case object EmptyL extends L[Pterm]{
     override def toString() : String = {
              
            return "[]"
      }
  }
  case object NilL extends L[Pterm]{
     override def toString() : String = {
              
            return "Nil"
      }
  }
  */
 	/*
	sealed trait TL[+A <: Ptype] extends Ptype
	case class Lst[+A<: Ptype] (e:A) extends TL[A]
	case object EmptyT extends TL[Nothing]
	*/
	
}