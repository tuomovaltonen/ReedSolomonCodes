
import scala.util.Random



//Helper object for creating random message that can be encoded to a codeword of RS[n,k]_q code
object RandMessage {
    def apply(n: Int,k: Int,field: FiniteField, alphaIn: Option[Vector[Int]] = None): RSmessage = {
        val message = Vector.fill[Int](k)(Random.nextInt(field.q))
        new RSmessage(n,field,message,alphaIn)
    }
}

// Evaluations points alpha are primitive powers from 0 until n by default
// messageIntegers and evaluation point integers alphaIn are interpreted as primitive powers + 1 of the field elements
case class RSmessage(n: Int, field: FiniteField, messageIntegers: Vector[Int], alphaIn: Option[Vector[Int]] = None){
    require(n>messageIntegers.length)
    val k = messageIntegers.length
    val q = field.q
    val message = messageIntegers.map(x => field.elements(x))
  
    var alpha: Vector[field.FieldElement] = field.elements.take(n).toVector
    if(alphaIn != None ){alpha = alphaIn.get.map(x => field.elements(x))}

    def generatorMatrix: Vector[Vector[field.FieldElement]] = (0 until k).toVector.map(x => alpha.map(y => y^x))
  
    def hammingDistance(v: RSmessage) =  this.message.zip(v.message).count{case(a,b) => !(a == b)}
    def johnsonBound:Int = { val bound = n-math.sqrt((k-1)*n) 
                            if(bound%1==0){bound.toInt-1} else{math.floor(bound).toInt}
    }
    def halfDistance: Int = math.floor((n-k).toDouble/2).toInt
    def sudanBound:Int = n-1-math.floor(math.sqrt(2*(k-1)*n)).toInt
  
    override def toString: String = {
        "[" + message.toString.drop(7).dropRight(1) + "] "+ 0x2208.toChar + " F^" + k + "_" + q
    }
    
    def printG ={
        generatorMatrix.transpose.map(_.mkString(" ")).foreach(println)
    }
  
    def encode: RScode = {
        val code = this.generatorMatrix.transpose.map(z => z.zip(message).map(y=>y._1*y._2).fold(field.zeroElement)(_+_))
        new RScode(k,field,code.map(_.pPPO),alphaIn)
    }

}

// Evaluations points alpha are primitive powers from 0 until n by default
// codeIntegers and evaluation point integers alphaIn are interpreted as primitive powers + 1 of the field elements
case class RScode(k: Int, field: FiniteField, codeIntegers: Vector[Int], alphaIn: Option[Vector[Int]] = None) {
  

    val q = field.q
    val n = codeIntegers.length
    val code = codeIntegers.map(x => field.elements(x))
    var alpha: Vector[field.FieldElement] = field.elements.take(n).toVector
    if(alphaIn != None ){alpha = alphaIn.get.map(x => field.elements(x))}
    


    override def toString: String = {
     code.toString.drop(6) + " " + 0x2208.toChar + " [" + n + "," + k + "]_" + q
    }

    def hammingDistance(v: RScode) = this.code.zip(v.code).count{case(a,b) => !(a == b)}
    def hammingDistance(v: Vector[field.FieldElement]) =  this.code.zip(v).count{case(a,b) => !(a == b)}
    def johnsonBound:Int = { 
                            val bound = n-math.sqrt((k-1)*n) 
                            if(bound%1==0){bound.toInt-1} else{math.floor(bound).toInt}
    }
    def halfDistance: Int = math.floor((n-k).toDouble/2).toInt
    def sudanBound:Int = n-1-math.floor(math.sqrt(2*(k-1)*n)).toInt

    //Returns RS codeword y s.t. d_H(x,y)=t, where x is this codeword.
    def makeErrors(t:Int): RScode = {
        var newCode = code
        while(this.hammingDistance(newCode) != t){
            val index = Random.nextInt(n)
            val error = field.randElement
            newCode = newCode.updated(index,error)
        } 
        new RScode(k,field,newCode.map(_.pPPO),alphaIn)
    }

    //Inverse map for RS evaluation map as Lagrange's polynomial
    def lagrange : XYpolynomial = {
        require(alpha.length==code.length)
        var p = new XYpolynomial(Vector(Vector(field.zeroElement)))
        val pf = p.field
        val one = new XYpolynomial(Vector(Vector(field.oneElement)))
        for(i <- 0 until alpha.length){
            var r = one
            var denominator = field.oneElement
            for(j <- 0 until code.length){
                if(i != j){
                    r = r * (new XYpolynomial(Vector(Vector( -alpha(j) ),Vector(field.oneElement))))
                    denominator =( denominator * (alpha(i)+(-alpha(j))) )
                }
            }
            p = p + ( r.scale( code(i) * denominator.inv)  )
        }
        p
    }

    //Compute parameters r and l as presented in jrsn thesis
    def computeParameters(tau: Int): (Int, Int) = {
        val that = (n-tau).toDouble
        val k0 = (k-1).toDouble
        val rmin = (tau*k0)/(math.pow(that,2)-n*k0)
        val r = math.floor(rmin+1).toInt
        val D = (r-rmin)*(math.pow(that,2)-n*k0)*r+math.pow(k0,2)/4
        val  l = math.floor( (that/k0)*r+0.5-math.sqrt(D)/k0 + 0.00000000000001).toInt //this is to reduce floating point errors
        (r,l)
    }

    // Return leading monomial of Q in terms of reverse lexicographic ordering with (1,w)-weighted degree
    // Return values is in the form (coeff,xdegree,ydegree,(xdegree+w*ydegree))
    def wLexYleading(w: Int,Q: XYpolynomial): (field.FieldElement, Int, Int, Int) = {
        Q.coefficients.map(x => x.zipWithIndex.filter(!_._1.isZero)).zipWithIndex.filter(!_._1.isEmpty).map(x => (x._1.last,x._2)).map(x => (x._1._1.asInstanceOf[field.FieldElement], x._2 ,x._1._2, x._2 + w * x._1._2)).sortBy(_._3).sortBy(_._4).last
    }
    // Lee-O'Sullivan polynomial Interpolation
    def LOinterpolator(r:Int,l:Int): XYpolynomial ={
        val h = this.lagrange
        val G = scala.collection.mutable.ArrayBuffer.fill[XYpolynomial](l+1)(new XYpolynomial(Vector(Vector(field.oneElement))))
        val y = new XYpolynomial(Vector(Vector(field.zeroElement,field.oneElement)))
        val x = new XYpolynomial(Vector(Vector(field.zeroElement),Vector(field.oneElement)))
        for(i <- 0 to r){
            var product = new XYpolynomial(Vector(Vector(field.oneElement)))
            for(j <- 0 until n ){
                    product = product*((new XYpolynomial( Vector(Vector( -alpha(j) ),Vector(field.oneElement))) )^(r-i))
            }
            G(i)=((y+(-h))^i)*product
        }

        for(i <- r+1 to l){
            G(i)=(y^(i-r))*((y+(-h))^r)
        }

        var m = 1
        var s = wLexYleading(k-1,G(m))._3
        
     
        while(m <= l){
            s = wLexYleading(k-1,G(m))._3
            while(m != s){
                val crs = G(m).coefficients.map(_(s))
                val css = G(s).coefficients.map(_(s))
                var count_crs = 0
                while(count_crs < crs.length-1 && crs(crs.length-1-count_crs ).isZero ){
                    count_crs +=1
                    }
                var count_css = 0
                while( count_css < css.length-1 && css(css.length-1-count_css).isZero ){
                    count_css +=1
                    }
                
                val d = crs.length-count_crs-css.length+count_css
                val c = (crs(crs.length-1-count_crs).asInstanceOf[field.FieldElement]*(css(css.length-1-count_css).asInstanceOf[field.FieldElement].inv))
                
                if(d >= 0){
                    G(m)= G(m)+((G(s).scale(-c).multiplyXpow(d)))
                } 
                else{
                    val gs = G(s)
                    G(s)=G(m)
                    G(m)=(G(m).multiplyXpow(-d))+((gs.scale(-c)))
                }
                s = wLexYleading(k-1,G(m))._3
            }
            m+=1
        }
     
        val leadingTerms = G.map(z => wLexYleading(k-1,z)).zipWithIndex.sortBy(_._1._3).sortBy(_._1._4)
        G(leadingTerms(0)._2)
    }

    //Gruswami-Sudan decoder with Lee-O'Sullivan interpolation, Roth-Ruckenstein factorization and parameters from jrsn
    def gsDecode(tau: Option[Int] = None, rl: Option[(Int,Int)] = None ): Vector[RSmessage]= {
        require(( tau != None || rl != None ) && ! ( tau != None && rl != None ))
        var params = (-1,-1)
        rl match {
            case None => params = computeParameters(tau.get)
            case Some(x) => params = x
        }
        if(params._1<0 || params._2<0){return Vector[RSmessage]()}
        val res = LOinterpolator(params._1,params._2).factor(k).map(z => new RSmessage(n, field, z.coefficients.padTo(k,Vector(field.zeroElement)).flatten.map(_.pPPO),alphaIn))
        if(tau != None){
            res.filter(_.encode.hammingDistance(this) <= tau.get)
        } else{res}
    }
}  