

import java.io._
import scala.util.Random


//Helper object for running the examples and tests
object test {
    def main(args: Array[String]) = {
        println("Welcome to GS-decoding program!")
        GS_example_as_presented_in_thesis
        println("Bye!")
        
    }
}

object GS_example_as_presented_in_thesis {
    val (n, k, q) = (10, 4, 11)
    // GF is companion object for initializing finite fields    
    val field = GF(q) 
    // map from field elements values to their primitive powers + 1
    val map = field.asInstanceOf[PrimeField].resToPPPO 
    val evalPo = Vector(1,2,3,4,5,6,7,8,9,10).map(map(_))
    val received = Vector(3, 7, 4, 3, 10, 4, 3, 9, 5, 0).map(map(_))
    val receivedRSCode = new RScode(k, field, received, Some(evalPo))
    //decode all the way to the Johnson Bound
    val tau = receivedRSCode.johnsonBound
    val res = receivedRSCode.gsDecode(Some(tau))
    //print results
    println("Results as primitive powers:")
    res.foreach(println(_))
    println("Results as residue form:")
    res.foreach(x => println(x.message.map(_.toRingElement)))
    println("Hamming distances of the encoded results from the received code:")
    println(res.map(_.encode.hammingDistance(receivedRSCode)))
}

//############## BE AWARE THAT THESE TESTS TAKE HOURS ####################

//Decode with 100 random error patterns (Figure 12 in thesis)
object RandomErrorLengthTest {
    val nv  = Vector(22,22,59,72,79,100)
    val kv = Vector(11,11,48,65,34,49)
    val qv = Vector(23,23,59,73,79,101)
    val tauv = Vector(7,6,6,4,27,29)
    val names = Vector("ZZ_22_11_23_7Lengths.txt","ZZ_22_11_23_6Lengths.txt","ZZ_59_48_59_6Lengths.txt","ZZ_72_65_73_4Lengths.txt","ZZ_79_34_79_27Lengths.txt","ZZ_100_49_101_29Lengths.txt")
    for(i <- 0 until names.length){
        val field = GF(qv(i))
        val n = nv(i)
        val k = kv(i)
        val m = RandMessage(n,k,field)
        val tau = tauv(i)
        val c = m.encode
        val resultLength = scala.collection.mutable.ArrayBuffer[Int]()
        var count = 1
        for(i <- 0 until 100){
            val ec = c.makeErrors(tau)
            val res = ec.gsDecode(Some(tau))
            resultLength.append(res.length)
            println(count + " / 100")
            count += 1
        }
        val file5 = names(i)
        val writer5 = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(file5)))
        for (x <- resultLength) {
            writer5.write(x + "\n") 
        }   
        writer5.close()
        println("next")
    }
}

//Decode from every error pattern (Figure 11 in thesis)
object LengthTest2 {
    val q = 17
    val field = GF(q)
    val n = 16
    val k = 11
    val tau = 3
    val m = new RSmessage(n,field,Vector(2, 3, 1, 7))
    val code = Vector(3, 4, 7, 2, 9, 0, 14, 5, 6, 5, 2, 10, 1, 0, 3, 3)
    val combs = (0 until n).combinations(tau).toVector.map(x => (x(0),x(1),x(2)))
    val resultLength = scala.collection.mutable.ArrayBuffer[Int]()
    val operations = scala.collection.mutable.ArrayBuffer[BigInt]()
    val times = scala.collection.mutable.ArrayBuffer[Long]()
    val erroredCodes = scala.collection.mutable.ArrayBuffer[Vector[Int]]()
    var count = 0
   
    for(i <- combs){
        for(j1 <- 0 until q) {
            for(j2 <- 0 until q){
                for(j3 <- 0 until q){
                    if(j1 != code(i._1) && j2 != code(i._2) && j3 != code(i._3)){
                        val ecode = code.updated(i._1,j1).updated(i._2,j2).updated(i._3,j3)
                        val ec = new RScode(k,field,ecode)
                        val res = ec.gsDecode(Some(tau))
                        resultLength.append(res.length)       
                    }
                }
            }
            count += 256
            println(count + " / 2 500 000")
       }
    }
    
    val file5 = "RL2_resultLength.txt"
    val writer5 = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(file5)))
    for (x <- resultLength) {
        writer5.write(x + "\n") 
    }   
    writer5.close()
    println("Done!!")
}

//Decode from every error pattern (Figure 11 in thesis)
object LengthTest1 {
    val q = 8
    val field = GF(q)
    val n = 8
    val k = 4
    val tau = 3
    val m = new RSmessage(n,field,Vector(2, 3, 1, 7))
    val code = Vector(2, 1, 5, 1, 4, 1, 5, 7)
    val combs = (0 until n).combinations(tau).toVector.map(x => (x(0),x(1),x(2)))
    val resultLength = scala.collection.mutable.ArrayBuffer[Int]()
    
    var count = 1
    for(i <- combs){
        for(j1 <- 0 until q) {
            for(j2 <- 0 until q){
                for(j3 <- 0 until q){
                    if(j1 != code(i._1) && j2 != code(i._2) && j3 != code(i._3)){
                        val ecode = code.updated(i._1,j1).updated(i._2,j2).updated(i._3,j3)
                        field.setCountZero
                        val ec = new RScode(k,field,ecode)
                        val t1 = System.currentTimeMillis
                        val res = ec.gsDecode(Some(tau))
                        val t2  = System.currentTimeMillis
                        resultLength.append(res.length)
                        println(count + " / 19208")
                        count +=1
                    }
                }
            }
        }
        
    }
   
    val file5 = "LengTest1_lengths.txt"
    val writer5 = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(file5)))
    for (x <- resultLength) {
        writer5.write(x + "\n") 
    }   
    writer5.close()
    
}

//Decoding up to the Johnson Bound (Figure 6 in thesis)
object IncreaseErrorsTest {
    def johnsonBound(n:Int,k:Int):Int = { 
        val bound = n-math.sqrt((k-1)*n) 
        if(bound%1==0){bound.toInt-1} else{math.floor(bound).toInt}
    }
    def nextPrimePower(n: Int):Int = {
        var res = n
        while(!Primer.isPrimePower(res)){
            res += 1
        }
        res
    }
    val nk = Vector((81,50),(16,3),(79,34),(100,49),(64,11),(93,26))
    val resultLength = scala.collection.mutable.ArrayBuffer[Int]()
    val nkTaurlValues = scala.collection.mutable.ArrayBuffer[(Int,Int,Int,Int,Int)]()
    val results = scala.collection.mutable.ArrayBuffer[(Vector[Int],Vector[Vector[Int]])]()
    val operations = scala.collection.mutable.ArrayBuffer[BigInt]()
    val times = scala.collection.mutable.ArrayBuffer[Long]()

    for(i <- nk){
        var e = 1
        val n = i._1
        val k = i._2
        val field = GF(nextPrimePower(n))
        while(e <= johnsonBound(n,k)) {
            val m = RandMessage(n,k,field)
            val c = m.encode
            val (r,l) = c.computeParameters(e)
            val ec = c.makeErrors(e)
            field.setCountZero
            val i1 = System.currentTimeMillis
            val res = ec.gsDecode(Some(e))
            val i2 = System.currentTimeMillis
            nkTaurlValues.append((n,k,e,r,l))
            times.append(i2-i1)
            operations.append(field.operationCount)
            results.append((m.messageIntegers,res.map(_.messageIntegers)))
            resultLength.append(res.length)
            e += 1
        }
        println("next")
    }

    val file3 = "EE_Operations.txt"
    val writer3 = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(file3)))
    for (x <- operations) {
        writer3.write(x + "\n")  
    }   
    writer3.close()
    val file4 = "EE_times.txt"
    val writer4 = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(file4)))
    for (x <- times) {
        writer4.write(x + "\n")  
    }   
    writer4.close()
    val file5 = "EE_resultLength.txt"
    val writer5 = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(file5)))
    for (x <- resultLength) {
        writer5.write(x + "\n")  
    }   
    writer5.close()
    val file6 = "EE_nkTaurlValues.txt"
    val writer6 = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(file6)))
    for (x <- nkTaurlValues) {
        writer6.write(x + "\n")  
    }   
    writer6.close()
    val file7 = "EE_results.txt"
    val writer7 = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(file7)))
    for (x <- results) {
        writer7.write(x._1 + " --- " + x._2 + "\n")  
    }   
    writer7.close()

    println("DONE!!!!")

}

//Decoding close to the Johnson Bound with semi random codewords where n in [3,100] (Figures 7-10 in thesis)
object BigTest {
    def nextPrimePower(n: Int):Int = {
        var res = n
        while(!Primer.isPrimePower(res)){
            res += 1
        }
        res
    }
    def johnsonBound(n:Int,k:Int):Int = { 
        val bound = n-math.sqrt((k-1)*n) 
        if(bound%1==0){bound.toInt-1} else{math.floor(bound).toInt}
    }

    def computeParameters(tau: Int, n: Int, k: Int): (Int, Int) = {
        val that = (n-tau).toDouble
        val k0 = (k-1).toDouble
        val rmin = (tau*k0).toDouble/(math.pow(that,2)-n*k0).toDouble
        val r = math.floor(rmin+1).toInt
        val D = (r-rmin)*(math.pow(that,2)-n*k0)*r+(math.pow(k0,2)).toDouble/4
        val  l = math.floor( (that/k0)*r+0.5-math.sqrt(D)/k0 + 0.00000000000001).toInt
        (r,l)
    }

    def Tau(n: Int, k: Int ): Int = {
        var tau = johnsonBound(n,k)
        if(computeParameters(tau,n,k)._1 > 15){
            tau = tau -1
        }
        tau
    }
    val interpolationOperations = scala.collection.mutable.ArrayBuffer[BigInt]()
    val factorizationOperations = scala.collection.mutable.ArrayBuffer[BigInt]()
    val interpolationTimes = scala.collection.mutable.ArrayBuffer[Long]()
    val factorizationTimes = scala.collection.mutable.ArrayBuffer[Long]()
    val resultLength = scala.collection.mutable.ArrayBuffer[Int]()
    val nkTaurlValues = scala.collection.mutable.ArrayBuffer[(Int,Int,Int,Int,Int)]()
    val results = scala.collection.mutable.ArrayBuffer[(Vector[Int],Vector[Vector[Int]])]()
    var count = 0
    val ns = (3 to 100).toVector.filter(Primer.isPrimePower(_))
    for(n <- ns){
        val ks = scala.util.Random.shuffle((2 until n).toVector).take(n/2)
        for(k <- ks){
            val R = k.toDouble/n.toDouble
            if(R >= 0.1 && R <= 0.9){
                val q = nextPrimePower(n)
                val field = GF(q)
                val tau = Tau(n,k)
                val (r,l) = computeParameters(tau,n,k)
                val m = RandMessage(n,k,field)
                val c = m.encode
                val ec = c.makeErrors(tau)
                field.setCountZero
                val i1 = System.currentTimeMillis
                val Q = ec.LOinterpolator(r,l)
                val i2 = System.currentTimeMillis
                interpolationOperations.append(field.operationCount)
                interpolationTimes.append(i2-i1)
                field.setCountZero
                val f1 = System.currentTimeMillis
                val res = Q.factor(k) 
                val f2 = System.currentTimeMillis
                factorizationOperations.append(field.operationCount)
                factorizationTimes.append(f2-f1)
                resultLength.append(res.length)
                nkTaurlValues.append((n,k,tau,r,l))
                results.append((m.messageIntegers,res.map(_.coefficients.flatten.map(_.pPPO))))
            }
        }
        count+=1
        println(count)
    }

    val file1 = "BB_interpolationOperations3.txt"
    val writer1 = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(file1)))
    for (x <- interpolationOperations) {
        writer1.write(x + "\n")  
    }   
    writer1.close()
    val file2 = "BB_interpolationTimes3.txt"
    val writer2 = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(file2)))
    for (x <- interpolationTimes) {
        writer2.write(x + "\n")  
    }   
    writer2.close()
    val file3 = "BB_factorizationOperations3.txt"
    val writer3 = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(file3)))
    for (x <- factorizationOperations) {
        writer3.write(x + "\n")  
    }   
    writer3.close()
    val file4 = "BB_factorizationTimes3.txt"
    val writer4 = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(file4)))
    for (x <- factorizationTimes) {
        writer4.write(x + "\n") 
    }   
    writer4.close()
    val file5 = "BB_resultLength3.txt"
    val writer5 = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(file5)))
    for (x <- resultLength) {
        writer5.write(x + "\n")  
    }   
    writer5.close()
    val file6 = "BB_nkTaurlValues3.txt"
    val writer6 = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(file6)))
    for (x <- nkTaurlValues) {
        writer6.write(x + "\n") 
    }   
    writer6.close()
    val file7 = "BB_results3.txt"
    val writer7 = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(file7)))
    for (x <- results) {
        writer7.write(x._1 + " --- " + x._2 + "\n")  
    }   
    writer7.close()

    println("DONE!!!!")
}



