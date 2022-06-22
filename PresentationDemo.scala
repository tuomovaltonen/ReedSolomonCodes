import scala.swing._

//Small GUI for the GS-decoder used in the presentation of the Bachelor's thesis
//Not very pretty or commented code
object PresentationDemo extends SimpleSwingApplication {
    
    def top = new MainFrame {
        val k = 11
        val n = 16
        val q = 25
        val field = GF(q)
        var m = RandMessage(n,k,field)
        var c = m.encode
        var ec = c
        var result = Vector[RSmessage]()
        var readyToTransmit = false
        var readyToDecode = false

        var polString = ""
       
        val emptyIcon = new javax.swing.ImageIcon()
        val iconSq  = new javax.swing.ImageIcon("Kirjaimet/graysquare.png")

        
        val labels = Vector.fill[Label](k)(new Label {icon = iconSq })
        val labels2 = Vector.fill[Label](n)(new Label)
        val labels3 = Vector.fill[Label](n)(new Label)

        var resultLabels = Vector.fill(10)(Vector.fill[Label](k)(new Label))
        def setResultEmpty = {
            resultLabels.foreach(_.foreach(_.icon = emptyIcon))
        }
        val viestiA = scala.collection.mutable.ArrayBuffer.fill[Int](k)(-1)

        var count = 0
        val aakkosetIcon = Vector("A.gif","B.jpeg","C.png","D.gif","E.gif","F.gif","G.gif","H.png","I.jpeg","J.gif","K.jpeg","L.gif","M.png","N.png","O.png","P.gif","Q.jpeg","R.jpeg","S.png","T.gif","U.png","V.gif","W.jpeg","X.png","Y.jpeg","Z.png").map(x => "Kirjaimet/" + x).map(x => new javax.swing.ImageIcon(x){
            getImage().getScaledInstance(1000,1000,10)
        })
        val events0 = Vector(event.Key.A, event.Key.B, event.Key.C, event.Key.D, event.Key.E, event.Key.F, event.Key.G, event.Key.H, event.Key.I, event.Key.J, event.Key.K, event.Key.L, event.Key.M, event.Key.N, event.Key.O, event.Key.P, event.Key.Q, event.Key.R, event.Key.S, event.Key.T, event.Key.U, event.Key.V, event.Key.W,event.Key.X, event.Key.Y, event.Key.Z).zipWithIndex
        contents =  new BoxPanel(Orientation.Vertical){
            contents += new BoxPanel(Orientation.Horizontal) {
                contents ++= labels
                border = Swing.EmptyBorder(30,30,10,10)
                listenTo(keys)
                for(x <- events0){
                    reactions += {
                        case event.KeyPressed(_, x._1, _, _) =>
                            if(count<k){
                            labels(count).icon = aakkosetIcon(x._2)
                            viestiA(count)= x._2
                            if(count < k){count += 1}
                            }
                    }
                }
                reactions+= {
                        case event.KeyPressed(_, event.Key.BackSpace, _, _) =>
                            if(count>0){count-=1}
                            labels(count).icon = iconSq
                            viestiA(count)= (-1)
                            
                    }
                
                focusable = true
                requestFocus
            }
            contents += new Button{
                    text = "Encode"
                    reactions += {
                        case event.ButtonClicked(_) =>
                        if(viestiA.forall(_>=0)){
                            m = new RSmessage(n,field,viestiA.toVector)
                            c = m.encode
                            for(i <- 0 until n){
                                labels2(i).icon=aakkosetIcon(c.codeIntegers(i))
                            }
                            readyToTransmit = true
                        }
                        focusable = false
                    
                    }
                }
            contents += new ScrollPane(new BoxPanel(Orientation.Horizontal) {
                contents ++= labels2
            })
            contents += new BoxPanel(Orientation.Horizontal) {
                val errors = new TextField {
                    text = "0"
                    columns = 5
                    maximumSize =  new Dimension(30, 30)
                }
                contents += errors
                contents += new Button{
                    text = "Make Errors"
                    reactions += {
                        case event.ButtonClicked(_) =>
                            if(readyToTransmit){
                                ec = c.makeErrors(errors.text.toInt)
                                for(i <- 0 until n){
                                labels3(i).icon=aakkosetIcon(ec.codeIntegers(i))
                                readyToDecode = true
                            }
                            }

                            
                    }
                }
            }
            
          
            contents += new ScrollPane(new BoxPanel(Orientation.Horizontal) {
                contents ++= labels3
            })

            contents += new BoxPanel(Orientation.Horizontal){
                val tau = new TextField {
                    text = "0"
                    columns = 2
                    maximumSize =  new Dimension(30, 30)

                }
                
                contents += tau
                contents += new Button{
                    text = "Decode"
                    reactions += {
                        case event.ButtonClicked(_) =>
                            if(readyToDecode){
                                setResultEmpty
                                field.setCountZero
                                val (r,l) = ec.computeParameters(tau.text.toInt)
                                if(r > 0 && l > 0){
                                val pol = ec.LOinterpolator(r,l)
                                var helper = ""
                                polString = pol.toString()
                                var laskuri = 0
                                for(a<-polString){
                                    if(laskuri%100 == 0){
                                        helper = helper + "\n"
                                    }
                                    helper = helper + a
                                    laskuri +=1 
                                }
                                
                                result = pol.factor(k).map(z => new RSmessage(n, field, z.coefficients.padTo(k,Vector(field.zeroElement)).flatten.map(_.pPPO)))
                                result = result.filter(_.encode.hammingDistance(ec) <= tau.text.toInt)
                                area.text = "interpolation Polynomial: \n" + helper + "\n \n"+ "Number of field operations: \n" + field.operationCount.toString() + "\nresult length: " + result.length.toString()
                                }
                                if(result.isEmpty || r < 0 || l < 0){
                                    resultLabels.foreach(_.foreach(_.icon = new javax.swing.ImageIcon()))
                                    resultLabels(0)(0).icon = aakkosetIcon(4)
                                    resultLabels(0)(1).icon = aakkosetIcon(17)
                                    resultLabels(0)(2).icon = aakkosetIcon(17)
                                    resultLabels(0)(3).icon = aakkosetIcon(14)
                                    resultLabels(0)(4).icon = aakkosetIcon(17)
                                } else{
                                for(j <- 0 until result.length){
                                    for(i <- 0 until k){
                                        resultLabels(j)(i).icon =  aakkosetIcon(result(j).messageIntegers(i))
                                    }
                                }
                                
                                }
                                
                            }
                            focusable = false
                    }
                 
                    
                }

            }
            
            val area =   new TextArea("interpolation Polynomial: \n", 10, 10){
                focusable = false
            }
            contents += new ScrollPane(area)
            contents ++= resultLabels.map(x => new BoxPanel(Orientation.Horizontal){contents ++= x})
        
        }
        
    }
}      
