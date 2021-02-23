package regression 

//ScalaTion imports
import scalation.analytics.{ANCOVA, Regression, QuadRegression, QuadXRegression, 
                            CubicRegression, CubicXRegression, LassoRegression, RidgeRegression}
import scalation.analytics.PredictorMat
import scalation.analytics.Fit
import scalation.columnar_db.Relation 
import scalation.linalgebra.{MatriD, MatrixD, VectorD, VectoD, VectorI}
import scalation.plot.Plot
import scalation.util.banner

//Scala imports 
import scala.Console
import java.io.{File, FileOutputStream}

//other imports 
import helper.Helper

/**
* Regression Analysis for winequality dataset. 
* @author Ayush Kumar
*/
object Wine extends App {

    val red_wine = Relation.apply("data/WineQuality/winequality-red.csv", "red_wine", 
                                domain=null, key = 0, eSep = ";", cPos = null)
    red_wine.show(5)

    val (x,y) = red_wine.toMatriDD(0 to 10, 11)
    val ox = new MatrixD(x.dim1, 1, 1.0) ++^ x // x augmented with vector of ones 
    //Creating all the models
    val MVR = new Regression(ox,y) 
    val Quad = new QuadRegression(x,y)
    val QuadX = new QuadXRegression(x, y) 
    val Cubic = new CubicRegression(x, y) 
    val CubicX = new CubicXRegression(x, y) 
    
    val Ridge = new RidgeRegression(ox, y) 
    val Lasso = new LassoRegression(ox, y)

    banner("Correlation Matrix")
    println(MVR.corrMatrix())

    banner("Simple Linear Model")
    
    var (backRegMVR, backReg) = Helper.backwardElimination(MVR, index_q = Fit.index_rSqBar)
    println(backRegMVR)
    
    var (forRegMVR, forReg) = Helper.forwardSelection(MVR, index_q = Fit.index_rSqBar)
    //MVR.forwardSelAll(cross = false, index_q = Fit.index_rSqBar)
    println(forRegMVR)
    
    var (stepRegMVR, stepReg) = MVR.stepRegressionAll()
    println(stepRegMVR, stepReg)
        
    
    banner("Quadratic Model")

    var (backRegQuad, backRegQ) = Helper.backwardElimination(Quad, index_q = Fit.index_rSqBar)
    println(backRegQuad)
    
    var (forRegQuad, forRegQ) = Helper.forwardSelection(Quad, index_q = Fit.index_rSqBar)
    //MVR.forwardSelAll(cross = false, index_q = Fit.index_rSqBar)
    println(forRegQuad)
    
    var (stepRegQuad, stepRegQ) = Helper.stepRegressionAll(Quad, index_q = Fit.index_rSqBar, suppressOutput = true)
    println(stepRegQuad)
    
    
    banner("Quadratic Model with Cross-Term")

    var (backRegQuadX, backRegQX) = Helper.backwardElimination(QuadX, index_q = Fit.index_rSqBar)
    println(backRegQuadX)
    
    var (forRegQuadX, forRegQX) = Helper.forwardSelection(QuadX, index_q = Fit.index_rSqBar)
    println(forRegQuadX)
    
    var (stepRegQuadX, stepRegQX) = Helper.stepRegressionAll(QuadX, index_q = Fit.index_rSqBar, suppressOutput = true)
    println(stepRegQuadX)
    
    banner("Cubic Model")

    //var (backRegCubic, backRegC) = Helper.backwardElimination(Cubic, index_q = Fit.index_rSqBar)
    //println(backRegCubic)
    
    var (forRegCubic, forRegC) = Helper.forwardSelection(Cubic, index_q = Fit.index_rSqBar)
    println(forRegCubic)
    
    var (stepRegCubic, stepRegC) = Helper.stepRegressionAll(Cubic, index_q = Fit.index_rSqBar, suppressOutput = true)
    println(stepRegCubic)

    
    banner("Cubic Model with Cross Term")

    //var (backRegCubicX, backRegCX) = Helper.backwardElimination(CubicX, index_q = Fit.index_rSqBar)
    //println(backRegCubicX)
    
    var (forRegCubicX, forRegCX) = Helper.forwardSelection(CubicX, index_q = Fit.index_rSqBar)
    println(forRegCubicX)
    
    var (stepRegCubicX, stepRegCX) = Helper.stepRegressionAll(CubicX, index_q = Fit.index_rSqBar, suppressOutput = false)
    println(stepRegCubicX)

} // Wine