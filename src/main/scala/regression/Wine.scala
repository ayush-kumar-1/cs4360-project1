package regression 

import scalation.analytics.{ANCOVA, Regression, QuadRegression, QuadXRegression, 
                            CubicRegression, CubicXRegression, LassoRegression, RidgeRegression}
import scalation.analytics.PredictorMat
import scalatoin.analytics.Fit
import scalation.columnar_db.Relation 
import scalation.linalgebra.{MatriD, MatrixD, VectorD, VectoD}


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
    val Quad = new QuadRegression(ox,y)
    val QuadX = new QuadXRegression(ox, y) 
    val Cubic = new CubicRegression(ox, y) 
    val CubicX = new CubicXRegression(ox, y) 

    /**
    * Takes a given regression, does forward selection all the way through, and 
    * then optionally plots R^2, adj-R^2, cv-R^2, AIC for all possible models. 
    * 
    * @param rg regression to forward select 
    * @param plot true if plot should be generated
    * @return optimal regression based on AIC 
    */
    def forwardSelection(rg: PredictorMat,  plot = false: Boolean): PredictorMat = {
        val k: Int = x.dim2

        var selectedVars = scala.collection.mutable.Set(0)
        var Rsq = new Array[Double](k)
        var adjRsq = new Array[Double](k)
        var cvRsq = new Array[Double](k)
        var AIC = new Array[Double](k)
        
        var result: PredictorMat = rg; 

        for (i <- 0 to k) { 
            var (i, reg) = rg.forwardSel(selectedVars, Fit.index_aic)
            
        } //for i 

    } //forwardSelection
    var zeroSet = scala.collection.mutable.Set(0)
    println(MVR.forwardSel(zeroSet))

} // Wine