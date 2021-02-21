package helper 

//ScalaTion imports
import scalation.analytics.{ANCOVA, Regression, QuadRegression, QuadXRegression, 
                            CubicRegression, CubicXRegression, LassoRegression, RidgeRegression}
import scalation.analytics.PredictorMat
import scalation.analytics.Fit
import scalation.columnar_db.Relation 
import scalation.linalgebra.{MatriD, MatrixD, VectorD, VectoD, VectorI}
import scalation.plot.Plot

//scala imports 
import scala.Console
import java.io.{File, FileOutputStream}

/**
* Specifies many useful regression utilities using scala that are related to 
* project one. 
* @author Ayush Kumar
*/
object Helper { 

    /**
    * Takes a given regression, does forward selection all the way through, and 
    * then optionally plots R^2, adj-R^2, cv-R^2, AIC for all possible models. Best
    * model is determined by lowest AIC. 
    * 
    * @param rg regression to forward select 
    * @param plot true if plot should be generated
    * @return optimal regression based on AIC 
    */
    def forwardSelection(rg: PredictorMat, plot: Boolean = false): PredictorMat = {
        val x: MatriD = rg.getX
        val k: Int = x.dim2 - 1

        var selectedVars = scala.collection.mutable.Set(0)
        var rsq = new Array[Double](k)
        var adjRsq = new Array[Double](k)
        var cvRsq = new Array[Double](k)
        var aic = new Array[Double](k)
        var bestModel: PredictorMat = null
        
        
        for (i <- 0 to x.dim2 - 2) { 
            val (toBeAdded, reg) = rg.forwardSel(selectedVars, Fit.index_aic)
            var qoF= reg.analyze().fit
            //updating arrays for plotting purposes
            val fos = new FileOutputStream(new File("./data/Garbage.txt"))
            Console.withOut(fos) {
            cvRsq(i) = reg.analyze().crossValidate()(0).mean
            } //suppressing output
            adjRsq(i) = qoF(1)
            rsq(i) = qoF(0)
            aic(i) = qoF(11)
            
            if (toBeAdded >= 0) { 
                selectedVars += toBeAdded
            } //if

            if (i == 0) { //first time bestmodel will be none
                bestModel = reg; 
            } else if (qoF(11) < bestModel.analyze().fit(11)) { 
                bestModel = reg; 
            } //elif

        } //for i 

        //converting arrays for vectors for plotting purposes 
        var rsqV = new VectorD(k, rsq)
        var adjRsqV = new VectorD(k, adjRsq)
        var cvRsqV = new VectorD(k, cvRsq)
        var aicV = new VectorD(k ,aic)

        //plotting if required
        if (plot) { 
            val domain = (0 to k).toArray.map(x => x.toDouble)
            val range = new VectorD(k, domain)
            new Plot(range, rsqV, _title = "R^2 vs. Model Complexity")
            new Plot(range, adjRsqV, _title = "adj-R^2 vs. Model Complexity")
            new Plot(range, cvRsqV, _title = "cv-R^2 vs. Model Complexity")
            new Plot(range, aicV, _title = "AIC vs. Model Complexity")
        } //if

        return bestModel

    } //forwardSelection

} //Helper