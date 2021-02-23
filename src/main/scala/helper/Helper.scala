package helper 

//ScalaTion imports
import scalation.analytics.{ANCOVA, Regression, QuadRegression, QuadXRegression}
import scalation.analytics.{CubicRegression, CubicXRegression, LassoRegression, RidgeRegression}
import scalation.analytics.PredictorMat
import scalation.analytics.Fit
import scalation.columnar_db.Relation 
import scalation.linalgebra.{MatriD, MatrixD, VectorD, VectoD, VectorI}
import scalation.plot.{Plot, PlotM}
import scalation.scala2d.VizFrame
import scala.collection.mutable.ArrayBuffer

//scala imports 
import scala.Console
import java.io.{File, FileOutputStream}
import java.awt.image.BufferedImage
import javax.imageio.ImageIO

/**
* Specifies many useful regression utilities using scala that are related to 
* project one. 
* @author Ayush Kumar
*/
object Helper { 

    /**
    * Takes a given regression, does backwards elmination until the 
    * qoF can no longer be improved upon by removing variables. 
    *
    * @param rg regression for backwardElmination 
    * @param index_q the index of the qOF measure to use
    * @return the set of variables in optimal solution, and best model
    */
    def backwardElimination(rg: PredictorMat, index_q: Int = Fit.index_rSqBar): (scala.collection.mutable.Set[Int], PredictorMat) = { 
        val x: MatriD = rg.getX
        val k: Int = x.dim2 - 1

        var selectedVars = scala.collection.mutable.Set((0 to k).toArray :_*)
        var (toBeRemoved, bestModel) = rg.backwardElim(selectedVars, index_q) 
        var bestFit = bestModel.fit(index_q)
        selectedVars -= toBeRemoved

        for (i <- 0 to x.dim2 - 3) { 
            val (toBeRemoved, reg) = rg.backwardElim(selectedVars, Fit.index_aic)
            var newFit: Double = reg.fit(index_q)
            
            if (newFit > bestFit) {
                bestModel = reg
                selectedVars -= toBeRemoved
                bestFit = newFit
            } else { 
                return (selectedVars, bestModel)
            } //else

        } //for i 
        return (selectedVars, bestModel)

    } //backwardElmination

    /**
    * Takes a given regression, does forward selection until the QoF measure is 
    * no longer improved by the addition of another variable and then adds it in. 
    * 
    * @param rg regression to forward select 
    * @param index_q the index to be used for forward selection 
    * @return optimal regression based on AIC 
    */
    def forwardSelection(rg: PredictorMat, index_q: Int = Fit.index_aic): (scala.collection.mutable.Set[Int], PredictorMat) = {
        val x: MatriD = rg.getX
        val k: Int = x.dim2 - 1

        var selectedVars = scala.collection.mutable.Set(0)   
        var (toBeAdded, bestModel) = rg.forwardSel(selectedVars, index_q)
        var bestFit: Double = bestModel.fit(index_q)
        selectedVars += toBeAdded

        
        //Maximum Iterations involves adding all the elements 
        for (i <- 0 to x.dim2 - 3) { 
            val (toBeAdded, reg) = rg.forwardSel(selectedVars, index_q)
            var newFit: Double = reg.fit(index_q)
            if (newFit > bestFit) { 
                bestModel = reg
                selectedVars += toBeAdded
                bestFit = newFit
            } else {
                return (selectedVars, bestModel)
            } //else         
        } //for i 
        return (selectedVars, bestModel)
        
    } //forwardSelection

    /**
    * Plots the 4 Quality of Fit measures used for the backwards elmination, 
    * forward selection, and stepwise regression methods. 
    * 
    * @param rsq R^2 values 
    * @param adjRsq adj-R^2 values 
    * @param cvRsq k-fold cross validated R^2 values 
    * @param aic aic values 
    */
    def plotQoF(rsq: Array[Double], adjRsq: Array[Double], cvRsq: Array[Double], aic: Array[Double], modelName: String, 
                path: String) = { 
        val k = rsq.length
        
        //converting arrays for vectors for plotting purposes 
        var rsqV = new VectorD(k, rsq)
        var adjRsqV = new VectorD(k, adjRsq)
        var cvRsqV = new VectorD(k, cvRsq)
        var aicV = new VectorD(k ,aic)
            
        val domain = (0 to k).toArray.map(x => x.toDouble)
        val range = new VectorD(k, domain)
        
        val rsq_plot = new Plot(range, rsqV, _title = "R^2 vs. Model Complexity")
        val adjRsq_plot = new Plot(range, adjRsqV, _title = "adj-R^2 vs. Model Complexity")
        val cvRsq_plot = new Plot(range, cvRsqV, _title = "cv-R^2 vs. Model Complexity")
        val aic_plot = new Plot(range, aicV, _title = "AIC vs. Model Complexity")

        val titlename = path + "/" + modelName
        save_plot(rsq_plot, titlename + "rsq.png")
        save_plot(adjRsq_plot, titlename + "adjRsq.png")
        save_plot(cvRsq_plot, titlename + "cvRsq.png")
        save_plot(aic_plot, titlename + "aic.png")


    } //plotQoF

    /**
    * Saves a given plot to the designated path. 
    * 
    * @param plot the plot to be saved 
    * @param fname the file path name
    */
    def save_plot(plot: VizFrame, fname: String)  = { 
        val bimg: BufferedImage = new BufferedImage(plot.getSize().width, plot.getSize().height, BufferedImage.TYPE_INT_ARGB)
        val gr = bimg.createGraphics()
        plot.paint(gr)
        gr.dispose()
        ImageIO.write(bimg, "png", new File(fname))
    } //save_plot

} //Helper