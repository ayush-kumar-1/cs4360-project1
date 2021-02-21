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
    * Takes a given regression, does backwards elmination all the way through, 
    * and then optionaly plots R^2, adj-R^2, cv-R^2, AIC for all possible models. 
    * best model is determined by AIC. 
    *
    * @param rg regression for backwardElmination 
    * @param plot true if plots should be generated 
    * @param path where the plots should be saved
    * @param modelName the name of the model 
    * @return optimal regression based on AIC
    */
    def backwardElimination(rg: PredictorMat, modelName: String, 
                            plot: Boolean = false, path: String): PredictorMat =  { 
        val x: MatriD = rg.getX
        val k: Int = x.dim2 - 1

        var selectedVars = scala.collection.mutable.Set((0 to k).toArray :_*)
        var rsq = new Array[Double](k)
        var adjRsq = new Array[Double](k)
        var cvRsq = new Array[Double](k)
        var aic = new Array[Double](k)
        var bestModel: PredictorMat = null
        
        
        for (i <- 0 to x.dim2 - 3) { 
            val (toBeRemoved, reg) = rg.backwardElim(selectedVars, Fit.index_aic)
            var qoF= reg.analyze().fit
            //updating arrays for plotting purposes
            val fos = new FileOutputStream(new File("./data/Garbage.txt"))
            Console.withOut(fos) {
            cvRsq(i) = reg.analyze().crossValidate()(0).mean
            } //suppressing output
            adjRsq(i) = qoF(1)
            rsq(i) = qoF(0)
            aic(i) = qoF(11)
            
            if (toBeRemoved >= 0) { 
                selectedVars -= toBeRemoved
            } //if

            if (i == 0) { //first time bestmodel will be none
                bestModel = reg; 
            } else if (qoF(11) < bestModel.analyze().fit(11)) { 
                bestModel = reg; 
            } //elif

        } //for i 

        if (plot) { 
            plotQoF(rsq.reverse, adjRsq.reverse, cvRsq.reverse, aic.reverse, modelName, path)
        } //plot
        
        return bestModel

    } //backwardElmination

    /**
    * Takes a given regression, does forward selection all the way through, and 
    * then optionally plots R^2, adj-R^2, cv-R^2, AIC for all possible models. Best
    * model is determined by lowest AIC. 
    * 
    * @param rg regression to forward select 
    * @param plot true if plot should be generated
    * @param path where the plots should be saved
    * @return optimal regression based on AIC 
    */
    def forwardSelection(rg: PredictorMat, modelName: String, plot: Boolean = false, path: String = null): PredictorMat = {
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

        if (plot) { 
            plotQoF(rsq, adjRsq, cvRsq, aic, modelName, path)
        } //plot

        return bestModel

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

    /**
    * Stepwise regression is an improvement over backward elmination and forward selection as it 
    * considers adding and removing variables at every step. 
    */
    def stepRegression (cols: scala.collection.mutable.Set[Int], index_q: Int = Fit.index_aic, 
        first: Int = 1, rg: PredictorMat): (Int, PredictorMat) = { 
            

    }

} //Helper