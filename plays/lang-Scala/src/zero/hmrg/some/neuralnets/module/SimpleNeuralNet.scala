package zero.hmrg.some.neuralnets.module

/*
seq net(same as layers):
    1 layer
    2 layer
    3 layer
    ...

list layer(same as lines):
    1 line
    2 line
    3 line
    ...

list,num line:
    weights,num ;

list weights:
    1 weight
    2 weight
    ...
*/

/**
 * make a simple neural-net from zero
 * use only one file
 * use only basic feature in Scala-lang
 * I want the code as easy as possible for read, means I want it as little ambiguity as possible
 */
case class SimpleNeuralNet
( weightLineBiaListSeq: Seq[(Int,List[(Int,(List[(Int,Double)],Double))])] )
object SimpleNeuralNet
{
    object Maker
    {
        /**
         * one line is : many weights -- 1 bia
         * one layer is : many lines
         * one net is : many layers
         */
        object Mode
        {
            trait NetMakeMode ;
            object RANDOM extends NetMakeMode ;
            object ZERO extends NetMakeMode ;
        } ;
        def makeNet
        ( modeChoose: Mode.NetMakeMode = Mode.RANDOM )
        ( sizeLineIn:Int, sizeLineOut:Int, sizesForHiddenLayer:Seq[Int] )
        : SimpleNeuralNet =
        {
            val sizesForLayer
            : List[Int] = sizesForHiddenLayer.toList ::: List(sizeLineOut) ;
            
            val sizeForNet
            : Int = sizesForLayer.size ;
            
            val sizeOfLines
            : Seq[(Int,Int)] = (1 to sizeForNet) zip (List(sizeLineIn) ::: sizesForHiddenLayer.toList) ;
            
            def getLineSizeForLayer (indexOfLayer:Int)
            : Int =
            {
                return sizeOfLines.toMap.get(indexOfLayer)getOrElse(-1) ;
            } ;
            
            def makeRandomLineBia (sizeOfLine:Int)
            : (List[(Int, Double)], Double) =
            {
                val indexedLine
                : List[(Int, Double)] =
                    (1 to sizeOfLine)
                        .toList
                        .map( indexInLine => indexInLine -> scala.util.Random.nextDouble ) ;
                return indexedLine ->
                       ( scala.util.Random.nextDouble +
                         scala.util.Random.nextInt(32) - 16 -
                         scala.util.Random.nextDouble ) ;
            } ;
            
            def makeZeroLineBia (sizeOfLine:Int)
            : (List[(Int, Double)], Double) =
            {
                val indexedLine
                : List[(Int, Double)] =
                    (1 to sizeOfLine)
                        .toList
                        .map( indexInLine => indexInLine -> 0.0 ) ;
                return indexedLine -> 0.0 ;
            } ;
            
            val makeLineBia
            : (Int) => (List[(Int, Double)], Double) =
                modeChoose match
                {
                    case Mode.RANDOM => makeRandomLineBia _
                    case _ | Mode.ZERO => makeZeroLineBia _
                } ;
            
            val netDataMade
            : Seq[(Int,List[(Int,(List[(Int,Double)],Double))])] =
                ((1 to sizeForNet) zip sizesForLayer)
                    .toSeq
                    .map
                    {
                        case (indexOfLayer,sizeLayer) =>
                            indexOfLayer ->
                            (1 to sizeLayer)
                                .toList
                                .map(
                                    /* here need to be: one index -> one line-bia */
                                    indexOfLine =>
                                        indexOfLine ->
                                        makeLineBia(getLineSizeForLayer(indexOfLayer))
                                    )
                        /* here need to be: one index -> one layer */
                    } ;
            return apply(netDataMade) ;
        } ;
    } ;
    
    object SomeGift
    {
        def relu (n:Double)
        : Double =
        {
            return if (n < 0) 0 else n ;
        } ;
    } ;
    
    object Operator
    {
        def multiplyListElems ( a:List[Double], b:List[Double] )
        : List[Double] =
        {
            return (a.map(Some(_)) zip b.map(Some(_)))
                .map{ case (Some(a),Some(b)) => a * b } ;
        } ;
        def getNextLayerNeuronList ( neuronList:List[Double], neuralJunctionGroupList:List[List[Double]] )
        : List[Double] =
        {
            /**
             * neuralJunctionGroup.Size should be eq to neuronList.Size
             * neuralJunctionGroupList.Size should be eq to nextLayerNeuronList.Size
             */
            return neuralJunctionGroupList
                .map(
                    neuralJunctionGroup =>
                        multiplyListElems(neuralJunctionGroup, neuronList).sum
                    ) ;
        } ;
    } ;
}