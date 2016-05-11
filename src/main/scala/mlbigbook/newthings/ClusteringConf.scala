package mlbigbook.newthings

case class ClusteringConf(
  nClusters:     Int,
  tolerance:     Double,
  maxIterations: Int
)