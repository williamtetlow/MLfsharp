module Common

module Math =
  let variance mean (vals : float list) = vals |> List.fold(fun v x -> v + (x - mean) ** 2.0) 0.0

  let covariance xVals xMean yVals yMean =
    (xVals, yVals)
    ||> List.fold2 (fun cv x y -> cv + (x - xMean) * (y - yMean)) 0.0

  let rootMeanSquaredError xVals yVals =
    let sumErr = List.fold2 (fun se x y -> se + (x - y) ** 2.0) 0.0 xVals yVals
    let meanErr = sumErr / (xVals |> List.length |> float)
    sqrt meanErr
