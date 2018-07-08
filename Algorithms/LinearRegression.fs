[<RequireQualifiedAccess>]
module LinearRegression

#if INTERACTIVE
#load @"Common.fs"
#endif

open Common.Math

type Coefficients = 
  {
    b0 : float
    b1 : float
  }

let private coefficients xFn yFn dataset =
  let x = dataset |> List.map xFn
  let xMean = x |> List.average

  let y = dataset |> List.map (yFn >> float)
  let yMean = y |> List.average

  let b1 = (covariance x xMean y yMean) / (x |> variance xMean)
  let b0 = yMean - b1 * xMean

  {
    b0 = b0
    b1 = b1
  }

let private predict coeffs map x =
  coeffs.b0 + coeffs.b1 * (x |> map)

let train xFn yFn trainData =
  trainData |> coefficients xFn yFn

let test coeffs xFn testData =
  testData |> List.map (predict coeffs xFn)

let evaluate xFn yFn trainData testData =
  let coeffs = train xFn yFn trainData

  let predicted = test coeffs xFn testData

  printfn "Predicted: %A" predicted

  let actual = testData |> List.map yFn

  printfn "Actual: %A" actual

  rootMeanSquaredError actual predicted

