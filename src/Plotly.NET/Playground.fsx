//#r "nuget: Plotly.NET"
#r "nuget: DynamicObj, 2.0.0"
#r "nuget: Giraffe.ViewEngine.StrongName, 2.0.0-alpha1"

#I "bin/Debug/netstandard2.0"
#I "bin/Release/netstandard2.0"
#r "Plotly.NET.dll"


//open Giraffe.ViewEngine.StrongName

open Plotly.NET


let xysizes = [1,"Group1",100; 1,"Group2",200; 2,"Group1",110; 2,"Group2",80]    // bubble size (third part of tuple) is diameter in px
Chart.Bubble(xysizes, MarkerSymbol = StyleParam.MarkerSymbol.CircleCross, MarkerColor = Color.fromColorScaleValues (xysizes |> List.map (fun (p,g,s) -> s)), MarkerColorScale = StyleParam.Colorscale.Earth) |> Chart.withMarkerStyle(ShowScale = true) |> Chart.show
Chart.Bubble(xysizes, MarkerSymbol = StyleParam.MarkerSymbol.CircleCross, Orientation = StyleParam.Orientation.Horizontal) |> Chart.show
Chart.Bubble(xysizes, MarkerSymbol = StyleParam.MarkerSymbol.CircleCross, Orientation = StyleParam.Orientation.Vertical) |> Chart.show

["Moliver"; "Oliver"; "Loliver"; "Oliver"] |> List.countBy id


// Overloads:
// - posGroupSizes (position in the group = column, each group = a row, size as pixel diameter) (standard ctor)
// - x (position in the group = column), group (each group = a row), size as pixel diameter (alt ctor)
// - posGroupSizes (position in the group = column, each group = a row, size as pixel area) (alt ctor)
// - x (position in the group = column), group (each group = a row), size as pixel area (alt ctor)
// Options for: 
// - orientation (switches groups and positions (so that groups are now columns and positions are rows)

type Chart with

    static member ProportionalArea(factorGroupSizes: (#System.IConvertible * #System.IConvertible * int) list, ?Orientation : StyleParam.Orientation, ?MarkerSymbol : StyleParam.MarkerSymbol, ?MarkerColor : Color, ?MarkerColorScale : StyleParam.Colorscale) =

        let biggestDiameter = factorGroupSizes |> List.maxBy (fun (p,g,s) -> s) |> fun (p,g,s) -> float s
        let smallestDiameter = factorGroupSizes |> List.minBy (fun (p,g,s) -> s) |> fun (p,g,s) -> float s
        let deltaBiggestSmallest = match biggestDiameter - smallestDiameter with | 0. -> 1. | _ -> biggestDiameter - smallestDiameter
        let sizeAdjusted =      // size shall range from 3 px (lowest) to 175 px (highest)
            factorGroupSizes 
            |> List.map (
                fun (p,g,s) -> 
                    p, 
                    g, 
                    (float s - smallestDiameter) / deltaBiggestSmallest * 172. + 3. |> round |> int
            )

        // use categorical axis for y axis description (maybe via casting to string)

        match Orientation with
        | None
        | Some StyleParam.Orientation.Horizontal ->
            Chart.Bubble(sizeAdjusted, ?MarkerSymbol = MarkerSymbol, ?MarkerColor = MarkerColor, ?MarkerColorScale = MarkerColorScale)
            |> Chart.withYAxis(LayoutObjects.LinearAxis.init (AutoRange = StyleParam.AutoRange.Reversed))
        | Some StyleParam.Orientation.Vertical ->
            sizeAdjusted 
            |> List.map (fun (p,g,s) -> g, p, s)
            |> fun orientationAdjusted -> Chart.Bubble(orientationAdjusted, ?MarkerSymbol = MarkerSymbol) 
            |> Chart.withYAxis(LayoutObjects.LinearAxis.init (AutoRange = StyleParam.AutoRange.Reversed))

    static member PercentageBlocksChart(groupPercentages : (#System.IConvertible * int) list, ?Orientation : StyleParam.Orientation, ?MarkerColor : Color) =
        0


Chart.ProportionalArea(xysizes, MarkerColor = Color.fromColors [Color.fromString "black"; Color.fromString "red"]) |> Chart.show
Chart.ProportionalArea(xysizes, MarkerColor = Color.fromColors [Color.fromString "black"; Color.fromString "red"]) |> Chart.withSize(1800.,800.) |> Chart.show
Chart.ProportionalArea(xysizes, MarkerColorScale = StyleParam.Colorscale.Electric) |> Chart.show
Chart.ProportionalArea(xysizes, Orientation = StyleParam.Orientation.Vertical) |> Chart.show

Chart.Line([1,1; 2,2], LineColorScale = StyleParam.Colorscale.Earth) |> Chart.show
Chart.Line([1,1; 2,2]) |> Chart.show

let xysizes2 = ["Factor1", "Group1", 1000; "Factor1", "Group2", 3; "Factor2", "Group1", 175; "Factor2", "Group2", 10]
Chart.ProportionalArea(xysizes2) |> Chart.show
let xysizes3 = [1., "Group1", 100; 1., "Group2", 50; 1.1, "Group1", 125; 1.00001, "Group2", 10]
Chart.ProportionalArea(xysizes3) |> Chart.show
let xysizes4 = [
    "Factor1", "Group1", 1000
    "Factor1", "Group2", 1000
    "Factor1", "Group3", 1000
    "Factor1", "Group4", 1000
    "Factor1", "Group5", 1000
    "Factor1", "Group6", 1000
    "Factor1", "Group7", 1000
    "Factor1", "Group8", 1000
    "Factor2", "Group1", 1000
    "Factor2", "Group2", 1000
    "Factor2", "Group3", 1000
    "Factor2", "Group4", 1000
    "Factor2", "Group5", 1000
    "Factor2", "Group6", 1000
    "Factor2", "Group7", 1000
    "Factor2", "Group8", 1000
    ]
Chart.ProportionalArea(xysizes4) |> Chart.withSize(1800.,800.) |> Chart.show



/// Takes a seq of floats and performs a [`upperBound`;`lowerBound`]-normalization on it.
let normalize lowerBound upperBound (data: float seq) =
    let yMax = Seq.max data
    let yMin = Seq.min data
    data |> Seq.map (fun y -> (y - yMin) / (yMax - yMin) * (upperBound - lowerBound) + lowerBound)

normalize 3. 175. (xysizes2 |> List.map (fun (p,g,s) -> float s))

let testSatz = [4.; 7.; 3.; 5.; 10.]
let minDavon = List.min testSatz
let maxDavon = List.max testSatz
testSatz |> List.map (fun y -> (y - minDavon) / (maxDavon - minDavon))


let inline add (x : 'T) (y : 'T) : 'T = x + y