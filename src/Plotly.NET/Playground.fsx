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

    static member ProportionalArea(posGroupSizes: (#System.IConvertible * #System.IConvertible * int) list, ?Orientation: StyleParam.Orientation, ?MarkerSymbol: StyleParam.MarkerSymbol, ?MarkerColor: Color, ?MarkerColorScale: StyleParam.Colorscale) =

        let biggestDiameter = posGroupSizes |> List.maxBy (fun (p,g,s) -> s) |> fun (p,g,s) -> float s
        let smallestDiameter = posGroupSizes |> List.minBy (fun (p,g,s) -> s) |> fun (p,g,s) -> float s
        let sizeAdjusted = posGroupSizes |> List.map (fun (p,g,s) -> p, g, (float s + 3. - smallestDiameter) / (biggestDiameter + 3. - smallestDiameter) * 175. |> int)

        // use categorical axis for y axis description (maybe via casting to string)

        match Orientation with
        | None
        | Some StyleParam.Orientation.Horizontal ->
            Chart.Bubble(sizeAdjusted, ?MarkerSymbol = MarkerSymbol, ?MarkerColor = MarkerColor, ?MarkerColorScale = MarkerColorScale)
        | Some StyleParam.Orientation.Vertical ->
            sizeAdjusted 
            |> List.map (fun (p,g,s) -> g, p, s)
            |> fun orientationAdjusted -> Chart.Bubble(orientationAdjusted, ?MarkerSymbol = MarkerSymbol) 
            |> Chart.withYAxis(LayoutObjects.LinearAxis.init (AutoRange = StyleParam.AutoRange.Reversed))     // search for a way to reverse y axis but without changing the values since they could be strings


Chart.ProportionalArea(xysizes, MarkerColor = Color.fromColors [Color.fromString "black"; Color.fromString "red"]) |> Chart.show
Chart.ProportionalArea(xysizes, MarkerColorScales = [StyleParam.Colorscale.Electric]) |> Chart.show
Chart.ProportionalArea(xysizes, Orientation = StyleParam.Orientation.Vertical) |> Chart.show

Chart.Line([1,1; 2,2], LineColorScale = StyleParam.Colorscale.Earth) |> Chart.show
Chart.Line([1,1; 2,2]) |> Chart.show

let xysizes2 = ["Factor1", "Group1", 1000; "Factor1", "Group2", 3; "Factor2", "Group1", 175; "Factor2", "Group2", 10]
Chart.ProportionalArea(xysizes2) |> Chart.show
let xysizes3 = [1., "Group1", 100; 1., "Group2", 50; 1.1, "Group1", 125; 1.00001, "Group2", 10]
Chart.ProportionalArea(xysizes3) |> Chart.show



/// Takes an array of floats and performs a 0;1-normalization on it.
let normalize (data: float []) =
    let yMax = Array.max data
    let yMin = Array.min data
    data |> Array.map (fun y -> (y + 3. - yMin) / (yMax + 3. - yMin) * 175.)

normalize (xysizes2 |> List.map (fun (p,g,s) -> float s) |> Array.ofList)

let testSatz = [4.; 7.; 3.; 5.; 10.]
let minDavon = List.min testSatz
let maxDavon = List.max testSatz
testSatz |> List.map (fun y -> (y - minDavon) / (maxDavon - minDavon))


let inline add (x : 'T) (y : 'T) : 'T = x + y