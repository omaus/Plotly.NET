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

        let biggestDiameter = posGroupSizes |> List.maxBy (fun (p,g,s) -> s)
        let smallesDiameter = posGroupSizes |> List.minBy (fun (p,g,s) -> s)
        // search for normalize in Master Thesis code...
        let y = posGroupSizes |> List.map 

        match Orientation with
        | None
        | Some StyleParam.Orientation.Horizontal ->
            Chart.Bubble(posGroupSizes, ?MarkerSymbol = MarkerSymbol, ?MarkerColor = MarkerColor, ?MarkerColorScale = MarkerColorScale)
        | Some StyleParam.Orientation.Vertical ->
            posGroupSizes 
            |> List.map (fun (p,g,s) -> g, p, s)
            |> fun orientationAdjusted -> Chart.Bubble(orientationAdjusted, ?MarkerSymbol = MarkerSymbol) 
            //|> Chart.withYAxisStyle()     // search for a way to reverse y axis but without changing the values since they could be strings


Chart.ProportionalArea(xysizes, MarkerColors = [Color.fromString "black"; Color.fromString "red"]) |> Chart.show
Chart.ProportionalArea(xysizes, MarkerColorScales = [StyleParam.Colorscale.Electric]) |> Chart.show
Chart.ProportionalArea(xysizes, Orientation = StyleParam.Orientation.Vertical) |> Chart.show

Chart.Line([1,1; 2,2], LineColorScale = StyleParam.Colorscale.Earth) |> Chart.show
Chart.Line([1,1; 2,2]) |> Chart.show

let xysizes2 = ["Factor1", "Group1", 175; "Factor1", "Group2", 3; "Factor2", "Group1", 175; "Factor2", "Group2", 10]
Chart.ProportionalArea(xysizes2) |> Chart.show
let xysizes3 = [1., "Group1", 100; 1., "Group2", 50; 1.1, "Group1", 125; 1.00001, "Group2", 10]
Chart.ProportionalArea(xysizes3) |> Chart.show