(*** hide ***)
#r "netstandard"
#r @"../../lib/Formatting/FSharp.Plotly.dll"

(** 
# FSharp.Plotly: Bar and Column Charts

*Summary:* This example shows how to create bar and a column charts in F#.

A bar chart or bar graph is a chart that presents grouped data with rectangular bars with 
lengths proportional to the values that they represent. The bars can be plotted vertically
or horizontally. A vertical bar chart is called a column bar chart.
*)

open FSharp.Plotly 
  
let values = [20; 14; 23;]
let keys   = ["Product A"; "Product B"; "Product C";]
let labels = ["27% market share"; "24% market share"; "19% market share";]

let bar1 =
    Chart.Column(keys,values,Labels=labels,Opacity=0.3,Marker=Marker.init(Color="rgba(222,45,38,0.8)",Size=1)) // Changing the thickness of the bar is not possible at the moment

(***do-not-eval***)
bar1 |> Chart.Show

(*** include-value:bar1 ***)


let bar2 =
    Chart.Bar(keys,values)

(***do-not-eval***)
bar2 |> Chart.Show

(*** include-value:bar2 ***)

(** 

## Stacked bar chart or column charts
The following example shows how to create a stacked bar chart by combining bar charts created by  `Chart.StackedBar` 
*)


let bar3 =
    [
        Chart.StackedBar(values,keys,Name="old");
        Chart.StackedBar([8; 21; 13;],keys,Name="new")
    ]
    |> Chart.Combine

(***do-not-eval***)
bar3 |> Chart.Show

(*** include-value:bar3 ***)


