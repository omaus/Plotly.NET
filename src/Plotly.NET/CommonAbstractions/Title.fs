﻿namespace Plotly.NET

open System.Runtime.InteropServices

open DynamicObj

type Title() =
    inherit DynamicObj()

    /// <summary>
    /// Returns a new Title object with the given styles. Note that this is a multi-purpose object that can be used as plot or different axis titles, with some attributes being only used in some layouts.
    /// </summary>
    /// <param name="Text">For all titles: Sets the plot's title. Note that before the existence of `title.text`, the title's contents used to be defined as the `title` attribute itself. This behavior has been deprecated.</param>
    /// <param name="AutoMargin">For plot titles: Determines whether the title can automatically push the figure margins. If `yref='paper'` then the margin will expand to ensure that the title doesn’t overlap with the edges of the container. If `yref='container'` then the margins will ensure that the title doesn’t overlap with the plot area, tick labels, and axis titles. If `automargin=true` and the margins need to be expanded, then y will be set to a default 1 and yanchor will be set to an appropriate default to ensure that minimal margin space is needed. Note that when `yref='paper'`, only 1 or 0 are allowed y values. Invalid values will be reset to the default 1.</param>
    /// <param name="Font">For all titles: Sets the title font. Note that the title's font used to be customized by the now deprecated `titlefont` attribute.</param>
    /// <param name="Standoff">For axis titles: Sets the standoff distance (in px) between the axis labels and the title text The default value is a function of the axis tick labels, the title `font.size` and the axis `linewidth`. Note that the axis title position is always constrained within the margins, so the actual standoff distance is always less than the set or default value. By setting `standoff` and turning on `automargin`, plotly.js will push the margins to fit the axis title at given standoff distance.</param>
    /// <param name="Side">For colorbar titles: Determines the location of color bar's title with respect to the color bar. Defaults to "top" when `orientation` if "v" and defaults to "right" when `orientation` if "h". Note that the title's location used to be set by the now deprecated `titleside` attribute.</param>
    /// <param name="X">For plot titles: Sets the x position with respect to `xref` in normalized coordinates from "0" (left) to "1" (right).</param>
    /// <param name="XAnchor">For plot titles: Sets the title's horizontal alignment with respect to its x position. "left" means that the title starts at x, "right" means that the title ends at x and "center" means that the title's center is at x. "auto" divides `xref` by three and calculates the `xanchor` value automatically based on the value of `x`.</param>
    /// <param name="XRef">For plot titles: Sets the container `x` refers to. "container" spans the entire `width` of the plot. "paper" refers to the width of the plotting area only.</param>
    /// <param name="Y">For plot titles: Sets the y position with respect to `yref` in normalized coordinates from "0" (bottom) to "1" (top). "auto" places the baseline of the title onto the vertical center of the top margin.</param>
    /// <param name="YAnchor">For plot titles: Sets the title's vertical alignment with respect to its y position. "top" means that the title's cap line is at y, "bottom" means that the title's baseline is at y and "middle" means that the title's midline is at y. "auto" divides `yref` by three and calculates the `yanchor` value automatically based on the value of `y`.</param>
    /// <param name="YRef">For plot titles: Sets the container `y` refers to. "container" spans the entire `height` of the plot. "paper" refers to the height of the plotting area only.</param>
    static member init
        (
            [<Optional; DefaultParameterValue(null)>] ?Text: string,
            [<Optional; DefaultParameterValue(null)>] ?AutoMargin: bool,
            [<Optional; DefaultParameterValue(null)>] ?Font: Font,
            [<Optional; DefaultParameterValue(null)>] ?Standoff: int,
            [<Optional; DefaultParameterValue(null)>] ?Side: StyleParam.Side,
            [<Optional; DefaultParameterValue(null)>] ?X: float,
            [<Optional; DefaultParameterValue(null)>] ?XAnchor: StyleParam.XAnchorPosition,
            [<Optional; DefaultParameterValue(null)>] ?XRef: string,
            [<Optional; DefaultParameterValue(null)>] ?Y: float,
            [<Optional; DefaultParameterValue(null)>] ?YAnchor: StyleParam.YAnchorPosition,
            [<Optional; DefaultParameterValue(null)>] ?YRef: string
        ) =
        Title() 
        |> Title.style (
            ?Text = Text, 
            ?AutoMargin = AutoMargin,
            ?Font = Font, 
            ?Standoff = Standoff, 
            ?Side = Side, 
            ?X = X,
            ?XAnchor = XAnchor,
            ?XRef = XRef,
            ?Y = Y,
            ?YAnchor = YAnchor,
            ?YRef = YRef
        )

    /// <summary>
    /// Returns a function that applies the given styles to a Title object. Note that this is a multi-purpose object that can be used as plot or different axis titles, with some attributes being only used in some layouts
    /// </summary>
    /// <param name="Text">For all titles: Sets the plot's title. Note that before the existence of `title.text`, the title's contents used to be defined as the `title` attribute itself. This behavior has been deprecated.</param>
    /// <param name="AutoMargin">For plot titles: Determines whether the title can automatically push the figure margins. If `yref='paper'` then the margin will expand to ensure that the title doesn’t overlap with the edges of the container. If `yref='container'` then the margins will ensure that the title doesn’t overlap with the plot area, tick labels, and axis titles. If `automargin=true` and the margins need to be expanded, then y will be set to a default 1 and yanchor will be set to an appropriate default to ensure that minimal margin space is needed. Note that when `yref='paper'`, only 1 or 0 are allowed y values. Invalid values will be reset to the default 1.</param>
    /// <param name="Font">For all titles: Sets the title font. Note that the title's font used to be customized by the now deprecated `titlefont` attribute.</param>
    /// <param name="Standoff">For axis titles: Sets the standoff distance (in px) between the axis labels and the title text The default value is a function of the axis tick labels, the title `font.size` and the axis `linewidth`. Note that the axis title position is always constrained within the margins, so the actual standoff distance is always less than the set or default value. By setting `standoff` and turning on `automargin`, plotly.js will push the margins to fit the axis title at given standoff distance.</param>
    /// <param name="Side">For colorbar titles: Determines the location of color bar's title with respect to the color bar. Defaults to "top" when `orientation` if "v" and defaults to "right" when `orientation` if "h". Note that the title's location used to be set by the now deprecated `titleside` attribute.</param>
    /// <param name="X">For plot titles: Sets the x position with respect to `xref` in normalized coordinates from "0" (left) to "1" (right).</param>
    /// <param name="XAnchor">For plot titles: Sets the title's horizontal alignment with respect to its x position. "left" means that the title starts at x, "right" means that the title ends at x and "center" means that the title's center is at x. "auto" divides `xref` by three and calculates the `xanchor` value automatically based on the value of `x`.</param>
    /// <param name="XRef">For plot titles: Sets the container `x` refers to. "container" spans the entire `width` of the plot. "paper" refers to the width of the plotting area only.</param>
    /// <param name="Y">For plot titles: Sets the y position with respect to `yref` in normalized coordinates from "0" (bottom) to "1" (top). "auto" places the baseline of the title onto the vertical center of the top margin.</param>
    /// <param name="YAnchor">For plot titles: Sets the title's vertical alignment with respect to its y position. "top" means that the title's cap line is at y, "bottom" means that the title's baseline is at y and "middle" means that the title's midline is at y. "auto" divides `yref` by three and calculates the `yanchor` value automatically based on the value of `y`.</param>
    /// <param name="YRef">For plot titles: Sets the container `y` refers to. "container" spans the entire `height` of the plot. "paper" refers to the height of the plotting area only.</param>
    static member style
        (
            [<Optional; DefaultParameterValue(null)>] ?Text: string,
            [<Optional; DefaultParameterValue(null)>] ?AutoMargin: bool,
            [<Optional; DefaultParameterValue(null)>] ?Font: Font,
            [<Optional; DefaultParameterValue(null)>] ?Standoff: int,
            [<Optional; DefaultParameterValue(null)>] ?Side: StyleParam.Side,
            [<Optional; DefaultParameterValue(null)>] ?X: float,
            [<Optional; DefaultParameterValue(null)>] ?XAnchor: StyleParam.XAnchorPosition,
            [<Optional; DefaultParameterValue(null)>] ?XRef: string,
            [<Optional; DefaultParameterValue(null)>] ?Y: float,
            [<Optional; DefaultParameterValue(null)>] ?YAnchor: StyleParam.YAnchorPosition,
            [<Optional; DefaultParameterValue(null)>] ?YRef: string
        ) =
        (fun (title: Title) ->

            Text |> DynObj.setValueOpt title "text"
            AutoMargin |> DynObj.setValueOpt title "automargin"
            Font |> DynObj.setValueOpt title "font"
            Standoff |> DynObj.setValueOpt title "standoff"
            Side |> DynObj.setValueOptBy title "side" StyleParam.Side.convert
            X |> DynObj.setValueOpt title "x"
            XAnchor |> DynObj.setValueOptBy title "xanchor" StyleParam.XAnchorPosition.convert
            XRef |> DynObj.setValueOpt title "xref"
            Y |> DynObj.setValueOpt title "y"
            YAnchor |> DynObj.setValueOptBy title "yanchor" StyleParam.YAnchorPosition.convert
            YRef |> DynObj.setValueOpt title "yref"

            title)
