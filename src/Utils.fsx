#i "nuget: https://api.nuget.org/v3/index.json"
#r "nuget: Deedle"

open Deedle

// Utility functions.

let boolToFloat (value: bool) = if value then 1. else 0.

module FrameExt =
    let modifyCol (column: 'C) (f: 'T0 -> 'T1) (frame: Frame<'R, 'C>) =
        frame
        |> Frame.replaceCol column (frame.Columns.[column] |> Series.mapValues (fun obj -> f (obj :?> 'T0)))

    let oneHot (column: string) (values: 'T0 seq) (frame: Frame<'R, string>) =
        frame
        |> Seq.foldBack
            (fun value frame ->
                let newColumn =
                    if box value :? string then
                        sprintf "%s_%s" column (box value :?> string)
                    else
                        sprintf "%s_%A" column value

                let newColumnValues =
                    frame
                    |> Frame.getCol column
                    |> Series.mapValues (fun obj -> if obj = value then 1. else 0.)

                frame |> Frame.addCol newColumn newColumnValues)
            values
        |> Frame.dropCol column

    let replaceColWith (column: 'C) (f: ObjectSeries<'R> -> ObjectSeries<'R>) (frame: Frame<'R, 'C>) =
        frame |> Frame.replaceCol column (frame.Columns.[column] |> f)

    let addColMissingTag (column: string) (frame: Frame<'R, string>) =
        frame
        |> Frame.addCol (column + "_missing") frame.Columns.[column]
        |> replaceColWith (column + "_missing") (Series.mapAll (fun _ v -> v.IsNone :> obj |> Some) >> ObjectSeries)

    let replaceColMissing (column: 'C) (defaultValue: obj) (frame: Frame<'R, 'C>) =
        frame
        |> replaceColWith
            column
            (Series.mapAll (fun _ -> Option.defaultValue defaultValue >> Some)
             >> ObjectSeries)
