module ReactRecycledScrolling

open Feliz
open Fable.Core.JsInterop

let reactRecycledScrolling: obj = importDefault "react-recycled-scrolling"

type IReactRecycledScrollingProperty = interface end

let toProp key value = unbox<IReactProperty>(key ==> value)

type ReactRecycledScrolling =

    static member inline itemHeight (height: int) = toProp "itemHeight" height
    static member inline rowOffset (offset: int) = toProp "rowOffset" offset
    static member inline className (name: string) = toProp "className" name
    static member inline attrList (items: 'item array) = toProp "attrList" items
    static member inline itemFn (render: 'item -> ReactElement) = toProp "itemFn" render
    static member inline create props = Interop.reactApi.createElement (reactRecycledScrolling, createObj !!props)