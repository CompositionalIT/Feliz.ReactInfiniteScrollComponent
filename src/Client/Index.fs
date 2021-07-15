module Index

open Elmish
open Fable.Remoting.Client
open Shared

type Model = { Todos: Todo list; Input: string }

type Msg =
    | GotTodos of Todo list
    | SetInput of string
    | AddTodo
    | AddedTodo of Todo

let todosApi =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.buildProxy<ITodosApi>

let init () : Model * Cmd<Msg> =
    let model = { Todos = []; Input = "" }

    let cmd =
        Cmd.OfAsync.perform todosApi.getTodos () GotTodos

    model, cmd

let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    match msg with
    | GotTodos todos -> { model with Todos = todos }, Cmd.none
    | SetInput value -> { model with Input = value }, Cmd.none
    | AddTodo ->
        let todo = Todo.create model.Input

        let cmd =
            Cmd.OfAsync.perform todosApi.addTodo todo AddedTodo

        { model with Input = "" }, cmd
    | AddedTodo todo ->
        { model with
              Todos = model.Todos @ [ todo ] },
        Cmd.none

open Feliz
open Feliz.Bulma

let navBrand =
    Bulma.navbarBrand.div [
        Bulma.navbarItem.a [
            prop.href "https://safe-stack.github.io/"
            navbarItem.isActive
            prop.children [
                Html.img [
                    prop.src "/favicon.png"
                    prop.alt "Logo"
                ]
            ]
        ]
    ]

let containerBox (model: Model) (dispatch: Msg -> unit) =
    Bulma.box [
        Bulma.content [
            Html.ol [
                for todo in model.Todos do
                    Html.li [ prop.text todo.Description ]
            ]
        ]
        Bulma.field.div [
            field.isGrouped
            prop.children [
                Bulma.control.p [
                    control.isExpanded
                    prop.children [
                        Bulma.input.text [
                            prop.value model.Input
                            prop.placeholder "What needs to be done?"
                            prop.onChange (fun x -> SetInput x |> dispatch)
                        ]
                    ]
                ]
                Bulma.control.p [
                    Bulma.button.a [
                        color.isPrimary
                        prop.disabled (Todo.isValid model.Input |> not)
                        prop.onClick (fun _ -> dispatch AddTodo)
                        prop.text "Add"
                    ]
                ]
            ]
        ]
    ]

open ReactRecycledScrolling

type Image =
    { Number : int
      Alt : string
      Url : string }

let items =
    [|0..1000|]
    |> Array.map (fun i ->
        { Number = i
          Alt = $"thumbnail of {i}.jpg"
          Url = $"https://picsum.photos/id/{i}/100/100.jpg" })

let render (item : Image) =
    Html.div [
        prop.style [
            style.padding (length.px 10)
            style.position.relative
            style.boxSizing.borderBox;
            style.borderTop (length.px 1, borderStyle.solid, "darkGray")
            style.display.block
            style.textAlign.center
        ]
        prop.children [
            Html.div [ prop.text (sprintf "Image %i" item.Number) ]
            Html.img [
                prop.src item.Url
                prop.alt item.Alt
            ]
        ]
    ]

let view (model: Model) (dispatch: Msg -> unit) =
    ReactRecycledScrolling.create [
        ReactRecycledScrolling.itemFn render
        ReactRecycledScrolling.attrList items
        ReactRecycledScrolling.className "scroller"
        ReactRecycledScrolling.itemHeight 150
    ]
