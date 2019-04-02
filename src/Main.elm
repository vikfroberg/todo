module Main exposing (main)

import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Nav
import Browser.Dom as Dom
import Json.Decode as Json exposing (Value)
import Url exposing (Url)
import Html exposing (Attribute, h1, text, div, input, label, node, a)
import Html.Attributes exposing (type_, class, checked, value, id)
import Html.Events exposing (onInput, onClick, on, keyCode)
import Task


type alias Todo =
    { title : String }


type alias Weight =
    Int


type alias Selected =
    Bool


type Item
    = GroupItem Selected Weight String (List Todo)
    | TodoItem Selected Weight Todo


initItems =
    [ TodoItem
        False
        0
        { title = "Not in group task" }
    , GroupItem
        False
        1
        "Lovis"
        [ { title = "Task 1" }
        , { title = "Task 1" }
        ]
    , GroupItem
        False
        2
        "Efter festival"
        [ { title = "Task 1" }
        , { title = "Task 1" }
        ]
    ]


type Modal
    = None
    | Move


type alias Model =
    { key : Nav.Key
    , items : List Item
    , input : String
    , modal : Modal
    }


type Msg
    = Noop
    | CheckedItem Item
    | ClickedClear
    | ClickedMove
    | KeyUp Int
    | Input String
    | Focus (Result Dom.Error ())


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , onUrlChange = onUrlChange
        , onUrlRequest = onUrlRequest
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( { key = key
      , items = initItems
      , input = ""
      , modal = None
      }
    , Task.attempt Focus (Dom.focus "new-todo")
    )


onUrlChange : Url -> Msg
onUrlChange url =
    Noop


onUrlRequest : UrlRequest -> Msg
onUrlRequest req =
    Noop


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model.modal of
        None ->
            case msg of
                Noop ->
                    ( model, Cmd.none )

                Focus result ->
                    case result of
                        Err err ->
                            Debug.todo "Unable to find dom id"

                        Ok () ->
                            ( model, Cmd.none )

                KeyUp key ->
                    let
                        newItems =
                            model.items ++ [ TodoItem False 0 { title = model.input } ]
                    in
                        if key == 13 then
                            ( { model
                                | input = ""
                                , items = newItems
                              }
                            , Cmd.none
                            )
                        else
                            ( model, Cmd.none )

                Input text ->
                    ( { model | input = text }, Cmd.none )

                ClickedClear ->
                    ( { model
                        | items = List.filter (not << itemIsSelected) model.items
                      }
                    , Cmd.none
                    )

                ClickedMove ->
                    ( { model | modal = Move }
                    , Cmd.none
                    )

                CheckedItem item ->
                    let
                        newItems =
                            List.map
                                (\item_ ->
                                    if item_ == item then
                                        itemToggleSelected item_
                                    else
                                        item_
                                )
                                model.items
                    in
                        ( { model | items = newItems }, Cmd.none )

        Move ->
            case msg of
                ClickedMove ->
                    ( { model | modal = None }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )


itemIsGroup item =
    case item of
        GroupItem selected weight title todos ->
            True

        TodoItem selected weight todo ->
            False


itemIsSelected item =
    case item of
        GroupItem selected weight title todos ->
            selected

        TodoItem selected weight todo ->
            selected


itemToggleSelected item =
    case item of
        GroupItem selected weight title todos ->
            GroupItem (not selected) weight title todos

        TodoItem selected weight todo ->
            TodoItem (not selected) weight todo


itemDeselect item =
    case item of
        GroupItem selected weight title todos ->
            GroupItem False weight title todos

        TodoItem selected weight todo ->
            TodoItem False weight todo


view : Model -> Document Msg
view model =
    { title = "Clear"
    , body =
        [ node "style" [] [ text reset ]
        , node "style" [] [ text css ]
        , h1 [ class "page-title" ] [ text "Clear" ]
        , div [ class "action-bar" ]
            [ clearLink (List.filter itemIsSelected model.items)
            , moveLink (List.filter itemIsSelected model.items)
            ]
        , input
            [ type_ "text"
            , class "new-todo"
            , onKeyUp KeyUp
            , onInput Input
            , value model.input
            , id "new-todo"
            ]
            []
        , div [] (itemsView model.items)
        , modalView model.modal model.items
        ]
    }


modalView modal items =
    case modal of
        None ->
            div [] []

        Move ->
            let
                notSelectedItems =
                    List.filter (not << itemIsSelected) items

                groups =
                    List.filter itemIsGroup notSelectedItems
            in
                div [] (List.map itemMoveView groups)


itemMoveView item =
    case item of
        TodoItem selected weight todo ->
            Debug.todo "should not happen"

        GroupItem selected weight title todos ->
            div [] [ text title ]


clearLink selected =
    let
        string =
            case selected of
                [] ->
                    "Clear (0)"

                items ->
                    "Clear (" ++ String.fromInt (List.length selected) ++ ")"
    in
        a [ class "link", onClick ClickedClear ] [ text string ]


moveLink selected =
    let
        string =
            case selected of
                [] ->
                    "Move (0)"

                items ->
                    "Move (" ++ String.fromInt (List.length selected) ++ ")"
    in
        a [ class "link", onClick ClickedMove ] [ text string ]


itemsView items =
    items |> List.map itemView


itemView item =
    case item of
        TodoItem selected weight todo ->
            label
                [ class "todo--wrapper" ]
                [ input
                    [ type_ "checkbox"
                    , checked selected
                    , onClick (CheckedItem item)
                    ]
                    []
                , text todo.title
                ]

        GroupItem selected weight title todos ->
            label
                [ class "todo--wrapper" ]
                [ input
                    [ type_ "checkbox"
                    , checked selected
                    , onClick (CheckedItem item)
                    ]
                    []
                , text title
                ]


onKeyUp : (Int -> msg) -> Attribute msg
onKeyUp tagger =
    on "keyup" (Json.map tagger keyCode)


css =
    """
    .page-title { font-size: 24px; font-weight: bold; }

    .todo--wrapper { display: block; }

    .link { color: red; cursor: pointer; }

    .action-bar { padding: 10px }
    .action-bar > * { margin-right: 10px }

    .new-todo { border: 1px solid #ccc; }
    """


reset =
    """
    article, aside, audio, command, datagrid, details, dialog, embed,
    figcaption, figure, footer, header, hgroup, menu, nav, section, summary,
    video, wbr {
      display: block;
    }

    bdi, figcaption, keygen, mark, meter, progress, rp, rt, ruby, time {
      display: inline;
    }

    acronym, applet, big, center, dir, font, frame, frameset, noframes, s,
    strike, tt, u, xmp {
      display: none;
    }

    a, abbr, area, article, aside, audio, b, bdo, blockquote, body, button,
    canvas, caption, cite, code, col, colgroup, command, datalist, dd, del,
    details, dialog, dfn, div, dl, dt, em, embed, fieldset, figure, form,
    h1, h2, h3, h4, h5, h6, head, header, hgroup, hr, html, i, iframe, img,
    input, ins, keygen, kbd, label, legend, li, map, mark, menu, meter, nav,
    noscript, object, ol, optgroup, option, output, p, param, pre, progress,
    q, rp, rt, ruby, samp, section, select, small, span, strong, sub, sup,
    table, tbody, td, textarea, tfoot, th, thead, time, tr, ul, var, video {
      background: transparent;
      border: 0;
      font-size: 100%;
      font: inherit;
      margin: 0;
      outline: none;
      padding: 0;
      text-align: left;
      text-decoration: none;
      vertical-align: baseline;
      z-index: 1;
    }

    body {
      line-height: 1;
    }

    ol, ul {
      list-style: none;
    }

    blockquote, q {
      quotes: none;

    }

    blockquote:before, blockquote:after, q:before, q:after {
      content: '';
      content: none;
    }

    table {
      border-collapse: collapse;
      border-spacing: 0;
    }
    """
