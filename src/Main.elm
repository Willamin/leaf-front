module Main exposing (..)

import Browser
import Http
import Html exposing (div, text, h1, input, p, button, pre, table, tr, td, th)
import Html.Events exposing (onClick)
import Json.Encode
import Json.Decode exposing (Decoder, field, string, list, succeed)
import Json.Decode.Pipeline exposing (required, optional, hardcoded)


main =
  Browser.element
    { init = init
    , update = update
    , view = view
    , subscriptions = subscriptions
    }

type alias Project =
  { name : String
  , status : String
  }

type alias Model =
  { projects : List Project
  , errors : List Http.Error
  }

init : () -> (Model, Cmd Msg)
init _ =
  ( { projects = []
    , errors = []
    }
  , getProjects
  )

subscriptions _ = Sub.none

type Msg
  = FetchProjects
  | GotProjects (Result Http.Error (List Project))
  | GotProject (Result Http.Error Project)
  | CreateProject

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    FetchProjects ->
      (model, getProjects)
    CreateProject ->
      (model, createProject)
    GotProject _ ->
      (model, getProjects)
    GotProjects result ->
      case result of
        Ok newProjects ->
          ({model | projects = newProjects}, Cmd.none)
        Err e ->
          ( { model
            | projects = []
            , errors = List.append model.errors [e]
            }
          , Cmd.none
          )

getProjects : Cmd Msg
getProjects =
  Http.request
  { method = "GET"
  , headers = []
  , url = "http://localhost:3000/projects"
  , body = Http.emptyBody
  , expect = Http.expectJson GotProjects projectsDecoder
  , timeout = Nothing
  , tracker = Nothing
  }

createProject : Cmd Msg
createProject =
  Http.request
  { method = "POST"
  , headers = []
  , url = "http://localhost:3000/projects"
  , body = Http.stringBody "application/json" "{\"name\":\"Baz\",\"status\":\"unstarted\"}"
  , expect = Http.expectJson GotProject projectDecoder
  , timeout = Nothing
  , tracker = Nothing
  }

projectDecoder : Decoder Project
projectDecoder =
  succeed Project
    |> required "name" string
    |> optional "status" string "unspecified"

projectsDecoder : Decoder (List Project)
projectsDecoder =
  list (projectDecoder)

view : Model -> Html.Html Msg
view model =
  div []
  [ Html.node "style" [] [ text css ]
  , div []
    [ div []
      ( model.errors
        |> List.map
        ( \e ->
          case e of
            Http.BadUrl url -> p [] [ text "The url was invalid" ]
            Http.Timeout -> p [] [ text "There was a network timeout" ]
            Http.NetworkError -> p [] [ text "The connection was lost" ]
            Http.BadStatus code -> p [] [ text ("The request returned " ++ String.fromInt code) ]
            Http.BadBody message -> p [] [ text message ]
        )
      )
    , table []
      ( List.append
        [ tr [] [td [] [pre [] [ text "Name" ] ], td [] [pre [] [ text "Status" ] ] ] ]
        ( model.projects
          |> List.map
          ( \proj ->
            tr []
              [ td [] [ pre [] [ text proj.name ] ]
              , td [] [ pre [] [ text proj.status ] ]
              ]
          )
        )
      )
    , div []
      [ button [ onClick CreateProject ] [ text "Create New Project" ]
      , button [ onClick FetchProjects ] [ text "Refresh Projects" ]
      ]
    ]
  ]

css =
  """
div { margin: 2em 1em; }

input { width: 30em; }

table, td { border: 1px solid black; border-collapse: collapse; }

td { min-width: 5em; padding: 0.2em 0.6em; text-align: left; }
table > tr:first-child > td { background-color: #a8d8a8; }
"""
