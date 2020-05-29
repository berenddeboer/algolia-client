module SimpleExample exposing (..)

import Algolia
import Algolia.Api as Api exposing (..)
import Browser
import Html as Html exposing (..)
import Html.Attributes exposing (class, rows, disabled, readonly, src, style)
import Html.Events exposing (onInput)
import Json.Decode exposing (succeed, list, bool, int, string)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode
import RemoteData exposing (RemoteData(..), WebData)


main : Program Json.Decode.Value Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias AlgoliaResponse = Algolia.Response Actor

type alias Model =
    { searchString : String
    , algoliaResponse : WebData AlgoliaResponse
    }


type Msg
    = NoOp
    | InputString String
    | AlgoliaResults (WebData AlgoliaResponse)


subscriptions : Model -> Sub msg
subscriptions model =
    Sub.none


init : Json.Decode.Value -> ( Model, Cmd msg )
init flags =
    ( { searchString = ""
      , algoliaResponse = NotAsked
      }
    , Cmd.none
    )


minCharsToStartSearch : Int
minCharsToStartSearch =
    3

{-- View
--}
view : Model -> Html Msg
view model =
    div []
        [ section [ class "hero is-info" ]
            [ div [ class "hero-body" ]
                [ h1 [ class "title" ]
                    [ text "Algolia instasearch with Elm" ]
                ]
            ]
        , section [ class "section" ]
            [ div [ class "columns" ]
                [ div [ class "container column is-half" ]
                    [ label [ class "label" ]
                        [ text "Search for actors e.g 'robert', 'robn' (intentional typo), 'julia', 'ben'"]
                    , input [ class "input", onInput InputString ] []
                    , displayResults model.algoliaResponse
                    ]
                ]
            ]
        ]

{-- Update
--}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        InputString str ->
            if String.length str >= minCharsToStartSearch then
                ( model, Algolia.fetch (config str) AlgoliaResults decodeActor )
            else
                ( { model | algoliaResponse = NotAsked }, Cmd.none )

        AlgoliaResults response ->
            ( { model | algoliaResponse = response }, Cmd.none )


{-- Config and query options for Algolia.
--}
config : String -> Algolia.Config
config str =
    { apiKey = "6be0576ff61c053d5f9a3225e2a90f76"
    , appId = "latency"
    , api = SearchAnIndex (searchOptions str)
    , extraHeaders = []
    }


searchOptions : String -> Api.SearchRecord
searchOptions str =
    { indexName = "actors"
    , params =
        Just
            [ TypoTolerance TypoToleranceMin
            , HitsPerPage 10
            , Query str
            ]
    }


{-- View helpers for the search results
--}

displayResults : WebData AlgoliaResponse -> Html Msg
displayResults response =
    case response of
        NotAsked -> text "Initialising."

        Loading -> text "Loading."

        Failure err -> text "Error talking to Algolia."

        Success results -> doDisplayResults results


doDisplayResults : AlgoliaResponse -> Html Msg
doDisplayResults a =
    ul [ class "section" ]
        (List.map (displayActor a.query) a.hits)


displayActor : String -> Actor -> Html Msg
displayActor query actor =
    li [ class "container is-fluid" ]
        [ div [ class "box columns" ]
            [ div [ class "column" ]
                [ img [ src ("https://image.tmdb.org/t/p/w154" ++ actor.image_path) ]
                    []
                ]
            , div [ class "column" ]
                (highlightedName actor.highlightResult)
            ]
        , br [] []
        ]


highlightedName : Actor_highlightResult -> List (Html Msg)
highlightedName result =
    let
        nameHtml =
            case result.name of
                Just struct ->
                    formatHighlight struct.value

                Nothing ->
                    []

        altNameHtml =
            case result.alternative_name of
                Just struct ->
                    formatHighlight struct.value

                Nothing ->
                    []
    in
    [ p [ class "subtitle" ] nameHtml
    , p [] altNameHtml
    ]


formatHighlight : String -> List (Html Msg)
formatHighlight str =
    let
        pre =
            String.split "<em>" str |> List.head |> Maybe.withDefault ""

        post =
            String.split "</em>" str |> List.reverse |> List.head |> Maybe.withDefault ""

        highlight =
            String.split "<em>" str
                |> List.reverse
                |> List.head
                |> Maybe.withDefault ""
                |> String.split "</em>"
                |> List.head
                |> Maybe.withDefault ""
    in
    [ span [] [ Html.text pre ]
    , span [ style "background-color" "#3273DC", style "color" "white" ] [ Html.text highlight ]
    , span [] [ Html.text post ]
    ]


{-- Now start the chunk of decoder code.
--}

type alias Actor =
    { name : String
    , rating : Int
    , image_path : String
    , alternative_name : Maybe String
    , objectID : String
    , highlightResult : Actor_highlightResult
    }


type alias HighlightResult =
    { value : String
    , matchLevel : String
    , fullyHighlighted : Maybe Bool
    , matchedWords : Maybe (List String)
    }


type alias Actor_highlightResult =
    { name : Maybe HighlightResult
    , alternative_name : Maybe HighlightResult
    }



decodeActor : Json.Decode.Decoder Actor
decodeActor =
    Json.Decode.succeed Actor
        |> required "name" string
        |> required "rating" int
        |> required "image_path" string
        |> required "alternative_name" (Json.Decode.nullable string)
        |> required "objectID" string
        |> required "_highlightResult" decodeActor_highlightResult


decode_highlightResult : Json.Decode.Decoder HighlightResult
decode_highlightResult =
    Json.Decode.succeed HighlightResult
        |> required "value" string
        |> required "matchLevel" string
        |> Json.Decode.Pipeline.optional "fullyHighlighted" (Json.Decode.nullable bool) Nothing
        |> Json.Decode.Pipeline.optional "matchedWords" (Json.Decode.nullable (Json.Decode.list string)) Nothing


decodeActor_highlightResult : Json.Decode.Decoder Actor_highlightResult
decodeActor_highlightResult =
    Json.Decode.succeed Actor_highlightResult
        |> required "name" (Json.Decode.nullable decode_highlightResult)
        |> Json.Decode.Pipeline.optional "alternative_name" (Json.Decode.nullable decode_highlightResult) (Just defaultHighlightResult)


defaultHighlightResult : HighlightResult
defaultHighlightResult =
    { value = ""
    , matchLevel = "none"
    , fullyHighlighted = Nothing
    , matchedWords = Nothing
    }
