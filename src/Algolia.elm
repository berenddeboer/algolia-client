module Algolia
    exposing
        ( Config
        , Msg(..)
        , getSearchResults
        , fetch
        , Output(..)
        )

{-| Helper functions for Algolia clients.


# Helpers

@docs Config, fetch, Msg, getSearchResults, Output

-}

import Algolia.Api as Api exposing (Method, formRqst)
import Http exposing (request, send, expectString, Header, Error(..))


{-| Config
-}
type alias Config =
    { apiKey : String
    , appId : String
    , api : Method
    , extraHeaders : List ( String, String )
    }


{-| Make remote HTTP call
-}
fetch : Config -> Cmd Msg
fetch config =
    let
        rqstRec =
            formRqst config

        rqst =
            request
                { method = rqstRec.httpMethod
                , headers = formatHeaders config
                , url = rqstRec.url
                , body = rqstRec.body
                , expect = expectString
                , timeout = Nothing
                , withCredentials = False
                }
    in
    Http.send Resp rqst


formatHeaders : Config -> List Header
formatHeaders config =
    let
        extraHeaders =
            List.map (\( k, v ) -> Http.header k v) config.extraHeaders

        headers =
            [ Http.header "X-Algolia-Application-Id" config.appId
            , Http.header "X-Algolia-API-Key" config.apiKey
            ]
    in
    headers ++ extraHeaders


{-| -}
type Msg
    = Resp (Result Http.Error String)

{-| -}
type Output
    = FatalError String
      | AllGood String

{-| Receive raw Json String in the Http response and pass it back to be decoded in the Parent module
--}
getSearchResults : Msg -> Output
getSearchResults msg =
    case msg of
        Resp (Ok str) ->
            AllGood str 

        Resp (Err err) ->
            let
                status =
                    case err of
                        BadUrl str ->
                            "Bad url - " ++ str

                        Timeout ->
                            "Remote server did not respond"

                        NetworkError ->
                            "Network error"

                        BadStatus resp ->
                            toString resp.status.code ++ " " ++ resp.status.message

                        BadPayload str resp ->
                            "Bad payload - " ++ str
            in
            FatalError status
