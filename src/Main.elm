module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Http
import Json.Decode as Decode
import Regex

-- MAIN
main : Program () Model Msg
main =
  Browser.document
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL
type alias Model = { postalCode : String, address : List Address, message : String }
init : () -> (Model, Cmd Msg)
init _ = ({ postalCode = "", address = [], message = "郵便番号を入力してください（ハイフン不要）。" }, Cmd.none)

type alias Address =
    { zipcode : String
    , prefcode : String
    , address1 : String
    , address2 : String
    , address3 : String
    , kana1 : String
    , kana2 : String
    , kana3 : String
    }

type alias Response =
    { status : Int
    , message : Maybe String
    , results : List Address
    }

-- UPDATE
type Msg = UpdatePostalCode String | Submit | GotAddress (Result Http.Error Response)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        UpdatePostalCode newPostalCode ->
            ({ model | postalCode = newPostalCode }, Cmd.none)
        Submit ->
            if validatePostalCode model.postalCode then
                ({ model | message = "検索中" }, getAddress model.postalCode)
            else
                ({ model | address = [], message = "郵便番号が正しくありません。郵便番号を正しく入力してください（ハイフン不要）。" }, Cmd.none)
        GotAddress (Ok response) ->
            ({ model | address = response.results, message = ""}, Cmd.none)
        GotAddress (Err _) ->
            ({ model | address = [], message = "エラーが発生しました。" }, Cmd.none)

validatePostalCode : String -> Bool
validatePostalCode postalCode =
    case Regex.fromString "^[0-9]{7}$" of
        Just regex ->
            Regex.contains regex postalCode
        Nothing ->
            False

-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

-- VIEW
view : { a | postalCode : String, address : List Address, message : String } -> { title : String, body : List (Html Msg) }
view model =
    { title = "郵便番号検索"
    , body = [
        div [] 
            [ h1 [] [ text "郵便番号検索" ]
            , input [ placeholder "1000000", value model.postalCode, onInput UpdatePostalCode ] []
            , button [ onClick Submit ] [ text "検索" ]
            , div [] [ ul [] (List.map addressToHtml model.address) ]
            , div [] [ text model.message ]
            ]
        ]
    }
    

addressToHtml : Address -> Html Msg
addressToHtml address =
    li [] [ text (String.join " " [ address.address1, address.address2, address.address3, "（", address.kana1, address.kana2, address.kana3, "）" ])]

 -- HTTP
getAddress : String -> Cmd Msg
getAddress postalCode =
    Http.get
        { url = "https://zipcloud.ibsnet.co.jp/api/search?zipcode=" ++ postalCode
        , expect = Http.expectJson GotAddress responseDecoder
        }


responseDecoder : Decode.Decoder Response
responseDecoder =
    Decode.map3 Response
        (Decode.field "status" Decode.int)
        (Decode.field "message" (Decode.maybe Decode.string))
        (Decode.field "results" (Decode.list addressDecoder))

addressDecoder : Decode.Decoder Address
addressDecoder =
    Decode.map8 Address
        (Decode.field "zipcode" Decode.string)
        (Decode.field "prefcode" Decode.string)
        (Decode.field "address1" Decode.string)
        (Decode.field "address2" Decode.string)
        (Decode.field "address3" Decode.string)
        (Decode.field "kana1" Decode.string)
        (Decode.field "kana2" Decode.string)
        (Decode.field "kana3" Decode.string)
