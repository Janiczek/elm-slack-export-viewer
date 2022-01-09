module Main exposing (main)

import Browser
import Browser.Navigation
import Data.Channel as Channel exposing (Channel)
import Data.Message as Message exposing (Message)
import Data.YearMonthDay exposing (YearMonthDay)
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as Attrs
import Html.Events as Events
import Http exposing (Error(..))
import Json.Decode exposing (Decoder)
import RemoteData exposing (RemoteData(..), WebData)
import Route exposing (Route(..))
import Url exposing (Url)


type alias Flags =
    ()


type alias Model =
    { navKey : Browser.Navigation.Key
    , route : Route

    --
    , errors : List ( ApiAction, Http.Error )

    --
    , channels : WebData (Dict Channel.Name Channel) -- channel.id instead?
    , channelDays : Dict Channel.Name (WebData (List YearMonthDay))
    , channelLogs : Dict ( Channel.Name, YearMonthDay ) (WebData (List Message))
    }


type Msg
    = UrlChanged Url
    | UrlRequested Browser.UrlRequest
    | GotChannels (Result Http.Error (List Channel))
    | GotChannelDays Channel.Name (Result Http.Error (List YearMonthDay))
    | GotChannelLogs Channel.Name YearMonthDay (Result Http.Error (List Message))


type ApiAction
    = FetchChannels
    | FetchChannelDays Channel.Name
    | FetchChannelLogs Channel.Name YearMonthDay


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = UrlRequested
        }



-- INIT


init : Flags -> Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init () url key =
    let
        initModel : Model
        initModel =
            { navKey = key
            , route = Route.fromUrl url

            --
            , errors = []

            --
            , channels = NotAsked
            , channelDays = Dict.empty
            }
    in
    ( initModel
    , Cmd.none
    )
        |> andThen (runApiActions [ FetchChannels ])



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlChanged url ->
            let
                route : Route
                route =
                    Route.fromUrl url
            in
            { model | route = route }
                |> runApiActions (apiActionsForRoute route)

        UrlRequested request ->
            case request of
                Browser.Internal url ->
                    ( model
                    , Browser.Navigation.pushUrl model.navKey (Url.toString url)
                    )

                Browser.External url ->
                    ( model
                    , Browser.Navigation.load url
                    )

        GotChannels (Err err) ->
            { model | channels = Failure err }
                |> addError FetchChannels err

        GotChannels (Ok channels) ->
            ( { model
                | channels =
                    channels
                        |> List.map (\ch -> ( ch.name, ch ))
                        |> Dict.fromList
                        |> Success
              }
            , Cmd.none
            )


addError : ApiAction -> Http.Error -> Model -> ( Model, Cmd Msg )
addError apiAction error model =
    ( { model | errors = ( apiAction, error ) :: model.errors }
    , Cmd.none
    )


apiActionsForRoute : Route -> List ApiAction
apiActionsForRoute route =
    case route of
        ChannelsRoute ->
            [ FetchChannels ]

        ChannelRoute channelName ->
            [ FetchChannels
            , FetchChannelDays channelName
            ]

        ChannelDayRoute channelName ymd ->
            [ FetchChannels
            , FetchChannelDays channelName
            , FetchChannelLogs channelName ymd
            ]


runApiActions : List ApiAction -> Model -> ( Model, Cmd Msg )
runApiActions apiActions model =
    List.foldl
        (\action modelAndCmd ->
            modelAndCmd
                |> andThen (runApiAction action)
        )
        ( model, Cmd.none )
        apiActions


andThen :
    (model -> ( model, Cmd msg ))
    -> ( model, Cmd msg )
    -> ( model, Cmd msg )
andThen fn ( model, cmd ) =
    let
        ( newModel, newCmd ) =
            fn model
    in
    ( newModel, Cmd.batch [ cmd, newCmd ] )


runApiAction : ApiAction -> Model -> ( Model, Cmd Msg )
runApiAction apiAction model =
    case apiAction of
        FetchChannels ->
            if canStartLoading model.channels then
                let
                    fetchCmd : Cmd Msg
                    fetchCmd =
                        Channel.fetchChannels
                            |> Cmd.map GotChannels
                in
                ( { model | channels = Loading }
                , fetchCmd
                )

            else
                ( model, Cmd.none )

        FetchChannelDays channelName ->
            let
                channelDays =
                    model.channelDays
                        |> Dict.get channelName
                        |> Maybe.withDefault NotAsked
            in
            if canStartLoading channelDays then
                let
                    fetchCmd : Cmd Msg
                    fetchCmd =
                        Channel.fetchChannelDays
                            |> Cmd.map GotChannelDays
                in
                ( { model | channelDays = Dict.insert channelName Loading model.channelDays }
                , fetchCmd
                )

            else
                ( model, Cmd.none )


canStartLoading : RemoteData x a -> Bool
canStartLoading data =
    case data of
        NotAsked ->
            True

        Loading ->
            False

        Failure _ ->
            True

        Success _ ->
            False



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Elm Slack export viewer"
    , body =
        [ case model.route of
            ChannelsRoute ->
                Html.div
                    [ Attrs.class "flex divide-x-1" ]
                    [ channelsView model
                    ]

            ChannelRoute channelName ->
                Html.div
                    [ Attrs.class "flex divide-x-1" ]
                    [ channelsView model
                    , daysView model channelName
                    ]

            ChannelDayRoute channelName ymd ->
                Html.div
                    [ Attrs.class "flex divide-x-1" ]
                    [ channelsView model
                    , daysView model channelName
                    , messagesView model channelName ymd
                    ]
        ]
    }


channelsView : Model -> Html Msg
channelsView model =
    webDataView "Channels" model.channels <|
        \channels ->
            Html.div
                [ Attrs.class "p-2 bg-sky-50" ]
                [ Html.h1
                    [ Attrs.class "text-xl font-bold" ]
                    [ Html.text "Channels" ]
                , Html.ul []
                    (channels
                        |> Dict.values
                        |> List.sortBy .name
                        |> List.map
                            (\channel ->
                                Html.li
                                    []
                                    [ Html.a
                                        [ Attrs.href <| Route.toUrl <| Route.ChannelRoute channel.name
                                        , Attrs.class "underline text-sky-600 hover:text-sky-800 hover:bg-sky-100"
                                        ]
                                        [ Html.text <| "#" ++ channel.name ]
                                    ]
                            )
                    )
                ]


daysView : Model -> Channel.Name -> Html Msg
daysView model channelName =
    webDataView
        ("Days for channel " ++ channelName)
        (model.channelDays
            |> Dict.get channelName
            |> Maybe.withDefault NotAsked
        )
    <|
        \days ->
            debugView "Days" days


messagesView model channelName ymd =
    Debug.todo "messages view"


debugView : String -> a -> Html msg
debugView label value =
    Html.div
        [ Attrs.class "p-2 flex flex-col gap-2 bg-sky-50" ]
        [ Html.text label
        , Html.pre
            [ Attrs.class "p-2 bg-sky-100 overflow-x-hidden whitespace-pre-wrap break-all" ]
            [ Html.text <| Debug.toString value ]
        ]


httpErrorToString : Http.Error -> String
httpErrorToString err =
    case err of
        BadStatus status ->
            "Bad status: " ++ String.fromInt status

        BadUrl url ->
            "Bad URL: " ++ url

        Timeout ->
            "Timeout"

        NetworkError ->
            "Network error"

        BadBody decoderError ->
            "Bad body: " ++ decoderError


webDataView : String -> WebData a -> (a -> Html msg) -> Html msg
webDataView label data successView =
    case data of
        NotAsked ->
            Html.div
                [ Attrs.class "show-slowly" ]
                [ Html.text <| "Uh... " ++ label ++ " not asked (bug?)" ]

        Loading ->
            Html.div
                [ Attrs.class "show-slowly" ]
                [ Html.text <| "Loading " ++ label ]

        Failure err ->
            Html.text <| "Error loading " ++ label ++ ": " ++ httpErrorToString err

        Success successData ->
            successView successData
