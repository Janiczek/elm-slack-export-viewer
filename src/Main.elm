module Main exposing (main)

import Api exposing (Action(..))
import Browser
import Browser.Dom
import Browser.Navigation
import Data.Channel as Channel exposing (Channel)
import Data.Message as Message
    exposing
        ( Block(..)
        , File
        , FilesMessageData
        , Message(..)
        , Reaction
        , RichTextElement(..)
        , SlackbotMessageData
        , UserMessageData
        )
import Data.YearMonthDay as YearMonthDay exposing (YearMonthDay)
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as Attrs
import Html.Events as Events
import Http exposing (Error(..))
import Json.Decode exposing (Decoder)
import RemoteData exposing (RemoteData(..), WebData)
import Route exposing (Route(..))
import Task
import Time exposing (Posix)
import Url exposing (Url)


type alias Flags =
    ()


type alias Model =
    { navKey : Browser.Navigation.Key
    , route : Route

    --
    , errors : List ( Api.Action, Http.Error )

    --
    , channels : WebData (Dict Channel.Name Channel)
    , channelDays : Dict Channel.Name (WebData (List YearMonthDay))
    , channelLogs : Dict ( Channel.Name, YearMonthDay ) (WebData (List Message))
    }


type Msg
    = UrlChanged Url
    | UrlRequested Browser.UrlRequest
    | ScrollAttempted
    | GotChannels (Result Http.Error (List Channel))
    | GotChannelDays Channel.Name (Result Http.Error (List YearMonthDay))
    | GotChannelLogs Channel.Name YearMonthDay (Result Http.Error (List Message))


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
        route : Route
        route =
            Route.fromUrl url

        initModel : Model
        initModel =
            { navKey = key
            , route = route

            --
            , errors = []

            --
            , channels = NotAsked
            , channelDays = Dict.empty
            , channelLogs = Dict.empty
            }
    in
    ( initModel
    , Cmd.none
    )
        |> andThen (runApiActions (apiActionsForRoute route))



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
                |> andThen (scrollToTopIfChangingDay model.route route)

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

        ScrollAttempted ->
            ( model, Cmd.none )

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

        GotChannelDays channelName (Err err) ->
            { model
                | channelDays =
                    model.channelDays
                        |> Dict.insert channelName (Failure err)
            }
                |> addError (FetchChannelDays channelName) err

        GotChannelDays channelName (Ok days) ->
            ( { model
                | channelDays =
                    model.channelDays
                        |> Dict.insert channelName (Success days)
              }
            , Cmd.none
            )

        GotChannelLogs channelName ymd (Err err) ->
            { model
                | channelLogs =
                    model.channelLogs
                        |> Dict.insert ( channelName, ymd ) (Failure err)
            }
                |> addError (FetchChannelDays channelName) err

        GotChannelLogs channelName ymd (Ok messages) ->
            ( { model
                | channelLogs =
                    model.channelLogs
                        |> Dict.insert ( channelName, ymd ) (Success messages)
              }
            , Cmd.none
            )


scrollToTopIfChangingDay : Route -> Route -> Model -> ( Model, Cmd Msg )
scrollToTopIfChangingDay oldRoute newRoute model =
    case ( oldRoute, newRoute ) of
        ( _, ChannelDayRoute _ _ ) ->
            if oldRoute /= newRoute then
                ( model, scrollToTop )

            else
                ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


scrollToTop : Cmd Msg
scrollToTop =
    Task.perform (\_ -> ScrollAttempted) (Browser.Dom.setViewport 0 0)


addError : Api.Action -> Http.Error -> Model -> ( Model, Cmd Msg )
addError apiAction error model =
    ( { model | errors = ( apiAction, error ) :: model.errors }
    , Cmd.none
    )


apiActionsForRoute : Route -> List Api.Action
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


runApiActions : List Api.Action -> Model -> ( Model, Cmd Msg )
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


runApiAction : Api.Action -> Model -> ( Model, Cmd Msg )
runApiAction apiAction model =
    case apiAction of
        FetchChannels ->
            if canStartLoading model.channels then
                let
                    fetchCmd : Cmd Msg
                    fetchCmd =
                        Api.fetchChannels
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
                        Api.fetchChannelDays channelName
                            |> Cmd.map (GotChannelDays channelName)
                in
                ( { model
                    | channelDays =
                        model.channelDays
                            |> Dict.insert channelName Loading
                  }
                , fetchCmd
                )

            else
                ( model, Cmd.none )

        FetchChannelLogs channelName ymd ->
            let
                channelLogs =
                    model.channelLogs
                        |> Dict.get ( channelName, ymd )
                        |> Maybe.withDefault NotAsked
            in
            if canStartLoading channelLogs then
                let
                    fetchCmd : Cmd Msg
                    fetchCmd =
                        Api.fetchChannelLogs channelName ymd
                            |> Cmd.map (GotChannelLogs channelName ymd)
                in
                ( { model
                    | channelLogs =
                        model.channelLogs
                            |> Dict.insert ( channelName, ymd ) Loading
                  }
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
        [ Html.div
            [ Attrs.class "flex flex-col items-stretch" ]
            [ if List.isEmpty model.errors then
                Html.text ""

              else
                Html.ul
                    [ Attrs.class "h-max-1/2 overflow-y-auto" ]
                    (model.errors
                        |> List.indexedMap
                            (\i ( errorApiAction, httpError ) ->
                                debugView
                                    ("Error " ++ String.fromInt i ++ " (" ++ Debug.toString errorApiAction ++ "):")
                                    httpError
                            )
                    )
            , case model.route of
                ChannelsRoute ->
                    Html.div
                        [ Attrs.class "flex" ]
                        [ channelsView model Nothing
                        ]

                ChannelRoute channelName ->
                    Html.div
                        [ Attrs.class "flex" ]
                        [ channelsView model (Just channelName)
                        , daysView model channelName Nothing
                        ]

                ChannelDayRoute channelName ymd ->
                    Html.div
                        [ Attrs.class "flex" ]
                        [ channelsView model (Just channelName)
                        , daysView model channelName (Just ymd)
                        , messagesView model channelName ymd
                        ]
            ]
        ]
    }


channelsView : Model -> Maybe Channel.Name -> Html Msg
channelsView model currentChannel =
    Html.div
        [ Attrs.class "p-2 bg-sky-50 h-screen max-h-screen overflow-y-scroll" ]
        [ Html.h1
            [ Attrs.class "text-xl font-bold" ]
            [ Html.text "Channels" ]
        , webDataView "Channels" model.channels <|
            \channels ->
                Html.ul []
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
                                        , if currentChannel == Just channel.name then
                                            Attrs.class "font-bold bg-sky-200 hover:bg-sky-300"

                                          else
                                            Attrs.class ""
                                        ]
                                        [ Html.text <| "#" ++ channel.name ]
                                    ]
                            )
                    )
        ]


daysView : Model -> Channel.Name -> Maybe YearMonthDay -> Html Msg
daysView model channelName currentDay =
    Html.div
        [ Attrs.class "p-2 bg-sky-100 h-screen max-h-screen overflow-y-scroll" ]
        [ Html.h1
            [ Attrs.class "text-xl font-bold" ]
            [ Html.text "Days" ]
        , webDataView
            ("Days for channel " ++ channelName)
            (model.channelDays
                |> Dict.get channelName
                |> Maybe.withDefault NotAsked
            )
          <|
            \days ->
                Html.ul
                    [ Attrs.class "text-center" ]
                    (days
                        |> List.map
                            (\ymd ->
                                Html.li
                                    []
                                    [ Html.a
                                        [ Attrs.href <| Route.toUrl <| Route.ChannelDayRoute channelName ymd
                                        , Attrs.class "underline text-sky-600 hover:text-sky-800 hover:bg-sky-200 tabular-nums"
                                        , if currentDay == Just ymd then
                                            Attrs.class "font-bold bg-sky-200 hover:bg-sky-300"

                                          else
                                            Attrs.class ""
                                        ]
                                        [ Html.text <| YearMonthDay.toString ymd ]
                                    ]
                            )
                    )
        ]


messagesView : Model -> Channel.Name -> YearMonthDay -> Html Msg
messagesView model channelName ymd =
    Html.div
        [ Attrs.class "flex-1 p-2 bg-sky-50 h-screen max-h-screen overflow-y-scroll" ]
        [ webDataView
            ("Messages for channel "
                ++ channelName
                ++ " for day "
                ++ YearMonthDay.toString ymd
            )
            (model.channelLogs
                |> Dict.get ( channelName, ymd )
                |> Maybe.withDefault NotAsked
            )
          <|
            \messages ->
                if List.isEmpty messages then
                    Html.text "No messages apart from 'channel_join' and 'channel_leave'!"

                else
                    Html.ul
                        [ Attrs.class "flex flex-col gap-2" ]
                        (List.map messageView messages)
        ]


messageView : Message -> Html Msg
messageView message =
    Html.div
        [ Attrs.class "p-2 rounded bg-orange-100 hover:bg-orange-200" ]
        [ case message of
            UserMessage msg ->
                userMessageView msg

            SlackbotMessage msg ->
                slackbotMessageView msg

            FilesMessage msg ->
                filesMessageView msg
        ]


type alias GenericMessage =
    { blocks : List Block
    , files : List File
    , reactions : List Reaction
    , username : Maybe String
    , avatarUrl : Maybe String
    , timestamp : Posix
    , wasEdited : Bool
    }


genericMessageView : GenericMessage -> Html Msg
genericMessageView message =
    Html.div
        [ Attrs.class "flex items-start gap-2" ]
        [ case message.avatarUrl of
            Nothing ->
                Html.div [ Attrs.class "w-[36px] h-[36px] min-w-[36px] min-h-[36px]" ] []

            Just url ->
                Html.img
                    [ Attrs.src url
                    , Attrs.width 36
                    , Attrs.height 36
                    ]
                    []
        , Html.div
            [ Attrs.class "flex flex-col gap-2" ]
            [ Html.div []
                [ Html.div
                    [ Attrs.class "flex gap-2 items-baseline" ]
                    [ case message.username of
                        Nothing ->
                            Html.text ""

                        Just username ->
                            Html.div
                                [ Attrs.class "font-bold" ]
                                [ Html.text username ]
                    , Html.div
                        [ Attrs.class "text-[12px] text-gray-500" ]
                        [ [ Time.toHour
                          , Time.toMinute
                          ]
                            |> List.map
                                (\fn ->
                                    fn Time.utc message.timestamp
                                        |> String.fromInt
                                        |> String.padLeft 2 '0'
                                )
                            |> String.join ":"
                            |> Html.text
                        ]
                    ]
                , blocksView message.blocks
                , wasEditedView message.wasEdited
                ]
            , filesView message.files
            , reactionsView message.reactions
            ]
        ]


wasEditedView : Bool -> Html Msg
wasEditedView wasEdited =
    if wasEdited then
        Html.div
            [ Attrs.class "text-[12px] text-gray-500 italic mt-2" ]
            [ Html.text "(edited)" ]

    else
        Html.text ""


blocksView : List Block -> Html Msg
blocksView blocks =
    Html.div
        [ Attrs.class "flex flex-row gap-2" ]
        (List.map blockView blocks)


blockView : Block -> Html Msg
blockView block =
    case block of
        RichText elements ->
            Html.div [] (List.map richTextElementView elements)


richTextElementView : RichTextElement -> Html Msg
richTextElementView element =
    case element of
        Text string ->
            Html.text string

        Link { url, text } ->
            Html.a
                [ Attrs.href url ]
                [ Html.text text ]

        Preformatted elements ->
            Html.div
                [ Attrs.class "font-mono text-[14px] px-3 py-2 border-gray-300 border bg-gray-100 rounded whitespace-pre-wrap break-all overflow-x-hidden" ]
                (List.map richTextElementView elements)

        _ ->
            debugView "element" element


filesView : List File -> Html Msg
filesView files =
    Html.div
        [ Attrs.class "flex gap-2" ]
        (List.map fileView files)


fileView : File -> Html Msg
fileView file =
    if String.startsWith "image/" file.mimetype then
        imageFileView file

    else if String.startsWith "video/" file.mimetype then
        videoFileView file

    else
        -- text/, audio/, application/, and who knows what else
        otherFileView file


humanReadableFilesize : Int -> String
humanReadableFilesize bytes =
    let
        withTwoDecimalPlaces : Float -> String
        withTwoDecimalPlaces float =
            (toFloat (floor (float * 100)) / 100)
                |> String.fromFloat
    in
    if bytes < 1024 then
        String.fromInt bytes ++ " B"

    else if bytes < 1048576 then
        withTwoDecimalPlaces (toFloat bytes / 1024) ++ " KB"

    else
        withTwoDecimalPlaces (toFloat bytes / 1048576) ++ " MB"


imageFileView : File -> Html Msg
imageFileView =
    genericFileView
        (\file ->
            Html.div
                [ Attrs.class "max-w-[200px] max-h-[200px]" ]
                [ Html.img
                    [ Attrs.src file.url
                    , Attrs.class "object-contain w-full h-full"
                    ]
                    []
                ]
        )


videoFileView : File -> Html Msg
videoFileView =
    genericFileView
        (\file ->
            Html.div
                [ Attrs.class "max-w-[200px] max-h-[200px]" ]
                [ Html.video
                    [ Attrs.class "object-contain w-full h-full" ]
                    [ Html.source
                        [ Attrs.src file.url
                        , Attrs.type_ file.mimetype
                        ]
                        []
                    ]
                ]
        )


otherFileView : File -> Html Msg
otherFileView =
    genericFileView
        (\file ->
            Html.div
                [ Attrs.class "font-bold text-[14px]" ]
                [ Html.text "File" ]
        )


genericFileView : (File -> Html Msg) -> File -> Html Msg
genericFileView specificFileView file =
    Html.div
        [ Attrs.class "flex flex-col text-[12px] text-gray-600 p-2 border-2 rounded border-orange-300 bg-orange-200 cursor-pointer hover:border-orange-400 hover:bg-orange-300" ]
        [ Html.a
            [ Attrs.href file.url
            , Attrs.target "_blank"
            , Attrs.class "flex flex-col gap-2"
            ]
            [ specificFileView file
            , Html.div []
                [ Html.div [] [ Html.text file.name ]
                , Html.div [] [ Html.text <| "Size: " ++ humanReadableFilesize file.size ]
                ]
            ]
        ]


reactionsView : List Reaction -> Html Msg
reactionsView reactions =
    Html.div
        [ Attrs.class "flex gap-2" ]
        (List.map reactionView reactions)


reactionView : Reaction -> Html Msg
reactionView reaction =
    Html.div
        [ Attrs.class "rounded flex gap-2 bg-orange-200 border-orange-300 border py-1 px-2 text-[12px] hover:border-orange-400 hover:bg-orange-300" ]
        [ Html.div
            [ Attrs.class "font-bold" ]
            [ Html.text <| ":" ++ reaction.emoji ++ ":" ]
        , Html.div [] [ Html.text <| String.fromInt reaction.count ]
        ]


userMessageView : UserMessageData -> Html Msg
userMessageView message =
    genericMessageView
        { blocks = message.blocks
        , files = message.files
        , reactions = message.reactions
        , username = Just message.username
        , avatarUrl = Just message.avatarUrl
        , timestamp = message.timestamp
        , wasEdited = message.wasEdited
        }


slackbotMessageView : SlackbotMessageData -> Html Msg
slackbotMessageView message =
    genericMessageView
        { blocks = Message.textToBlocks message.text
        , files = []
        , reactions = message.reactions
        , username = Just "slackbot"
        , avatarUrl = Nothing
        , timestamp = message.timestamp
        , wasEdited = False
        }


filesMessageView : FilesMessageData -> Html Msg
filesMessageView message =
    genericMessageView
        { blocks = Message.textToBlocks message.text
        , files = message.files
        , reactions = message.reactions
        , username = Nothing
        , avatarUrl = Nothing
        , timestamp = message.timestamp
        , wasEdited = False
        }


codeBlockView : String -> String -> Html msg
codeBlockView label message =
    Html.div
        [ Attrs.class "p-2 flex flex-col gap-2 bg-red-50" ]
        [ Html.text label
        , Html.pre
            [ Attrs.class "p-2 bg-red-100 overflow-x-hidden whitespace-pre-wrap break-all text-[12px]" ]
            [ Html.text message ]
        ]


debugView : String -> a -> Html msg
debugView label value =
    codeBlockView label (Debug.toString value)


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
            codeBlockView
                ("Error loading " ++ label ++ ":")
                (httpErrorToString err)

        Success successData ->
            successView successData
