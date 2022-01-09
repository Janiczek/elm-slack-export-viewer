module Data.Message exposing
    ( Block(..)
    , File
    , FilesMessageData
    , Message(..)
    , Reaction
    , RichTextElement(..)
    , SlackbotMessageData
    , UserMessageData
    , decoder
    , textToBlocks
    )

import Json.Decode as Decode exposing (Decoder)
import Time exposing (Posix)


type Message
    = UserMessage UserMessageData
    | SlackbotMessage SlackbotMessageData
    | FilesMessage FilesMessageData


type alias UserMessageData =
    { blocks : List Block
    , username : String
    , avatarUrl : String
    , reactions : List Reaction
    , timestamp : Posix
    , files : List File
    , wasEdited : Bool -- "edited" field present
    }


type alias FilesMessageData =
    { files : List File
    , reactions : List Reaction
    , timestamp : Posix
    , text : String
    }


type alias SlackbotMessageData =
    { text : String
    , reactions : List Reaction
    , timestamp : Posix
    }


type alias File =
    { name : String
    , mimetype : String
    , size : Int
    , url : String -- url_private

    -- thumbnail
    }


type alias Reaction =
    { emoji : String
    , count : Int

    -- users ID list/set
    }


type Block
    = RichText (List RichTextElement)


type RichTextElement
    = Text String
    | Link { url : String, text : String }
    | InlineCode String -- type=text {style: code}
    | Emoji String
    | Quote (List RichTextElement)
    | Preformatted (List RichTextElement)


{-| This Maybe is a way of being able to filter out "channel\_join" and other messages
-}
decoder : Decoder (Maybe Message)
decoder =
    Decode.oneOf
        [ Decode.field "subtype" Decode.string
            |> Decode.andThen
                (\subtype ->
                    case subtype of
                        "channel_join" ->
                            Decode.succeed Nothing

                        "channel_leave" ->
                            Decode.succeed Nothing

                        "slackbot_response" ->
                            slackbotMessageDecoder
                                |> Decode.map Just

                        _ ->
                            Decode.fail <| "Unknown subtype: '" ++ subtype ++ "'"
                )
        , userMessageDecoder
            |> Decode.map Just
        , filesMessageDecoder
            |> Decode.map Just
        ]


userMessageDecoder : Decoder Message
userMessageDecoder =
    Decode.map7 UserMessageData
        blocksDecoder
        nameDecoder
        (Decode.at [ "user_profile", "image_72" ] Decode.string)
        reactionsDecoder
        timestampDecoder
        filesDecoder
        (Decode.maybe (Decode.field "edited" (Decode.succeed True))
            |> Decode.map (Maybe.withDefault False)
        )
        |> Decode.map UserMessage


nameDecoder : Decoder String
nameDecoder =
    Decode.map5
        (\displayName name realName firstName id ->
            [ displayName
            , name
            , realName
            , firstName
            , id
            ]
                |> List.filter (not << String.isEmpty)
                |> List.head
        )
        (Decode.at [ "user_profile", "display_name" ] Decode.string)
        (Decode.at [ "user_profile", "name" ] Decode.string)
        (Decode.at [ "user_profile", "real_name" ] Decode.string)
        (Decode.at [ "user_profile", "first_name" ] Decode.string)
        (Decode.field "user" Decode.string)
        |> Decode.andThen
            (\name ->
                case name of
                    Nothing ->
                        Decode.fail "Couldn't find a non-empty name!"

                    Just name_ ->
                        Decode.succeed name_
            )


filesMessageDecoder : Decoder Message
filesMessageDecoder =
    Decode.map4 FilesMessageData
        filesDecoder
        reactionsDecoder
        timestampDecoder
        (Decode.field "text" Decode.string)
        |> Decode.map FilesMessage


slackbotMessageDecoder : Decoder Message
slackbotMessageDecoder =
    Decode.map3 SlackbotMessageData
        (Decode.field "text" Decode.string)
        reactionsDecoder
        timestampDecoder
        |> Decode.map SlackbotMessage


timestampDecoder : Decoder Posix
timestampDecoder =
    Decode.field "ts" Decode.string
        |> Decode.andThen
            (\timestampString ->
                case String.toFloat timestampString of
                    Nothing ->
                        Decode.fail "Timestamp wasn't float string!"

                    Just float ->
                        float
                            |> floor
                            |> (*) 1000
                            |> Time.millisToPosix
                            |> Decode.succeed
            )


reactionsDecoder : Decoder (List Reaction)
reactionsDecoder =
    Decode.maybe (Decode.field "reactions" (Decode.list reactionDecoder))
        |> Decode.map (Maybe.withDefault [])


filesDecoder : Decoder (List File)
filesDecoder =
    Decode.maybe (Decode.field "files" (Decode.list fileDecoder))
        |> Decode.map (Maybe.withDefault [])


fileDecoder : Decoder File
fileDecoder =
    Decode.map4 File
        (Decode.field "name" Decode.string)
        (Decode.field "mimetype" Decode.string)
        (Decode.field "size" Decode.int)
        (Decode.field "url_private" Decode.string)


reactionDecoder : Decoder Reaction
reactionDecoder =
    Decode.map2 Reaction
        (Decode.field "name" Decode.string)
        (Decode.field "count" Decode.int)


blocksDecoder : Decoder (List Block)
blocksDecoder =
    {- Either we have "blocks" field with rich text blocks or we have just
       plaintext "text" which we can convert to [RichText [Text string]]
    -}
    Decode.oneOf
        [ Decode.field "blocks" (Decode.list blockDecoder)
        , Decode.field "text" Decode.string
            |> Decode.map textToBlocks
        ]


blockDecoder : Decoder Block
blockDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "rich_text" ->
                        richTextBlockDecoder

                    _ ->
                        Decode.fail <| "Unknown block type: '" ++ type_ ++ "'"
            )


richTextBlockDecoder : Decoder Block
richTextBlockDecoder =
    Decode.field "elements" (Decode.list richTextElementDecoder)
        |> Decode.map (List.concat >> RichText)


richTextElementDecoder : Decoder (List RichTextElement)
richTextElementDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "rich_text_section" ->
                        Decode.field "elements" (Decode.list richTextElementDecoder)
                            |> Decode.map List.concat

                    _ ->
                        Decode.fail <| "Unknown rich text element type: '" ++ type_ ++ "'"
            )


textToBlocks : String -> List Block
textToBlocks text =
    [ RichText [ Text text ] ]
