module Data.Message exposing (Message)


type alias Message =
    { blocks : List Block
    , username : String
    , avatarUrl : String
    , reactions : List Reaction
    , files : List File
    , wasEdited : Bool -- "edited" field present
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
