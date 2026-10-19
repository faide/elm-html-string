module Html.Types exposing
    ( Attribute(..)
    , Children(..)
    , EventDecoder(..)
    , Html(..)
    , map
    , mapAttribute
    , toHtml
    , toString
    )

import Html
import Html.Attributes
import Html.Events
import Html.Keyed
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Encode as Encode
import Regex exposing (Regex)
import String.Conversions


type Children msg
    = NoChildren
    | Regular (List (Html msg))
    | Keyed (List ( String, Html msg ))


mapChildren : (a -> b) -> Children a -> Children b
mapChildren f children =
    case children of
        NoChildren ->
            NoChildren

        Regular nodes ->
            Regular (List.map (map f) nodes)

        Keyed keyedNodes ->
            Keyed (List.map (Tuple.mapSecond <| map f) keyedNodes)


type Html msg
    = Node String (List (Attribute msg)) (Children msg)
    | TextNode String


map : (a -> b) -> Html a -> Html b
map f node =
    case node of
        Node tagName attrs children ->
            Node tagName (List.map (mapAttribute f) attrs) (mapChildren f children)

        TextNode content ->
            TextNode content


type Attribute msg
    = Attribute String String
    | StringProperty String String
    | BoolProperty String Bool
    | ValueProperty String Value
    | Style String String
    | Event String (EventDecoder msg)


type EventDecoder msg
    = Normal (Decoder msg)
    | MayStopPropagation (Decoder ( msg, Bool ))
    | MayPreventDefault (Decoder ( msg, Bool ))
    | Custom (Decoder { message : msg, stopPropagation : Bool, preventDefault : Bool })


mapAttribute : (a -> b) -> Attribute a -> Attribute b
mapAttribute f attribute =
    case attribute of
        Attribute key value ->
            Attribute key value

        StringProperty key value ->
            StringProperty key value

        BoolProperty key value ->
            BoolProperty key value

        ValueProperty key value ->
            ValueProperty key value

        Style key value ->
            Style key value

        Event name eventDecoder ->
            Event name (mapEventDecoder f eventDecoder)


mapEventDecoder : (a -> b) -> EventDecoder a -> EventDecoder b
mapEventDecoder f eventDecoder =
    case eventDecoder of
        Normal d ->
            Normal (Decode.map f d)

        MayStopPropagation d ->
            MayStopPropagation (Decode.map (Tuple.mapFirst f) d)

        MayPreventDefault d ->
            MayPreventDefault (Decode.map (Tuple.mapFirst f) d)

        Custom d ->
            Custom
                (Decode.map
                    (\v ->
                        { message = f v.message
                        , stopPropagation = v.stopPropagation
                        , preventDefault = v.preventDefault
                        }
                    )
                    d
                )



-- TO HTML


toHtml : Html msg -> Html.Html msg
toHtml node =
    case node of
        Node tagName attributes children ->
            case children of
                NoChildren ->
                    Html.node tagName (List.concatMap (attributeToHtml tagName) attributes) []

                Regular nodes ->
                    Html.node tagName (List.concatMap (attributeToHtml tagName) attributes) (List.map toHtml nodes)

                Keyed keyedNodes ->
                    Html.Keyed.node tagName (List.concatMap (attributeToHtml tagName) attributes) (List.map (Tuple.mapSecond toHtml) keyedNodes)

        TextNode content ->
            Html.text content


{-| An attribute as the browser's Elm sees it: the same attribute `toString` writes, so a program
started on HTML written by `toString` finds nothing to change. Elm reads existing HTML back as
attributes, so a property standing in for one (`className` for `class`) would be patched on start.

The properties holding a form's live state (`value`, `checked`, `selected`) are set as properties
too, or the page would show the first value after somebody changed it; but not an option's `value`,
a property that writes its attribute back.

-}
attributeToHtml : String -> Attribute msg -> List (Html.Attribute msg)
attributeToHtml tagName attribute =
    case attribute of
        Attribute key value ->
            [ Html.Attributes.attribute key value ]

        StringProperty key value ->
            if isLive key && not (tagName == "option" && key == "value") then
                [ Html.Attributes.attribute (attributeName key) value, Html.Attributes.property key (Encode.string value) ]

            else
                [ Html.Attributes.attribute (attributeName key) value ]

        BoolProperty key value ->
            let
                present =
                    if value then
                        [ Html.Attributes.attribute (attributeName key) "" ]

                    else
                        []
            in
            if isLive key then
                Html.Attributes.property key (Encode.bool value) :: present

            else
                present

        ValueProperty key value ->
            [ Html.Attributes.property key value ]

        Style key value ->
            [ Html.Attributes.style key value ]

        Event name (Normal d) ->
            [ Html.Events.on name d ]

        Event name (MayStopPropagation d) ->
            [ Html.Events.stopPropagationOn name d ]

        Event name (MayPreventDefault d) ->
            [ Html.Events.preventDefaultOn name d ]

        Event name (Custom d) ->
            [ Html.Events.custom name d ]


isLive : String -> Bool
isLive key =
    key == "value" || key == "checked" || key == "selected"



-- TO STRING


{-| The HTML of a node: on one line with an indent of 0, or a node per line, indented by `indent`
spaces per level.

Each tag and each text is one string, gathered in a list, last first, and the list is joined once at
the end: joining the page piece by piece would copy it over and over. The tags themselves are built
with `++`, a single call where adding to a list costs three, which counts in an interpreter.

-}
toString : Int -> Html msg -> String
toString indent html =
    if indent <= 0 then
        written [ html ] [] []
            |> List.reverse
            |> String.concat

    else
        lines (String.repeat indent " ") "" [ html ] [] []
            |> List.reverse
            |> String.join "\n"


{-| The tags still open, each with the nodes that follow it.
-}
type alias Open msg =
    List ( String, List (Html msg) )


written : List (Html msg) -> Open msg -> List String -> List String
written nodes open out =
    case nodes of
        [] ->
            case open of
                [] ->
                    out

                ( tagName, after ) :: outer ->
                    written after outer (("</" ++ tagName ++ ">") :: out)

        (TextNode text) :: rest ->
            written rest open (textIn open text :: out)

        (Node tagName attributes children) :: rest ->
            case children of
                NoChildren ->
                    written rest open (startTag tagName attributes :: out)

                Regular [] ->
                    if isVoid tagName then
                        written rest open (startTag tagName attributes :: out)

                    else
                        written rest open ((startTag tagName attributes ++ "</" ++ tagName ++ ">") :: out)

                Regular inner ->
                    written inner (( tagName, rest ) :: open) (startTag tagName attributes :: out)

                Keyed inner ->
                    written (List.map Tuple.second inner) (( tagName, rest ) :: open) (startTag tagName attributes :: out)


{-| A node per line, indented by its depth.
-}
lines : String -> String -> List (Html msg) -> Open msg -> List String -> List String
lines unit indentation nodes open out =
    case nodes of
        [] ->
            case open of
                [] ->
                    out

                ( tagName, after ) :: outer ->
                    let
                        outerIndentation =
                            String.dropLeft (String.length unit) indentation
                    in
                    lines unit outerIndentation after outer ((outerIndentation ++ "</" ++ tagName ++ ">") :: out)

        (TextNode text) :: rest ->
            lines unit indentation rest open ((indentation ++ textIn open text) :: out)

        (Node tagName attributes children) :: rest ->
            let
                start =
                    indentation ++ startTag tagName attributes
            in
            case children of
                NoChildren ->
                    lines unit indentation rest open (start :: out)

                Regular [] ->
                    if isVoid tagName then
                        lines unit indentation rest open (start :: out)

                    else
                        lines unit indentation rest open ((indentation ++ "</" ++ tagName ++ ">") :: start :: out)

                Regular inner ->
                    lines unit (indentation ++ unit) inner (( tagName, rest ) :: open) (start :: out)

                Keyed inner ->
                    lines unit (indentation ++ unit) (List.map Tuple.second inner) (( tagName, rest ) :: open) (start :: out)


{-| A text as its parent element reads it: escaped, but in a `script` or a `style`, whose text the
browser reads as it is, where only a `</` that would close the element is guarded.
-}
textIn : Open msg -> String -> String
textIn open text =
    case open of
        ( parent, _ ) :: _ ->
            if parent == "script" || parent == "style" then
                String.replace "</" "<\\/" text

            else
                escapeText text

        [] ->
            escapeText text


{-| The elements HTML closes on their own: no closing tag is written for them.
-}
isVoid : String -> Bool
isVoid tagName =
    case tagName of
        "area" ->
            True

        "base" ->
            True

        "br" ->
            True

        "col" ->
            True

        "embed" ->
            True

        "hr" ->
            True

        "img" ->
            True

        "input" ->
            True

        "link" ->
            True

        "meta" ->
            True

        "source" ->
            True

        "track" ->
            True

        "wbr" ->
            True

        _ ->
            False


{-| A start tag: `class` and `style` first, each once with every value given, then the other
attributes in the order written. Events write nothing.
-}
startTag : String -> List (Attribute msg) -> String
startTag tagName attributes =
    case attributes of
        [] ->
            "<" ++ tagName ++ ">"

        _ ->
            let
                ( classes, styles ) =
                    classesAndStyles attributes [] []
            in
            "<"
                ++ tagName
                ++ joinedAttribute " class=\"" " " classes
                ++ joinedAttribute " style=\"" "; " styles
                ++ otherAttributes attributes ""
                ++ ">"


{-| An attribute given several times, its values joined; nothing when it is not given.
-}
joinedAttribute : String -> String -> List String -> String
joinedAttribute opening separator values =
    case values of
        [] ->
            ""

        [ value ] ->
            opening ++ escape value ++ "\""

        _ ->
            opening ++ escape (String.join separator (List.reverse values)) ++ "\""


{-| The values of every `class` and `style` among the attributes, last first.
-}
classesAndStyles : List (Attribute msg) -> List String -> List String -> ( List String, List String )
classesAndStyles attributes classes styles =
    case attributes of
        [] ->
            ( classes, styles )

        (StringProperty "className" value) :: rest ->
            classesAndStyles rest (value :: classes) styles

        (Style key value) :: rest ->
            classesAndStyles rest classes ((key ++ ": " ++ value) :: styles)

        _ :: rest ->
            classesAndStyles rest classes styles


otherAttributes : List (Attribute msg) -> String -> String
otherAttributes attributes done =
    case attributes of
        [] ->
            done

        (Attribute key value) :: rest ->
            otherAttributes rest (done ++ " " ++ key ++ "=\"" ++ escape value ++ "\"")

        (StringProperty "className" _) :: rest ->
            otherAttributes rest done

        (StringProperty key value) :: rest ->
            otherAttributes rest (done ++ " " ++ attributeName key ++ "=\"" ++ escape value ++ "\"")

        (BoolProperty key True) :: rest ->
            otherAttributes rest (done ++ " " ++ attributeName key)

        (BoolProperty _ False) :: rest ->
            otherAttributes rest done

        (ValueProperty key value) :: rest ->
            otherAttributes rest (done ++ " " ++ attributeName key ++ "=\"" ++ escape (String.Conversions.fromValue value) ++ "\"")

        (Style _ _) :: rest ->
            otherAttributes rest done

        (Event _ _) :: rest ->
            otherAttributes rest done


{-| The attribute a property is written as: its HTML name. Every property `Html.String.Attributes`
sets is named as its attribute but these.
-}
attributeName : String -> String
attributeName property =
    case property of
        "className" ->
            "class"

        "htmlFor" ->
            "for"

        "defaultValue" ->
            "value"

        "acceptCharset" ->
            "accept-charset"

        "accessKey" ->
            "accesskey"

        "contentEditable" ->
            "contenteditable"

        "isMap" ->
            "ismap"

        "noValidate" ->
            "novalidate"

        "readOnly" ->
            "readonly"

        "useMap" ->
            "usemap"

        _ ->
            property


{-| An attribute's value as HTML reads it back: `&`, `"`, `<` and `>` as entities. Most values
have none, so one search comes before any replacing.
-}
escape : String -> String
escape value =
    if Regex.contains special value then
        Regex.replace special entity value

    else
        value


{-| A text as HTML reads it back: `&`, `<` and `>` as entities, quotes as they are.
-}
escapeText : String -> String
escapeText text =
    if Regex.contains textSpecial text then
        Regex.replace textSpecial entity text

    else
        text


special : Regex
special =
    Regex.fromString "[&\"<>]" |> Maybe.withDefault Regex.never


textSpecial : Regex
textSpecial =
    Regex.fromString "[&<>]" |> Maybe.withDefault Regex.never


entity : Regex.Match -> String
entity found =
    case found.match of
        "&" ->
            "&amp;"

        "\"" ->
            "&quot;"

        "<" ->
            "&lt;"

        _ ->
            "&gt;"
