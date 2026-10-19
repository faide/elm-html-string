module Tests exposing
    ( attributeNames
    , attributes
    , booleanProperties
    , classList
    , classes
    , classesFirst
    , escapedAttributes
    , eventsAreStripped
    , indentedOutput
    , nestedIndentation
    , rawText
    , styles
    , testCases
    , textQuotes
    , voidElements
    )

import Expect exposing (Expectation)
import Html.String as Html exposing (Html)
import Html.String.Attributes as Attr
import Html.String.Events as Events
import Test exposing (..)


cases : List ( Html msg, String )
cases =
    [ ( Html.text "hello!", "hello!" )
    , ( Html.span [] [ Html.text "spanned" ], "<span>spanned</span>" )
    , ( Html.div []
            [ Html.text "before"
            , Html.br [] []
            , Html.text "after"
            ]
      , "<div>before<br>after</div>"
      )
    , ( Html.text "&<>", "&amp;&lt;&gt;" )
    ]


testCase : Html msg -> String -> Test
testCase html expected =
    test expected <|
        \_ ->
            html
                |> Html.toString 0
                |> Expect.equal expected


testCases : Test
testCases =
    describe "simple cases" <| List.map (\( input, output ) -> testCase input output) cases


indentedOutput : Test
indentedOutput =
    test "setting indent adds newlines and adds indentation" <|
        \_ ->
            Html.div [] [ Html.text "Hello world!" ]
                |> Html.toString 2
                |> Expect.equal "<div>\n  Hello world!\n</div>"


eventsAreStripped : Test
eventsAreStripped =
    test "if there are eventshandler attached, remove them from the markup" <|
        \_ ->
            Html.button [ Events.onClick 0 ] []
                |> Html.toString 0
                |> Expect.equal "<button></button>"


attributes : Test
attributes =
    test "attributes are rendered as key-value pairs, in the order written" <|
        \_ ->
            Html.input [ Attr.value "hello", Attr.placeholder "hold my place" ] []
                |> Html.toString 0
                |> Expect.equal "<input value=\"hello\" placeholder=\"hold my place\">"


classes : Test
classes =
    test "multiple calls to `class` get joined together" <|
        \_ ->
            Html.span
                [ Attr.class "class-1"
                , Attr.class "class-2"
                ]
                []
                |> Html.toString 0
                |> Expect.equal "<span class=\"class-1 class-2\"></span>"


styles : Test
styles =
    test "styles are serialized to proper css. Sorta." <|
        \_ ->
            Html.span
                [ Attr.style "line-height" "12px"
                , Attr.style "color" "black"
                ]
                []
                |> Html.toString 0
                |> Expect.equal "<span style=\"line-height: 12px; color: black\"></span>"


classList : Test
classList =
    test "classList is converted to a `class`" <|
        \_ ->
            Html.span
                [ Attr.classList
                    [ ( "foo", True )
                    , ( "bar", False )
                    , ( "et-cetera", True )
                    ]
                ]
                []
                |> Html.toString 0
                |> Expect.equal "<span class=\"foo et-cetera\"></span>"


nestedIndentation : Test
nestedIndentation =
    test "nested things stack indentation" <|
        \_ ->
            Html.div [] [ Html.span [] [ Html.text "hi!" ] ]
                |> Html.toString 2
                |> Expect.equal "<div>\n  <span>\n    hi!\n  </span>\n</div>"


classesFirst : Test
classesFirst =
    test "class and style come first, the other attributes in the order written" <|
        \_ ->
            Html.a [ Attr.href "/cart", Attr.style "color" "red", Attr.class "cart", Attr.title "Cart" ] []
                |> Html.toString 0
                |> Expect.equal "<a class=\"cart\" style=\"color: red\" href=\"/cart\" title=\"Cart\"></a>"


escapedAttributes : Test
escapedAttributes =
    describe "an attribute's value is escaped as HTML reads it back"
        [ test "a quote cannot end the attribute" <|
            \_ ->
                Html.img [ Attr.alt "Trail 12\" \" onmouseover=\"alert(1)" ] []
                    |> Html.toString 0
                    |> Expect.equal "<img alt=\"Trail 12&quot; &quot; onmouseover=&quot;alert(1)\">"
        , test "an ampersand and angle brackets" <|
            \_ ->
                Html.a [ Attr.href "/search?q=a&b", Attr.title "<b>" ] []
                    |> Html.toString 0
                    |> Expect.equal "<a href=\"/search?q=a&amp;b\" title=\"&lt;b&gt;\"></a>"
        , test "a class" <|
            \_ ->
                Html.span [ Attr.class "a\"b" ] []
                    |> Html.toString 0
                    |> Expect.equal "<span class=\"a&quot;b\"></span>"
        ]


textQuotes : Test
textQuotes =
    test "a text keeps its quotes" <|
        \_ ->
            Html.p [] [ Html.text "Brisa \"Pro\" & 'Light'" ]
                |> Html.toString 0
                |> Expect.equal "<p>Brisa \"Pro\" &amp; 'Light'</p>"


attributeNames : Test
attributeNames =
    describe "attribute names"
        [ test "an attribute is written with the name given" <|
            \_ ->
                Html.node "svg" [ Attr.attribute "viewBox" "0 0 24 24" ] []
                    |> Html.toString 0
                    |> Expect.equal "<svg viewBox=\"0 0 24 24\"></svg>"
        , test "a property is written as its attribute" <|
            \_ ->
                Html.form [ Attr.acceptCharset "utf-8", Attr.novalidate True ]
                    [ Html.label [ Attr.for "name" ] [] ]
                    |> Html.toString 0
                    |> Expect.equal "<form accept-charset=\"utf-8\" novalidate><label for=\"name\"></label></form>"
        ]


booleanProperties : Test
booleanProperties =
    test "a boolean property is its attribute's name when true, nothing when false" <|
        \_ ->
            Html.input [ Attr.readonly True, Attr.disabled False, Attr.checked True ] []
                |> Html.toString 0
                |> Expect.equal "<input readonly checked>"


rawText : Test
rawText =
    describe "the text of a script or a style is written as it is"
        [ test "no entities in a script" <|
            \_ ->
                Html.node "script" [ Attr.type_ "application/ld+json" ] [ Html.text "{\"name\":\"Black & White\"}" ]
                    |> Html.toString 0
                    |> Expect.equal "<script type=\"application/ld+json\">{\"name\":\"Black & White\"}</script>"
        , test "a script's text cannot close it" <|
            \_ ->
                Html.node "script" [] [ Html.text "var a = '</script><b>';" ]
                    |> Html.toString 0
                    |> Expect.equal "<script>var a = '<\\/script><b>';</script>"
        , test "no entities in a style" <|
            \_ ->
                Html.node "style" [] [ Html.text "a > b { content: \"&\" }" ]
                    |> Html.toString 0
                    |> Expect.equal "<style>a > b { content: \"&\" }</style>"
        ]


voidElements : Test
voidElements =
    describe "void elements"
        [ test "a void element made with node has no closing tag" <|
            \_ ->
                Html.node "meta" [ Attr.attribute "charset" "utf-8" ] []
                    |> Html.toString 0
                    |> Expect.equal "<meta charset=\"utf-8\">"
        , test "an empty element that is not void is closed" <|
            \_ ->
                Html.node "section" [] []
                    |> Html.toString 0
                    |> Expect.equal "<section></section>"
        , test "indented, a void element takes one line" <|
            \_ ->
                Html.div [] [ Html.node "link" [ Attr.rel "stylesheet" ] [], Html.text "after" ]
                    |> Html.toString 2
                    |> Expect.equal "<div>\n  <link rel=\"stylesheet\">\n  after\n</div>"
        ]
