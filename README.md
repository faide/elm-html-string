# elm-html-string
> Write code resembling elm/html and serialize to either a string or actual
HTML

A fork of [zwilias/elm-html-string](https://github.com/zwilias/elm-html-string)
2.0.2, by Ilias Van Peer, kept under the same BSD-3 license. It renders the
public pages of a site on the server and hands the same views to the browser.

## This fork

- **Attribute values are escaped as HTML**: `&`, `"`, `<` and `>` become
  entities. 2.0.2 wrote a quote as `\"`, which HTML does not read as an
  escape, so a value with a quote ended the attribute early.
- **Attribute names are written as given** (`viewBox` stays `viewBox`), and a
  property is written as its HTML attribute (`readOnly` as `readonly`,
  `acceptCharset` as `accept-charset`).
- **The text of a `script` or a `style` is written as it is**, with no
  entities, and a `</` inside it is written `<\/` so it cannot close the
  element. JSON-LD keeps its `&`.
- **A void element made with `node`** (`node "meta"`, `node "link"`) gets no
  closing tag.
- **Attributes come out in the order written**, `class` and `style` first.
- **`toHtml` hands the browser attributes**, the same ones `toString` writes:
  an Elm program started on HTML written by `toString` finds nothing to change.
  `value`, `checked` and `selected` are set as properties too, but an option's
  `value`.
- **`toString` is about twice as fast** in an interpreter such as QuickJS: one
  string per tag and per text, joined once at the end, and one regular
  expression to find what needs escaping.

It depends on `elm/regex` too, and its tests run on `elm-explorations/test`
2.x.

This package copies the entire API of `elm/html`, but adds 2 functions:

- `toHtml` which serializes the resulting `Html.String.Html msg` node into a
`Html.Html msg` node
- `toString` which serializes the node into a `String` with optional indentation

Two caveats:

- You need to change your imports
- It can't properly support `lazy` nodes since those can't be expressed in Elm

## Show me an example!

```elm
import Html.String as Html exposing (Html)
import Html.String.Attributes as Attr
import Html.String.Events as Events


type Msg = Increment | Decrement


counter : Int -> Html Msg
counter count =
    Html.div
        [ Attr.classList [ ( "below-zero" = count < 0 ), ( "counter" = True ) ] ]
        [ Html.button [ Events.onClick Decrement ] [ Html.text "-" ]
        , Html.text <| toString count
        , Html.button [ Events.onClick Increment ] [ Html.text "+" ]
        ]

counterAsString : String
counterAsString =
    Html.toString 2 <| counter -5

{- Expected output:

    <div class="below-zero counter">
      <button>
        -
      </button>
      -5
      <button>
        +
      </button>
    </div>
-}
```

## Cool, anything else I should be aware of?

Yes.

- Event handlers are removed from the output.
- Void elements ignore children (`br`, `img`, etc.)
- Boolean properties are stripped from the output when `False`, and added as
boolean properties when `True`. I.e. `selected True` will result in
`<someNode selected>`.

## Alright, coolbeans

Made with ❤️ and licensed under BSD-3. Fork me and send me some pull-requests!
