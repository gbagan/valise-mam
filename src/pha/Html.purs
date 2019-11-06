module Pha.Html where

import Prelude
import Pha (VDom, Prop(..), h, text)
import Pha.Action (Action)

key :: forall a effs. String -> Prop a effs
key = Key

attr :: forall a effs. String -> String -> Prop a effs
attr = Attr

class' :: forall a effs. String -> Boolean -> Prop a effs
class' = Class

style :: forall a effs. String -> String -> Prop a effs
style = Style

click :: forall a effs. Action a effs -> Prop a effs
click = Event "click"

contextmenu :: forall a effs. Action a effs -> Prop a effs
contextmenu = Event "contextmenu"

pointermove :: forall a effs. Action a effs -> Prop a effs
pointermove = Event "pointermove"

pointerup :: forall a effs. Action a effs -> Prop a effs
pointerup = Event "pointerup"

pointerdown :: forall a effs. Action a effs -> Prop a effs
pointerdown = Event "pointerdown"

pointerenter :: forall a effs. Action a effs -> Prop a effs
pointerenter = Event "pointerenter"

pointerleave :: forall a effs. Action a effs -> Prop a effs
pointerleave = Event "pointerleave"

keydown :: forall a effs. Action a effs -> Prop a effs
keydown = Event "keydown"

-- elements

br :: forall a effs. VDom a effs
br = h "br" [] []

button :: forall a effs. Array (Prop a effs) -> Array (VDom a effs) -> VDom a effs
button = h "button"

div' :: forall a effs. Array (Prop a effs) -> Array (VDom a effs) -> VDom a effs
div' = h "div"

span :: forall a effs. Array (Prop a effs) -> Array (VDom a effs) -> VDom a effs
span = h "span"

h2 :: forall a effs. Array (Prop a effs) -> Array (VDom a effs) -> VDom a effs
h2 = h "h2"

p :: forall a effs. Array (Prop a effs) -> Array (VDom a effs) -> VDom a effs
p = h "p"

a :: forall a effs. Array (Prop a effs) -> Array (VDom a effs) -> VDom a effs
a = h "a"

-- attributes

disabled :: forall a effs. Boolean -> Prop a effs
disabled b = attr "disabled" (if b then "true" else "")
width :: forall a effs. String -> Prop a effs
width = attr "width"
height :: forall a effs. String -> Prop a effs
height = attr "height"
href :: forall a effs. String -> Prop a effs
href = attr "href"

    -- svg
x :: forall a effs. String -> Prop a effs
x = attr "x"
y :: forall a effs. String -> Prop a effs
y = attr "y" 
stroke :: forall a effs. String -> Prop a effs
stroke = attr "stroke"
fill :: forall a effs. String -> Prop a effs
fill = attr "fill"
viewBox :: forall a effs. Int -> Int -> Int -> Int -> Prop a effs
viewBox a b c d = attr "viewBox" $ show a <> " " <> show b <> " " <> show c <> " " <> show d
transform :: forall a effs. String -> Prop a effs
transform = attr "transform"
strokeWidth :: forall a effs. String -> Prop a effs
strokeWidth = attr "stroke-width"
strokeDasharray :: forall a effs. String -> Prop a effs
strokeDasharray = attr "stroke-dasharray"


g :: forall a effs. Array (Prop a effs) -> Array (VDom a effs) -> VDom a effs
g = h "g"

svg :: forall a effs. Array (Prop a effs) -> Array (VDom a effs) -> VDom a effs
svg = h "svg"

rect :: forall a effs. Number -> Number -> Number -> Number -> Array (Prop a effs) -> VDom a effs
rect x' y' w h' props = h "rect" ([attr "x" $ show x', attr "y" $ show y', attr "width" $ show w, attr "height" $ show h'] <> props) []

path :: forall a effs. String -> Array (Prop a effs) -> VDom a effs
path d props = h "path" ([attr "d" d] <> props) []

line :: forall a effs. Number -> Number -> Number -> Number -> Array (Prop a effs) -> VDom a effs
line x1 y1 x2 y2 props = h "line" ([attr "x1" $ show x1, attr "y1" $ show y1, attr "x2" $ show x2, attr "y2" $ show y2] <> props) []

circle :: forall a effs. Number -> Number -> Number -> Array (Prop a effs) -> VDom a effs
circle cx cy r props = h "circle" ([attr "cx" $ show cx, attr "cy" $ show cy, attr "r" $ show r] <> props) []

use :: forall a effs. Number -> Number -> Number -> Number -> String -> Array (Prop a effs) -> VDom a effs
use x y w h' href' props =
    h "use" ([attr "x" $ show x, attr "y" $ show y, attr "width" $ show w, attr "height" $ show h', attr "href" href'] <> props) []

text' :: forall a effs. Number -> Number -> String -> Array (Prop a effs) -> VDom a effs
text' x y t props = h "text" ([attr "x" $ show x, attr "y" $ show y] <> props) [text t]

translate :: Number -> Number -> String
translate x y = "translate(" <> show x <> "px," <> show y <> "px)"

svguse :: forall a effs. String -> Array (Prop a effs) -> VDom a effs
svguse symbol props = svg ([width "100%", height "100%"]  <> props) [h "use" [attr "href" symbol] []]

rgbColor :: Int -> Int -> Int -> String
rgbColor r g' b = "rgb(" <> show r <> "," <> show g' <> "," <> show b <> ")"

