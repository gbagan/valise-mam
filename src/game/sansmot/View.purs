module Game.Sansmot.View where

import MyPrelude
import Data.Map (Map, fromFoldable) as M
import Lib.Util (repeat)
import Pha (VDom, Prop, text, key, class_, attr, style)
import Pha.Elements (div, p, h1, h2)
import Pha.Events (onclick)
import Pha.Svg (svg, path, line, text', stroke, fill, viewBox, x_, y_, width, height, opacity)
import Pha.Util (pc, translate)
import Game.Sansmot.Model (State, Msg(..), Page(..), pythaAnimation, carollAnimation)

line' ∷ ∀a. Int → Int → Int → Int → Array (Prop a) → VDom a
line' x1_ y1_ x2_ y2_ props = line ([attr "x1" (show x1_), attr "x2" (show x2_), attr "y1" (show y1_), attr "y2" (show y2_)] <> props)

-- besoin d'un transform par défault pour empécher un bug sous safari
defaultStyle ∷ ∀a. Array (Prop a)
defaultStyle = [style "transform" "translate(0px, 0px)"]

compStyle ∷ ∀a. Number → Number → {rotation ∷ Int, translation ∷ Tuple Int Int, duration ∷ Int} → Array (Prop a)
compStyle width height { rotation, translation: x ∧ y, duration} = [
    style "transform" $ 
        translate (pc $ toNumber x / width) (pc $ toNumber y / height),
    style "transition" $ "transform linear " <> show duration <> "ms"
]

pythaStyles ∷ ∀a. M.Map String (Array (Array (Prop a)))
pythaStyles = M.fromFoldable [
    "a" ∧ [[opacity "0"], defaultStyle, compStyle 700.0 300.0 { translation: 400 ∧ (-100), rotation: 0, duration: 600 }],
    "b" ∧ [[opacity "0"], defaultStyle, compStyle 700.0 300.0 { translation: 600 ∧ 0,      rotation: 0, duration: 600 }],
    "c" ∧ [[opacity "0"], defaultStyle, compStyle 700.0 300.0 { translation: 400 ∧ 0,      rotation: 0, duration: 600 }],
    "d" ∧ [[opacity "0"], defaultStyle, compStyle 700.0 300.0 { translation: 300 ∧ 200,    rotation: 0, duration: 600 }],
    "e" ∧ [[opacity "0"], []]
]

carollStyles ∷ ∀a. M.Map String (Array (Array (Prop a)))
carollStyles = M.fromFoldable [
    "a" ∧ [defaultStyle, compStyle 1370.0 270.0 { translation: 300 ∧ 150, rotation: 0, duration: 600 }],
    "b" ∧ [defaultStyle, compStyle 1370.0 270.0 { translation: 550 ∧ 50,   rotation: 0, duration: 600 }],
    "c" ∧ [defaultStyle, compStyle 1370.0 270.0 { translation: 700 ∧ 0,       rotation: 0, duration: 600 }],
    "d" ∧ [defaultStyle, compStyle 1370.0 270.0 { translation: 950 ∧ (-100),     rotation: 0, duration: 600 }],
    "e" ∧ [[opacity "0"], []]
]


-- const bbbb = styles ⇒ state ⇒ styles |> omap((style, name) ⇒ style[state.anim[name] || 0]);
animPytha ∷ State → VDom Msg
animPytha {anim} =
    let f key = 
                let phase = anim ^. at key # fromMaybe 0 in
                fromMaybe [] $ pythaStyles ^. at key >>= \t → t !! phase        
    in
    svg [class_ "sansmot-svg", viewBox 0 0 700 300, style "width" "84vmin", style "height" "36vmin"] [
        path "M 0 300 h 300 v -300 h -300 Z L 100 100 M 0 100 h 300 l -200 -100 v 300" [fill "transparent", stroke "#000"],
        path "M 400 300 h 300 v -300 h -300 Z M 400 200 l 200 100 l 100 -200 l -200 -100 l -100 200" [fill "transparent", stroke "#000"],
        path "M 0 300 v -200 h 100 Z" $ [fill "blue", stroke "#000"] <> f "a",
        path "M 0 300 h 100 v -200 Z" $ [fill "yellow", stroke "#000"] <> f "b",
        path "M 100 0 h 200 v 100 Z" $ [fill "#00FF00", stroke "#000"] <> f "c",
        path "M 100 0 v 100 h 200 Z" $ [fill "red", stroke "#000"] <> f "d",
        path "M 0 300 v -200 h 100 Z" $ [fill "blue", stroke "#000"] <> f "e",
        path "M 0 300 h 100 v -200 Z" $ [fill "yellow", stroke "#000"] <> f "e",
        path "M 100 0 h 200 v 100 Z" $ [fill "#00FF00", stroke "#000"] <> f "e",
        path "M 100 0 v 100 h 200 0 Z" $ [fill "red", stroke "#000"] <> f "e",
        text' "a" $ [x_ "5", y_ "55", style "font-size" "20"] <> f "e",
        text' "a" $ [x_ "46", y_ "12", style "font-size" "20"] <> f "e",
        text' "b" $  [x_ "105", y_ "210", style "font-size" "20"] <> f "e",
        text' "b" $ [x_ "198", y_ "120", style "font-size" "20"] <> f "e",
        text' "c" $ [x_ "450", y_ "98", style "font-size" "20"] <> f "e",
        text' "c" $ [x_ "595", y_ "80", style "font-size" "20"] <> f "e"
    ]

animCaroll ∷ State → VDom Msg
animCaroll {anim} =
    let f key = 
                let phase = anim ^. at key # fromMaybe 0 in
                fromMaybe [] $ carollStyles ^. at key >>= \t → t !! phase        
    in
    svg [class_ "sansmot-svg", viewBox (-10) (-10) 1370 270, width "90vw", height "19vw"] $concat [
        [
            path "M 400 100 h 250 v -100 Z"                    $  [fill "orange"] <> f "a",
            path "M 400 200 h 150 v -50 h 100 v -50 h -250 Z" $ [fill "red"] <> f "b",
            path "M 400 250 h 250 v -100 h -100 v 50 h -150 Z" $ [fill "blue"] <> f "c",
            path "M 0 250 h 400 v -150 Z"                   $ [fill "green"] <> f "d",
            path "M 400 100 h 250 v -100 Z"                    $  [fill "orange"] <> f "e",
            path "M 400 200 h 150 v -50 h 100 v -50 h -250 Z"  $ [fill "red"] <> f "e", 
            path "M 400 250 h 250 v -100 h -100 v 50 h -150 Z" $ [fill "blue"] <> f "e",
            path "M 0 250 h 400 v -150 Z"                    $ [fill "green"] <> f "e"
        ],
        repeat 28 \i →
            line' (50 * i) (-10) (50 * i) 260 [class_ "sansmot-grid"],
        repeat 6 \i →
            line' (-10) (50 * i) 1360 (50 * i) [class_ "sansmot-grid"]
    ]

view ∷ State → VDom Msg
view state = 
    div [class_ "sansmot-main"] [
        div [class_ "sansmot-menu"] [
            div [class_ "sansmot-pagelink", onclick $ SetPage PythaPage] [text "1"],
            div [class_ "sansmot-pagelink", onclick $ SetPage CarollPage] [text "2"]
        ],
        main state.page
    ] where
    
    main PythaPage = div [key "pytha"] [
        h1 [class_ "sansmot-title"] [text "Preuve sans mot"],

        h2 [class_ "sansmot-h2"] [text "Que raconte le théorème de Pythagore ?"],
     
        p [class_ "sansmot-center"] [
            svg [class_ "sansmot-svg", viewBox 0 (-100) 200 250, width "20vmin", height "25vmin"] [
                path "M 50 50 h 100 v 100 h -100 Z" [fill "yellow", stroke "black"],
                path "M 0 0 h 50 v 50 h -50 Z" [fill "yellow", stroke "black"],
                path "M 50 0 l 100 50 l 50 -100 l -100 -50 Z" [fill "#00ff00", stroke "black"],
                text' "a²" [x_ "90", y_ "105", attr "font-size" "35"],
                text' "b²" [x_ "18", y_ "35", attr "font-size" "35"],
                text' "c²" [x_ "110", y_ "-10", attr "font-size" "35"]
            ]
        ],

        h2 [class_ "sansmot-h2"] [text "Preuve sans mot due à un auteur chinois inconnu qui vivait vers 200 avant J.-C."],
        p [class_ "sansmot-center"] [
            animPytha state
        ],
        p [class_ "sansmot-center sansmot-link", onclick $ Animate pythaAnimation] [text "Lancer l'animation"]
    ]

    main CarollPage = div [key "caroll"] [
        h1 [class_ "sansmot-title"] [text "Preuve sans mot"],
        h2 [class_ "sansmot-h2"] [text "Où est passé le carré manquant ?"],
        p [class_ "sansmot-center"] [
            animCaroll state
        ],
        p [class_ "sansmot-center sansmot-link", onclick $ Animate carollAnimation] [text "Lancer l'animation"]
    ]
