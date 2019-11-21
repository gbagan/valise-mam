module Game.Sansmot.View where

import MyPrelude
import Data.Map (Map, fromFoldable) as M
import Lib.Util (tabulate)
import Game.Effs (EFFS)
import Pha (VDom, Prop, text)
import Pha.Html (div', p, h1, h2, svg, path, line, text',
                key, class', attr, style, stroke, fill, viewBox, pc, opacity, width, height, translate, click)
import Game.Sansmot.Model (State, Page(..), pythaAnimation, carollAnimation, animateA, setPageA)

-- besoin d'un transform par défault pour empécher un bug sous safari
defaultStyle :: ∀a effs. Array (Prop a effs)
defaultStyle = [style "transform" "translate(0px, 0px)"]

compStyle :: ∀a effs. Number -> Number -> {rotation :: Int, translation :: Tuple Int Int, duration :: Int} -> Array (Prop a effs)
compStyle width height { rotation, translation: x ∧ y, duration} = [
    style "transform" $ 
        translate (pc $ toNumber x / width) (pc $ toNumber y / height),
    style "transition" $ "transform linear " <> show duration <> "ms"
]

pythaStyles :: ∀a effs. M.Map String (Array (Array (Prop a effs)))
pythaStyles = M.fromFoldable [
    "a" ∧ [[opacity "0"], defaultStyle, compStyle 700.0 300.0 { translation: 400 ∧ (-100), rotation: 0, duration: 600 }],
    "b" ∧ [[opacity "0"], defaultStyle, compStyle 700.0 300.0 { translation: 600 ∧ 0,      rotation: 0, duration: 600 }],
    "c" ∧ [[opacity "0"], defaultStyle, compStyle 700.0 300.0 { translation: 400 ∧ 0,      rotation: 0, duration: 600 }],
    "d" ∧ [[opacity "0"], defaultStyle, compStyle 700.0 300.0 { translation: 300 ∧ 200,    rotation: 0, duration: 600 }],
    "e" ∧ [[opacity "0"], []]
]

carollStyles :: ∀a effs. M.Map String (Array (Array (Prop a effs)))
carollStyles = M.fromFoldable [
    "a" ∧ [defaultStyle, compStyle 1370.0 270.0 { translation: 300 ∧ 150, rotation: 0, duration: 600 }],
    "b" ∧ [defaultStyle, compStyle 1370.0 270.0 { translation: 550 ∧ 50,   rotation: 0, duration: 600 }],
    "c" ∧ [defaultStyle, compStyle 1370.0 270.0 { translation: 700 ∧ 0,       rotation: 0, duration: 600 }],
    "d" ∧ [defaultStyle, compStyle 1370.0 270.0 { translation: 950 ∧ (-100),     rotation: 0, duration: 600 }],
    "e" ∧ [[opacity "0"], []]
]


-- const bbbb = styles => state => styles |> omap((style, name) => style[state.anim[name] || 0]);
animPytha :: ∀a effs. State -> VDom a effs
animPytha {anim} =
    let f key = 
                let phase = anim ^. at key # fromMaybe 0 in
                fromMaybe [] $ pythaStyles ^. at key >>= \t -> t !! phase        
    in
    svg [class' "sansmot-svg" true, viewBox 0 0 700 300, style "width" "84vmin", style "height" "36vmin"] [
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
        text' 5 55 "a" $ [style "font-size" 20] <> f "e",
        text' 46 12 "a" $ [style "font-size" 20] <> f "e",
        text' 105 210 "b" $  [style "font-size" 20] <> f "e",
        text' 198 120 "b" $ [style "font-size" 20] <> f "e",
        text' 460 98 "c" $ [style "font-size" 20] <> f "e",
        text' 595 80 "c" $ [style "font-size" 20] <> f "e"
    ]

animCaroll :: ∀effs. State -> VDom State effs
animCaroll {anim} =
    let f key = 
                let phase = anim ^. at key # fromMaybe 0 in
                fromMaybe [] $ carollStyles ^. at key >>= \t -> t !! phase        
    in
    svg [class' "sansmot-svg" true, viewBox (-10) (-10) 1370 270, width "90vw", height "19vw"] $concat [
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
        tabulate 28 \i ->
            line (50 * i) (-10) (50 * i) 260 [class' "sansmot-grid" true],
        tabulate 6 \i ->
            line (-10) (50 * i) 1360 (50 * i) [class' "sansmot-grid" true]
    ]

view :: State -> VDom State EFFS
view state = 
    div' [class' "sansmot-main" true] [
        div' [class' "sansmot-menu" true] [
            div' [class' "sansmot-pagelink" true, click $ setPageA PythaPage] [text "1"],
            div' [class' "sansmot-pagelink" true, click $ setPageA CarollPage] [text "2"]
        ],
        main state.page
    ] where
    
    main PythaPage = div' [key "pytha"] [
        h1 [class' "sansmot-title" true] "Preuve sans mot",

        h2 [class' "sansmot-h2" true] "Que raconte le théorème de Pythagore ?",
     
        p [class' "sansmot-center" true] [
            svg [class' "sansmot-svg" true, viewBox 0 (-100) 200 250, width "20vmin", height "25vmin"] [
                path "M 50 50 h 100 v 100 h -100 Z" [fill "yellow", stroke "black"],
                path "M 0 0 h 50 v 50 h -50 Z" [fill "yellow", stroke "black"],
                path "M 50 0 l 100 50 l 50 -100 l -100 -50 Z" [fill "#00ff00", stroke "black"],
                text' 90 105 "a²" [attr "font-size" 35],
                text' 18 35 "b²" [attr "font-size" 35],
                text' 110 (-10) "c²" [attr "font-size" 35]
            ]
        ],

        h2 [class' "sansmot-h2" true] "Preuve sans mot due à un auteur chinois inconnu qui vivait vers 200 avant J.-C.",
        p [class' "sansmot-center" true] [
            animPytha state
        ],
        p [class' "sansmot-center sansmot-link" true, click $ animateA pythaAnimation] [text "Lancer l'animation"]
    ]

    main CarollPage = div' [key "caroll"] [
        h1 [class' "sansmot-title" true] "Preuve sans mot",
        h2 [class' "sansmot-h2" true] "Où est passé le carré manquant ?",
        p [class' "sansmot-center" true] [
            animCaroll state
        ],
        p [class' "sansmot-center sansmot-link" true, click $ animateA carollAnimation] [text "Lancer l'animation"]
    ]
