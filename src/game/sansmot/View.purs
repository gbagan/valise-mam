module Game.Sansmot.View where

import MyPrelude
import Data.Map as Map
import Pha.Html (Html)
import Pha.Html as H
import Pha.Html.Attributes as P
import Pha.Html.Events as E
import Pha.Html.Util (pc, translate)
import Game.Sansmot.Model (State, Msg(..), Page(..), pythaAnimation, carollAnimation)

line' ∷ ∀a. Int → Int → Int → Int → Array (H.Prop a) → Html a
line' x1 y1 x2 y2 props = H.line ([P.x1 $ toNumber x1, P.x2 $ toNumber x2, P.y1 $ toNumber y1, P.y2 $ toNumber y2] <> props)

-- besoin d'un transform par défault pour empécher un bug sous safari
defaultStyle ∷ ∀a. Array (H.Prop a)
defaultStyle = [H.style "transform" "translate(0px, 0px)"]

compStyle ∷ ∀a. Number → Number → {rotation ∷ Int, translation ∷ Tuple Int Int, duration ∷ Int} → Array (H.Prop a)
compStyle width height { translation: x ∧ y, duration} = [
    H.style "transform" $ 
        translate (pc $ toNumber x / width) (pc $ toNumber y / height),
    H.style "transition" $ "transform linear " <> show duration <> "ms"
]

pythaStyles ∷ ∀a. Map String (Array (Array (H.Prop a)))
pythaStyles = Map.fromFoldable [
    "a" ∧ [[P.opacity 0.0], defaultStyle, compStyle 700.0 300.0 { translation: 400 ∧ (-100), rotation: 0, duration: 600 }],
    "b" ∧ [[P.opacity 0.0], defaultStyle, compStyle 700.0 300.0 { translation: 600 ∧ 0,      rotation: 0, duration: 600 }],
    "c" ∧ [[P.opacity 0.0], defaultStyle, compStyle 700.0 300.0 { translation: 400 ∧ 0,      rotation: 0, duration: 600 }],
    "d" ∧ [[P.opacity 0.0], defaultStyle, compStyle 700.0 300.0 { translation: 300 ∧ 200,    rotation: 0, duration: 600 }],
    "e" ∧ [[P.opacity 0.0], []]
]

carollStyles ∷ ∀a. Map String (Array (Array (H.Prop a)))
carollStyles = Map.fromFoldable [
    "a" ∧ [defaultStyle, compStyle 1370.0 270.0 { translation: 300 ∧ 150,    rotation: 0, duration: 600 }],
    "b" ∧ [defaultStyle, compStyle 1370.0 270.0 { translation: 550 ∧ 50,     rotation: 0, duration: 600 }],
    "c" ∧ [defaultStyle, compStyle 1370.0 270.0 { translation: 700 ∧ 0,      rotation: 0, duration: 600 }],
    "d" ∧ [defaultStyle, compStyle 1370.0 270.0 { translation: 950 ∧ (-100), rotation: 0, duration: 600 }],
    "e" ∧ [[P.opacity 0.0], []]
]

animPytha ∷ State → Html Msg
animPytha {anim} =
    let f key = 
                let phase = anim ^. at key ?: 0 in
                pythaStyles ^? (ix key ∘ ix phase) ?: []
    in
    H.svg [H.class_ "sansmot-svg", P.viewBox 0 0 700 300, H.style "width" "84vmin", H.style "height" "36vmin"]
    [   H.path $ [P.d "M 0 300 h 300 v -300 h -300 Z L 100 100 M 0 100 h 300 l -200 -100 v 300", P.fill "transparent", P.stroke "#000"]
    ,   H.path $ [P.d "M 400 300 h 300 v -300 h -300 Z M 400 200 l 200 100 l 100 -200 l -200 -100 l -100 200", 
                    P.fill "transparent", P.stroke "#000"]
    ,   H.path $ [P.d "M 0 300 v -200 h 100 Z", P.fill "blue", P.stroke "#000"] <> f "a"
    ,   H.path $ [P.d "M 0 300 h 100 v -200 Z", P.fill "yellow", P.stroke "#000"] <> f "b"
    ,   H.path $ [P.d "M 100 0 h 200 v 100 Z", P.fill "#00FF00", P.stroke "#000"] <> f "c"
    ,   H.path $ [P.d "M 100 0 v 100 h 200 Z", P.fill "red", P.stroke "#000"] <> f "d"
    ,   H.path $ [P.d "M 0 300 v -200 h 100 Z", P.fill "blue", P.stroke "#000"] <> f "e"
    ,   H.path $ [P.d "M 0 300 h 100 v -200 Z", P.fill "yellow", P.stroke "#000"] <> f "e"
    ,   H.path $ [P.d "M 100 0 h 200 v 100 Z", P.fill "#00FF00", P.stroke "#000"] <> f "e"
    ,   H.path $ [P.d "M 100 0 v 100 h 200 0 Z", P.fill "red", P.stroke "#000"] <> f "e"
    ,   H.text_ "a" $ [P.x 5.0, P.y 55.0, H.style "font-size" "20"] <> f "e"
    ,   H.text_ "a" $ [P.x 46.0, P.y 12.0, H.style "font-size" "20"] <> f "e"
    ,   H.text_ "b" $ [P.x 105.0, P.y 210.0, H.style "font-size" "20"] <> f "e"
    ,   H.text_ "b" $ [P.x 198.0, P.y 120.0, H.style "font-size" "20"] <> f "e"
    ,   H.text_ "c" $ [P.x 450.0, P.y 98.0, H.style "font-size" "20"] <> f "e"
    ,   H.text_ "c" $ [P.x 595.0, P.y 80.0, H.style "font-size" "20"] <> f "e"
    ]

animCaroll ∷ State → Html Msg
animCaroll {anim} =
    let f key = 
                let phase = anim ^. at key ?: 0 in
                carollStyles ^? (ix key ∘ ix phase) ?: []        
    in
    H.svg [H.class_ "sansmot-svg", P.viewBox (-10) (-10) 1370 270, P.width "90vw", P.height "19vw"] $ concat 
    [
        [   H.path $ [P.d "M 400 100 h 250 v -100 Z"                   , P.fill "orange"] <> f "a"
        ,   H.path $ [P.d "M 400 200 h 150 v -50 h 100 v -50 h -250 Z" , P.fill "red"] <> f "b"
        ,   H.path $ [P.d "M 400 250 h 250 v -100 h -100 v 50 h -150 Z", P.fill "blue"] <> f "c"
        ,   H.path $ [P.d "M 0 250 h 400 v -150 Z"                     , P.fill "green"] <> f "d"
        ,   H.path $ [P.d "M 400 100 h 250 v -100 Z"                   , P.fill "orange"] <> f "e"
        ,   H.path $ [P.d "M 400 200 h 150 v -50 h 100 v -50 h -250 Z" , P.fill "red"] <> f "e"
        ,   H.path $ [P.d "M 400 250 h 250 v -100 h -100 v 50 h -150 Z", P.fill "blue"] <> f "e"
        ,   H.path $ [P.d "M 0 250 h 400 v -150 Z"                     , P.fill "green"] <> f "e"
        ]
    ,   repeat 28 \i →
            line' (50 * i) (-10) (50 * i) 260 [H.class_ "sansmot-grid"]
    ,   repeat 6 \i →
            line' (-10) (50 * i) 1360 (50 * i) [H.class_ "sansmot-grid"]
    ]

view ∷ State → Html Msg
view state = 
    H.div [H.class_ "sansmot-main"] 
    [   H.div [H.class_ "sansmot-menu"] 
        [   H.div [H.class_ "sansmot-pagelink", E.onClick \_ → SetPage PythaPage] [H.text "1"]
        ,   H.div [H.class_ "sansmot-pagelink", E.onClick \_ → SetPage CarollPage] [H.text "2"]
        ]
    ,   main state.page
    ] where
    
    main PythaPage =
        H.div []
        [   H.h1 [H.class_ "sansmot-title"] [H.text "Preuve sans mot"]
        ,   H.h2 [H.class_ "sansmot-h2"] [H.text "Que raconte le théorème de Pythagore ?"]
        ,   H.p [H.class_ "sansmot-center"]
            [   H.svg [H.class_ "sansmot-svg", P.viewBox 0 (-100) 200 250, P.width "20vmin", P.height "25vmin"]
                [   H.path [P.d "M 50 50 h 100 v 100 h -100 Z", P.fill "yellow", P.stroke "black"]
                ,   H.path [P.d "M 0 0 h 50 v 50 h -50 Z", P.fill "yellow", P.stroke "black"]
                ,   H.path [P.d "M 50 0 l 100 50 l 50 -100 l -100 -50 Z", P.fill "#00ff00", P.stroke "black"]
                ,   H.text_ "a²" [P.x 90.0, P.y 105.0, H.attr "font-size" "35"]
                ,   H.text_ "b²" [P.x 18.0, P.y 35.0, H.attr "font-size" "35"]
                ,   H.text_ "c²" [P.x 110.0, P.y (-10.0), H.attr "font-size" "35"]
                ]
            ]
        ,   H.h2 [H.class_ "sansmot-h2"] [H.text "Preuve sans mot due à un auteur chinois inconnu qui vivait vers 200 avant J.-C."]
        ,   H.p [H.class_ "sansmot-center"] [animPytha state]
        ,   H.p [H.class_ "sansmot-center sansmot-link", E.onClick \_ → Animate pythaAnimation] [H.text "Lancer l'animation"]
        ]

    main CarollPage =
        H.div []
        [   H.h1 [H.class_ "sansmot-title"] [H.text "Preuve sans mot"]
        ,   H.h2 [H.class_ "sansmot-h2"] [H.text "Où est passé le carré manquant ?"]
        ,   H.p [H.class_ "sansmot-center"] [animCaroll state]
        ,   H.p [H.class_ "sansmot-center sansmot-link", E.onClick \_ → Animate carollAnimation] [H.text "Lancer l'animation"]
        ]
