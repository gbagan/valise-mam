module Game.Valise.View where
import Prelude
import Data.Maybe (Maybe(..))
import Data.Lens (Lens')
import Game.Valise.Model (ValiseState)
import Pha (VDom, h, text, whenN, maybeN)
import Pha.Html (div', a, svg, class', svguse, href, width, height)


object {drag, link, help, symbol} props children = div' ([
    -- payloadFn = relativePointerPosition -- >>= (set('name', drag));
    -- position = if drag then state.position[drag];
    
        -- 'touch-action': 'none',
    class' "valise-object ui-touch-action-none" true,
    class' "draggable" drag
        -- style: {
        --    ...(position && { left: position.left + 'px', top: position.top + 'px' })
        --},
        -- onpointerdown: drag && combine([actions.beginDrag, payloadCreator], preventDefault),
] <> props) [
    svg [width "100%", height "100%"] [
        h "use" [href $ "#" <> symbol, class' "valise-symbol" true] []
    ],
    div' [
        class' "valise-object-area" true
        --  pointerenter: onmouseenter <> showHelpA help,
        -- onpointerleave: onpointerleave <> showHelpA Nothing
    ] ([
        maybeN $ link <#> \l -> a [
            href $ "#" <> l,
            class' "valise-object-link" true
        ] []
    ] <> children)
]

valise :: forall a. ValiseState -> VDom a
valise state = div' [
    class' "valise-image valise-open image-openvalise" true
    -- onpointerup: actions.finishDrag,
    -- onpointermove: [actions.moveObject, relativePointerPosition],
] [
    div' [class' "valise-objects" true] [
        div' [
            class' "valise-help" true,
            class' "visible" (state.helpVisible && state.help /= "")
        ] [text state.help],
        object { symbol: "switch", link: Nothing, help: "", drag: false } [
            class' "valise-switch" true,
            class' "on" state.isSwitchOn
            -- onclick: actions.toggleSwitch
        ] [], {-

            Object({
                symbol: 'bulboff',
                class: {'valise-bulb': true, on: state.isSwitchOn},
                help: 'Trouve un moyen d\'allumer l\'ampoule',
            },
                Object({
                    symbol: 'bulbon',
                    class: {'valise-bulbon': true, on: state.isSwitchOn},
                    help: 'Jeu: tout noir, tout blanc',
                    link: 'noirblanc'
                })
            ),

            Object({
                drag: 'flowerpot',
                symbol: 'flowerpot',
                class: 'valise-flowerpot',
                help: 'Quelque chose se cache derrière ce pot'
            }),
        -}
        object { symbol: "frog", link : Just "frog", help: "jeu: La grenouille", drag: false} [class' "vanille-frog" true]
        {-
            Object({
                symbol: 'hanoitop',
                drag: 'hanoitop',
                class: 'valise-hanoi-top',
                help: 'Quelque chose se cache sous cette tour'
            }),

            Object({
                symbol: 'hanoibot',
                class: 'valise-hanoi-bot',
                help: 'Jeu: Solitaire',
                link: 'solitaire'
            }),

            Object({
                class: 'valise-knight',
                symbol: 'knight',
                link: 'queens',
                help: 'Jeu: les 8 reines'
            }),

            Object({
                symbol: 'pen',
                class: 'valise-pen',
                link: 'dessin',
                help: 'Jeu: dessin'
            }),

            Object({
                symbol: 'stack',
                class: 'valise-stack',
                link: 'jetons',
                help: 'Jeu: jetons'
            }),

            Object({
                symbol: 'wheel',
                class: 'valise-wheel',
                link: 'roue',
                help: 'Jeu: roue des couleurs'
            }),

            Object({
                symbol: 'card',
                class: 'valise-card',
                link: 'nim',
                help: 'Jeu: Poker Nim'
            }),

            repeat(7, i => i !== 3 &&
                Object({
                    symbol: 'paw',
                    class: `valise-pawimage valise-paw-${i}`
                })
            ),
            Object({
                symbol: 'paw',
                class: 'valise-pawimage valise-paw-3 valise-paw',
                style: {opacity: state.pawPassings*0.1},
                help: state.pawPassings === 4 ? 'Jeu: La bête' : 'Trouve un moyen de dévoiler l\'empreinte cachée',
                onpointerleave: actions.incPawPassings,
                link: state.pawPassings === 4 && 'labete'
            }),

            Object({
                symbol: 'tile',
                class: 'valise-tile',
                link: 'tiling',
                help: 'Jeu: carrelage'
            }),

            Object({
                symbol: 'tricolor',
                class: 'valise-tricolor',
                link: 'baseball',
                help: 'Jeu: baseball tricolore'
            }),

            Object({
                symbol: 'race',
                class: 'valise-race',
                link: 'paths',
                help: 'Jeu: chemins'
            }),

            Object({
                symbol: 'quiet',
                class: 'valise-quiet',
                link: 'sansmot',
                help: 'Jeu: Preuve sans mot'
            }),

            Object({
                symbol: 'chocolate',
                class: 'valise-chocolate',
                link: 'chocolat',
                help: 'Jeu: Chocolat'
            }), -}
    ]
]

view :: forall a. Lens' a ValiseState -> ValiseState -> VDom a 
view lens state = div' [
    class' "ui-flex-center valise-main-container" true,
    class' "open" state.isOpen
] [
    div' [] [
        div' [class' "valise-logo" true] [svguse "#logo" []],
        div' [class' "valise-container" true] [
            div' [class' "valise-close valise-image image-valise" true] [],
            valise state
        ]
    ]
]