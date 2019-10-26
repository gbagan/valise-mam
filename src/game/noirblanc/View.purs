{- const levels = [
    { text: '3x3' },
    { text: '4x4' },
    { text: '2x10' },
    { text: '3x10' },
    { text: '5x5' },
    { text: 'NxM', tooltip: 'Dimensions personnalisées' },
    { symbol: 'lo-rand', tooltip: 'Grille aléatoire' },
];
-}

-- const lockedLevel = { symbol: 'locked', tooltip: 'Difficulté non débloquée', disabled: true };

square :: forall a. Boolean -> Array (Attr a) -> VDom a
square light attrs = 
    div' ([class' "noirblanc-square"] <> attrs) [
        div' [class' "noirblanc-square-inner" true, class' "blanc" light] [
            div' [class' "noirblanc-square-blanc" true] [
                {- cross && svg({
                    width: '100%', height: '100%',
                    class: 'ui-absolute noirblanc-cross'
                }, use({href: '#cross'}))
            ), -}
            ],

            div' [class' "noirblanc-square-noir" true] [
               {-
                cross && svg({
                    width: '100%', height: '100%',
                    class: 'ui-absolute noirblanc-cross'
                }, use({href: '#cross'}))
                -}
            ]
        ]
    ]

export default state => template(state, actions, C => {
    grid = div' [class' "ui-board" true] [ -- style: gridStyle(state.rows, state.columns)
        tabulate2 state.rows state.columns \row col index ->
            square 
                (state.position.light[index])
                (state.help && state.position.played[index]) [
                key' $ show index,
                style "height" $ show (86.0 / state.nbRows) <> "%",
                style "width" $ show (86.0 / state.nbColumns) <> "%",
                style "left" $ show ((100.0 * toNumber col + 7) / state.columns) <> '%',
                style "top" $ show (100 * row + 7) / state.rows + "%"
                click: [actions.play, index]
            ]

    board = incDecGrid grid

    config =
        card "Tout noir tout blanc"
            I.Group({
                title: 'Mode jeu',
                list: [0, 1, 2, 3],
                symbol: i => `lo-mode${i + 1}`,
                select: state.mode,
                onclick: actions.selectMode
            }),

            I.Group({ title: 'Difficulté' },
                levels.map((m, i) =>
                    I.Icon({
                        ...(i <= state.maxLevels[state.mode] ? m : lockedLevel),
                        key: i,
                        selected: state.level === i,
                        onclick: [actions.selectLevel, i]
                    })
                )
            ),

            I.Group({ title: 'options' },
                I.Help(), I.Reset(), I.Rules()
            )
        )
    );

    const HelpDialog = () => C.HelpDialog('blablahblah');

    return { Config, Board, HelpDialog };
});