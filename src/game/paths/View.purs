module Game.Paths.View where
import Prelude
import Pha.Class (VDom, Prop)
import Pha (emptyNode)
import Pha.Html (g, use, class')

square :: forall a. Boolean -> Boolean -> Boolean -> Number -> Number -> Array (Prop a) -> VDom a
square trap darken door x y props =
    g ([class' "paths-darken" darken] <> props) [
        use x y 100.0 100.0 "#paths-background" [],
        if door then use x y 100.0 100.0 "#paths-door" [] else emptyNode,
        use x y 100.0 100.0 "#paths-trap" [class' "paths-trap" true, class' "visible" $ trap && not door]
    ]
{-
export default state => template(state, actions, C => {
    const Config = () => iconfactory(state, actions)(I =>
        Card({ title: 'Chemins' },
            I.Group({
                title: 'Mode de jeu',
                list: [0, 1],
                symbol: 'paths-mode',
                select: state.mode,
                tooltip: ['Mode 1', 'Mode 2'],
                onclick: actions.selectMode
            }),

            I.Sizes({
                sizes: [[4, 6], [5, 5], [3, 8], 'custom']
            }),

            I.Group({ title: 'Options' },
                I.Help(), I.Undo(), I.Redo(), I.Reset(), I.Rules()
            )
        )
    );

    const Hero = () => state.position |> last |> coords(state.columns) |> (([row, col]) =>
        use({
            href: '#meeplehat',
            width: 80,
            height: 80,
            class: 'paths-hero',
            style: {
                transform: `translate(${(col * 100 + 10) / state.columns}%, ${(row * 100 + 10) / state.rows}%)`
            }
        })
    );

    const DoorCursor = () =>
        use({
            href: '#paths-door',
            x: -50,
            y: -50,
            width: 100,
            height: 100,
            opacity: 0.6,
            'pointer-events': 'none',
            style: svgCursorStyle(state.pointerPosition)
        });

    const HeroCursor = () =>
        use({
            href: '#meeplehat',
            x: -40,
            y: -40,
            width: 80,
            height: 80,
            opacity: 0.6,
            'pointer-events': 'none',
            style: svgCursorStyle(state.pointerPosition)
        });

    const pathdec = state.position.flatMap((v, i) =>
        [i === 0 ? 'M' : 'L', 100 * (v % state.columns) + 50, 100 * (v / state.columns | 0) + 50]
    ).join(' ');

    const Grid = () =>
        C.Board({
            trackPointer: true,
            style: gridStyle(state.rows, state.columns)
        },
            svg({ width: '100%', height: '100%', viewBox: `0 0 ${100 * state.columns} ${100 * state.rows}` },
                repeat2(state.rows, state.columns, (row, col, index) =>
                    Square({
                        key: index,
                        x: 100 * col,
                        y: 100 * row,
                        darken: state.help && (row + col) % 2 === 0,
                        trap: init(state.position).includes(index),
                        door: state.exit === index,
                        onclick: [actions.selectVertex, index]
                    })
                ),
                path({
                    d: pathdec,
                    class: 'paths-path'
                }),
                state.position.length > 0 && Hero({ key: 'hero' }),
                state.pointerPosition && state.position.length === 0 && HeroCursor({ key: 'chero' }),
                state.pointerPosition && state.position.length > 0 && state.exit === null && DoorCursor({ key: 'cdoor' })
            )
        );

    const Board = () => C.IncDecGrid(Grid());

    const HelpDialog = () =>
        C.HelpDialog(
            p(
                'Après de moultes péripéties dans le temple maudit de Berge, le professeur Hamilton Jones se retrouve dans la dernière salle', br,
                'Pour sortir de celle-ci, il doit s\'enfuir par une porte au dessous de lui.', br,
                'Celle ci ne peut être ouverte qu\'en marchant sur chacune des dalles dans la salle.'
            ),
            p(
                'Malheusement, ces dalles sont piégées, le piège se déclenchant peu de temps après avoir marché dessus.', br,
                'Donc, Hamilton ne peut pas remarcher sur une dalle sur laquelle il a déjà été.', br,
                'N\'ayant plus l\'aisance de sa jeunesse, Hamilton ne peut se déplacer que d\'une dalle à la fois et ne peut le faire en diagonale.'
            ),
            p('Trouve un parcours pour résoudre l\'énigme. Ca semble facile? Mais, cela est-il possible pour toutes les tailles de grille.'),
            p(
                'Dans le deuxième mode de jeu, tu peux choisir la position de départ d\'Hamilton ainsi que celle de la porte.', br,
                'Tu remarqueras qu\'il n\'y a pas toujours de solution.', br,
                'Trouve des critères sur les positions d\'Hamilton et de la porte pour qu\'une solution soit possible.'
            )
        );

    return { Board, HelpDialog, Config };
});