//import commonjs from 'rollup-plugin-commonjs';
//import { terser } from "rollup-plugin-terser";
//import resolve from 'rollup-plugin-node-resolve';
//import babel from 'rollup-plugin-babel';

import postcss from 'rollup-plugin-postcss';

export default {
    input: 'index.js',
    output: {
        file: 'bundle.js',
        format: 'iife',
        sourcemap: true
    },
    plugins: [
        postcss({
            extensions: [ '.css', '.scss' ],
            extract: true,
            minimize: true
        })
    ],
};


/*
export default {
    input: "./index.js",
    output: {
        file: "./bundle.js",
        format: "iife",
        name: "f"
    },

    plugins: [
        commonjs(),
        terser({
                ecma: 6,
                compress: {
                    unsafe_arrows: true
                }
        })
    ]
};
*/