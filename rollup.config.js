import commonjs from 'rollup-plugin-commonjs';
import { terser } from "rollup-plugin-terser";
//import resolve from 'rollup-plugin-node-resolve';
//import babel from 'rollup-plugin-babel';
// import postcss from 'rollup-plugin-postcss';

export default {
    input: 'index.js',
    output: {
        file: 'test.js',
        format: 'iife',
        sourcemap: false
    },
    plugins: [
        commonjs(),
        terser({
                ecma: 7,
                compress: {
                    unsafe_arrows: true
                }
        })
    ]
};