import commonjs from 'rollup-plugin-commonjs';
import { terser } from "rollup-plugin-terser";

export default {
    input: 'index.js',
    output: {
        file: 'dist/bundle.js',
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