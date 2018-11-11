import node_resolve from 'rollup-plugin-node-resolve';
import commonjs from 'rollup-plugin-commonjs';
import replace from 'rollup-plugin-replace';

export default {
    input: './src/Main.bs.js',
    output: {
        file: './release/main.js',
        format: 'iife',
        name: 'starter'

    },
    plugins: [
        node_resolve({module: true, browser: true}),

        commonjs({
            namedExports: {
                // left-hand side can be an absolute path, a path
                // relative to the current directory, or the name
                // of a module in node_modules
                'node_modules/lodash/lodash.js': [ 'throttle' ],
            //    'node_modules/in-view/src/index.js': [ 'inView' ]
            }
        }),

        replace({
            'process.env.NODE_ENV': JSON.stringify( 'production' )
        }),
    ],
    watch: {
        clearScreen: false
    }
}
