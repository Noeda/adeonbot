module.exports = {
    context: __dirname + "/src",
    entry: './entry.jsx',

    module: {
        loaders: [
            {
                test: /.jsx?$/,
                loader: 'babel-loader',
                exclude: /node_modules/,
                query: {
                    presets: ['es2017', 'react'],
                    plugins: [require('babel-plugin-transform-object-rest-spread')]
                }
            }
        ]
    },

    output: {
        path: __dirname + "/dist",
        filename: 'bundle.js'
    }
}

