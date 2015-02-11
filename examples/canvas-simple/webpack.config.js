var webpack = require("webpack");

module.exports = {
    entry: [
        "./run.js",
        'webpack/hot/dev-server',
    ],
    output: {
        path: './build',
        filename: "out.js",
    },
    loaders: [
        { test: /\.hs/, loader: 'haskell-loader' }
    ],
    // plugins: [
    //     new webpack.HotModuleReplacementPlugin()
    // ],
};
