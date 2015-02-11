module.exports = {
    entry: "./runtime.js",
    output: {
        filename: "./runtime-bundle.js",
    },
    loaders: [
        {test: /\.js?$/, exclude: /node_modules/, loader: '6to5-loader?experimental&optional=selfContained' }
    ]
};
