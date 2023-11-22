const os = require('os'),
      webpack = require('webpack'),
      fs = require('fs'),
      path = require('path'),
      { VueLoaderPlugin } = require('vue-loader');

const
  basics = (argv) => ({
    mode: 'development',
    devtool: argv.mode !== 'production' ? "source-map" : undefined,
    stats: {
      hash: false, version: false, modules: false  // reduce verbosity
    },
    performance: {
      maxAssetSize: 1e6, maxEntrypointSize: 1e6   // 250k is too small
    }
  }),
  ts = {
    test: /\.tsx?$/,
    loader: 'ts-loader',
    options: {
        allowTsInNodeModules: true,
        appendTsSuffixTo: [/\.vue$/]
    }
  },
  css = {
    test: /\.css$/i,
    use: ['style-loader', 'css-loader'],
  },
  scss = {
    test: /\.scss$/i,  /* Vue.js has some */
    use: ['style-loader', 'css-loader', 'sass-loader'],
  },
  vuesfc = {
    test: /\.vue$/,
    use: 'vue-loader'
  },
  plugins = [
    new VueLoaderPlugin(),
    new webpack.DefinePlugin({
      __VUE_OPTIONS_API__: true,
      __VUE_PROD_DEVTOOLS__: true
    }),
    new webpack.ProvidePlugin({Buffer: ['buffer', 'Buffer']})
  ],
  resolve = {
    extensions: [ '.tsx', '.ts', '.js', '.cjs' ]
  },
  output = (dirname, filename) => ({
    filename, path: path.join(__dirname, dirname)
  });

module.exports = (env, argv) => [
{
  name: 'ui',
  entry: './src/ui/main.ts',
  ...basics(argv),
  module: {
    rules: [ts, css, scss, vuesfc]
  },
  resolve,
  output: output('dist', 'main.js'),
  externals: {
    fs: 'commonjs2 fs', path: 'commonjs2 path',
    child_process: 'commonjs2 child_process'
  },
  plugins
},
{
  name: 'aux',
  entry: './src/other/the-cppnator.ts',
  target: 'node',
  ...basics(argv),
  module: {
    rules: [ts, css, scss, vuesfc]
  },
  resolve,
  output: output('build/cppnator', 'the-cppnator.js')
}
];
