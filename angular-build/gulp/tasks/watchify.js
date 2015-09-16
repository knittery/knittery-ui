'use strict';

var gulp = require('gulp');
var gutil = require('gulp-util');
var browserify = require('browserify');
var watchify = require('watchify');
var source = require('vinyl-source-stream');
var coffeeify = require('coffeeify');
var ngAnnotate = require('browserify-ngannotate');
var transform = require('vinyl-transform');
var mold = require('mold-source-map')
var exorcist = require('exorcist');
var path = require('path');

var libs = require('./browserify').libs;

var opts = {
  entries: [config.paths.src.modules],
  extensions: ['.coffee'],
  debug: true,
  cache: {},
  packageCache: {}
};

var b = browserify(opts)
libs.forEach(function (lib) {
  b.external(lib);
});
b.transform(coffeeify)
  .transform(ngAnnotate, {add: true, remove: false, ext: ".coffee"});
var w = watchify(b)

function cleanSourceMapPaths(file) {
  var relative = path.relative('.', file)
  return relative.replace(/\\/g, '/')
}

function bundle() {
  return w
    .bundle()
    .on('error', gutil.log.bind(gutil, 'Browserify Error'))
    .pipe(mold.transformSourcesRelativeTo('./'))
    .pipe(mold.transformSources(cleanSourceMapPaths))
    .pipe(source(config.filenames.build.scripts))
    .pipe(transform(function () {
      return exorcist(
        path.join(config.paths.dest.build.scripts,
          config.filenames.build.scripts + '.map'));
    }))
    .pipe(gulp.dest(config.paths.dest.build.scripts));
}

w.on('update', bundle);
w.on('log', gutil.log.bind(gutil, "Browserify: "));

module.exports = gulp.task('watchify', ['browserify-vendor'], bundle);
