'use strict';

var gulp = require('gulp');
var gulpif = require('gulp-if');
var replace = require('gulp-replace');
var minifyHTML = require('gulp-minify-html');

function replaceStyles(config) {
  var styles = '<link href="/' + config.vendorStyles + '" rel="stylesheet">';
  styles += '<link href="/' + config.styles + '" rel="stylesheet">';
  return replace('<!--styles-->', styles)
}

function replaceScripts(config) {
  var scripts = '<script src="/' + config.vendorScripts + '"></script>';
  scripts += '<script src="/' + config.scripts + '"></script>';
  return replace('<!--scripts-->', scripts)
}

module.exports = gulp.task('index', function () {
  return gulp.src(config.paths.src.index)
    .pipe(gulpif(release, minifyHTML({comments: true, empty: true, spare: true, quotes: true})))
    .pipe(gulpif(release,
      replaceStyles(config.filenames.release),
      replaceStyles(config.filenames.build)
    ))
    .pipe(gulpif(release,
      replaceScripts(config.filenames.release),
      replaceScripts(config.filenames.build)
    ))
    .pipe(gulpif(release,
      gulp.dest(config.paths.dest.release.index),
      gulp.dest(config.paths.dest.build.index)));
});
