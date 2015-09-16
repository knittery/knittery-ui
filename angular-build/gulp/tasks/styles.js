'use strict';

var gulp = require('gulp');
var gulpif = require('gulp-if');
var rename = require('gulp-rename');
var autoprefixer = require('gulp-autoprefixer');
var less = require('gulp-less');
var sourcemaps = require('gulp-sourcemaps');
var minifyCss = require('gulp-minify-css');
var replace = require('gulp-replace');

function handleError(err) {
  console.log(err.toString());
  this.emit('end');
}

var prefixer = function() { return autoprefixer('last 1 version') };

module.exports = gulp.task('styles-vendor', function () {
  return gulp.src(config.paths.src.vendorStyles)
    .pipe(gulpif(!release, sourcemaps.init()))
    .pipe(less({
      paths: [
        config.paths.src.node_modules,
        config.paths.src.styles
      ]
    }).on('error', handleError))
    .pipe(prefixer())
    .pipe(minifyCss({
      relativeTo: config.paths.src.node_modules
    }))
    .pipe(replace('../../node_modules/', '/assets/vendor/'))
    .pipe(gulpif(!release, sourcemaps.write()))
    .pipe(gulpif(release, rename(config.filenames.release.vendorStyles), rename(config.filenames.build.vendorStyles)))
    .pipe(gulpif(release, gulp.dest(config.paths.dest.release.styles), gulp.dest(config.paths.dest.build.styles)));
});


module.exports = gulp.task('styles', function () {
  return gulp.src(config.paths.src.styles)
    .pipe(gulpif(!release, sourcemaps.init()))
    .pipe(less({
      paths: [config.paths.src.styles]
    }).on('error', handleError))
    .pipe(prefixer())
    .pipe(gulpif(release, minifyCss()))
    .pipe(gulpif(!release, sourcemaps.write()))
    .pipe(gulpif(release, rename(config.filenames.release.styles), rename(config.filenames.build.styles)))
    .pipe(gulpif(release, gulp.dest(config.paths.dest.release.styles), gulp.dest(config.paths.dest.build.styles)));
});
