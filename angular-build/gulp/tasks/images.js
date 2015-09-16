'use strict';

var gulp = require('gulp');
var gulpif = require('gulp-if');

module.exports = gulp.task('images', function () {
  return gulp.src(config.paths.src.images)
    .pipe(gulpif(release, gulp.dest(config.paths.dest.release.images), gulp.dest(config.paths.dest.build.images)));
});
