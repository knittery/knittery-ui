'use strict';

var gulp = require('gulp');
var gulpif = require('gulp-if');
var templateCache = require('gulp-angular-templatecache');
var header = require('gulp-header');
var minifyHTML = require('gulp-minify-html');


module.exports = gulp.task('templates', function () {
  return gulp.src([config.paths.src.templates, config.paths.src.templatesHTML])
    .pipe(gulpif(release, minifyHTML({empty: true, spare: true, quotes: true})))
    .pipe(templateCache({ standalone: true }))
    .pipe(header('module.exports = '))
    .pipe(gulp.dest(config.paths.src.templatesCompiled));
});
