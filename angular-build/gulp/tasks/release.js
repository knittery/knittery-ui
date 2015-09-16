'use strict';

var gulp = require('gulp');
var runSequence = require('run-sequence');

module.exports = gulp.task('release', function () {
  global.release = true;
  runSequence(
    'clean',
    ['index', 'styles-vendor', 'styles', 'images', 'assets', 'templates', 'lint'],
    'browserify',
    'minify'
  );
});
