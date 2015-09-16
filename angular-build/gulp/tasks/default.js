'use strict';

var gulp = require('gulp');
var runSequence = require('run-sequence');

module.exports = gulp.task('default', function () {
  if (release) {
    runSequence(
      'clean',
      ['index', 'styles-vendor', 'styles', 'images', 'assets', 'assets-vendor', 'templates', 'lint'],
      'browserify',
      ['minify', 'serve']
    );
  } else {
    runSequence(
      'clean',
      ['index', 'styles-vendor', 'styles', 'images', 'assets', 'assets-vendor', 'templates', 'lint'],
      'browserify-vendor',
      ['watchify', 'watch', 'serve']
    );
  }
});
