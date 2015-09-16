'use strict';

var gulp = require('gulp');
var livereload = require('gulp-livereload');

module.exports = gulp.task('watch', function () {
  livereload.listen({
    port: config.ports.livereloadServer
  });
  gulp.watch(config.paths.src.livereload).on('change', function (file) {
    livereload.changed(file.path);
  });

  gulp.watch(config.paths.src.assets, ['assets']);
  gulp.watch(config.paths.src.scripts, ['lint']);
  gulp.watch(config.paths.src.index, ['index']);
  gulp.watch([config.paths.src.templates, config.paths.src.templatesHTML], ['templates']);
  gulp.watch(config.paths.src.stylesGlob, ['styles']);
  gulp.watch(config.paths.src.vendorStyles, ['styles-vendor']);
});
