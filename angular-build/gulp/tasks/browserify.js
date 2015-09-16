'use strict';

var gulp = require('gulp');
var gulpif = require('gulp-if');
var browserify = require('browserify');
var source = require('vinyl-source-stream');
var coffeeify = require('coffeeify');
var ngAnnotate = require('browserify-ngannotate');
var transform = require('vinyl-transform');
var exorcist = require('exorcist');
var path = require('path');

var libs =
  Object.keys(config.packageJson.dependencies).filter(function (entry) {
    var pkg = requireFromProject('node_modules/' + entry + '/package.json');
    return pkg.main
  });

gulp.task('browserify-vendor', function () {
  return browserify({
    require: libs,
    debug: !release
  })
    .bundle()
    .pipe(gulpif(release, source(config.filenames.release.vendorScripts), source(config.filenames.build.vendorScripts)))
    .pipe(gulpif(!release, transform(function () {
      return exorcist(path.join(config.paths.dest.build.scripts, config.filenames.build.vendorScripts + '.map'));
    })))
    .pipe(gulpif(release, gulp.dest(config.paths.dest.release.scripts), gulp.dest(config.paths.dest.build.scripts)));
});

gulp.task('browserify', ['browserify-vendor', 'templates'], function () {
  return browserify({
    entries: [config.paths.src.modules],
    extensions: ['.coffee'],
    external: libs
  })
    .transform(coffeeify)
    .transform(ngAnnotate, {add: true, remove: false, ext: ".coffee"})
    .bundle()
    .pipe(source(config.filenames.release.scripts))
    .pipe(gulp.dest(config.paths.dest.release.scripts));
});

module.exports.libs = libs;
