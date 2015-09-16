'use strict';

var gulp = require('gulp');
var karma = require('gulp-karma');
var coffee = require('gulp-coffee');

gulp.task('test-scripts', function() {
  //Javascript
  gulp.src(config.paths.test.scripts + '/**/*.js')
    .pipe(gulp.dest(config.paths.dest.test));

  //Coffeescript
  gulp.src(config.paths.test.scripts + '/**/*.coffee')
    .pipe(coffee())
    .pipe(gulp.dest(config.paths.dest.test))
});

gulp.task('test', ['browserify', 'test-scripts'], function () {
  return gulp.src('./use-from-karma-conf') // use from karma.conf.js instead
    .pipe(karma({
      configFile: 'karma.conf.js',
      action: 'run'
    }))
    .on('error', function (err) {
      this.emit('end');
    });
});

gulp.task('autotest', ['test'], function () {
  return gulp.watch(['src/**/*.js', 'src/**/*.coffee', 'test/**/*Spec.js', 'test/**/*Spec.coffee'], ['test']);
});
