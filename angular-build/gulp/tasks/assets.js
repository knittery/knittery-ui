'use strict';

var gulp = require('gulp');
var gulpif = require('gulp-if');
var fs = require('fs');
var path = require('path');

module.exports = gulp.task('assets', function () {
  return gulp.src(config.paths.src.assets)
    .pipe(gulpif(release, gulp.dest(config.paths.dest.release.assets), gulp.dest(config.paths.dest.build.assets)));
});

var npmDeps = Object.keys(config.packageJson.dependencies);

module.exports = gulp.task('assets-vendor', function () {
  npmDeps.forEach(function (module) {
    var pModule = path.join(config.paths.src.node_modules, module);
    var pDist = path.join(pModule, 'dist');

    var hasDist = fs.existsSync(pDist);
    var globs = [
      '/**/*.{eot,svg,ttf,woff,woff2}', // fonts
      '/**/*.{gif,jpg,png}' // images
    ].map(function (glob) {
        return hasDist ? path.join(pDist, glob) : path.join(pModule, glob);
      });

    var dest;
    if (hasDist) {
      dest = release
        ? path.join(config.paths.dest.release.vendorAssets, module, "dist")
        : path.join(config.paths.dest.build.vendorAssets, module, "dist");
    } else {
      dest = release
        ? path.join(config.paths.dest.release.vendorAssets, module)
        : path.join(config.paths.dest.build.vendorAssets, module);
    }

    gulp.src(globs).pipe(gulp.dest(dest));

  });
});
