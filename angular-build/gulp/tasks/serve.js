'use strict';

var gulp = require('gulp');
var connect = require('connect');
var serveStatic = require('serve-static');
var rewriteModule = require('http-rewrite-middleware');

var staticServer = connect();

module.exports = gulp.task('serve', function (next) {
  var staticServerPath = BUILD_FOLDER;
  if (release)
    staticServerPath = RELEASE_FOLDER;

  staticServer
    .use(rewriteModule.getMiddleware([
      {from: '^$', to: '/ui/', redirect: 'temporary'},
      {from: '^/$', to: '/ui/', redirect: 'temporary'},
      {from: '^/ui$', to: '/index.html'},
      {from: '^/ui/.*$', to: '/index.html'}
    ]))
    .use(serveStatic(staticServerPath))
    .listen(process.env.PORT || config.ports.staticServer, next);
});
