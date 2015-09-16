'use strict';

var gulp = require('gulp');
var fs = require('fs');
var path = require('path');
var argv = require('yargs').argv;
var tasks = fs.readdirSync(path.join(__dirname, 'tasks'));

require('./config');

// --release flag when executing a task
global.release = argv.release;

tasks.forEach(function (task) {
  require('./tasks/' + task);
});

module.exports = gulp
