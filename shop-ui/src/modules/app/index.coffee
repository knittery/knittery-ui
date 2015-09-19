'use strict'

module.exports = angular.module('knittery-shop', [
  require('../../../tmp/templates/templates').name,
  require('./infrastructure').name,

  require('./products').name
  require('./welcome').name
])
