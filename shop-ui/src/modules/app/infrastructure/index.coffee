'use strict'

require('angular-bootstrap')

module.exports = angular.module('knittery-shop.infrastructure', [
  'ui.bootstrap',

  require('./route').name
])
