'use strict'

require('angular-bootstrap')
require('restangular')
global._ = require('underscore')

module.exports = m = angular.module('knittery-shop.infrastructure', [
  'ui.bootstrap',
  'restangular',

  require('./route').name
])


m.config((RestangularProvider) ->
  RestangularProvider.setBaseUrl('http://localhost:9000/api/v1')
)
