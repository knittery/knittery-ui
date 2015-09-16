'use strict'

require 'angular-route'

module.exports = angular.module('knittery-shop.infrastructure.route', ['ngRoute'])

module.exports.config(($routeProvider) ->
## More routes will be provided by the app modules. ###
  $routeProvider
  .when('/', redirectTo: '/welcome')
  .otherwise({redirectTo: '/welcome'}))
.config(($locationProvider) ->
  $locationProvider.html5Mode({
    enabled: true,
  }))
