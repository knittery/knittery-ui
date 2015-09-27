'use strict'

m = angular.module('knittery-shop.overview', [])
module.exports = m

m.config(($routeProvider) ->
  $routeProvider.when('/overview',
    templateUrl: 'app/overview/overview.html'
  )
)
