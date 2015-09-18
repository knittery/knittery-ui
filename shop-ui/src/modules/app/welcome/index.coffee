'use strict'

m = angular.module('knittery-shop.welcome', [])
module.exports = m

m.config(($routeProvider) ->
  $routeProvider.when('/welcome',
    templateUrl: 'app/welcome/welcome.html'
  )
)

require('./WelcomeCtrl')(m)
