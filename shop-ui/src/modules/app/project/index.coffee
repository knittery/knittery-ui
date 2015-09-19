'use strict'

module.exports = m = angular.module('knittery-shop.project', [])


m.config(($routeProvider) ->
  $routeProvider.when('/project/:projectId',
    templateUrl: 'app/project/project.html'
  )
)

require('./projects')(m)

require('./ProjectCtrl')(m)
