'use strict'

_ = require('underscore')

class ProjectCtrl
  constructor: (@Projects, @Products, $routeParams, @$scope, $interval) ->
    id = $routeParams.projectId
    @updating = true
    @Projects.get(id).then((project) =>
      @project = project
      productType = _.keys(project.product)[0]
      @product = @Products.get(productType)
      @Projects.getKnitting(id).then((knitting) =>
        @knitting = knitting
        @updating = false
      )).catch(@onError)
    stopTimer = $interval(@checkForChange, 1000)
    @$scope.$on('$destroy', -> $inverval.cancel(stopTimer))

  checkForChange: =>
    if not @updating and (@$scope.settings.$dirty or @error)
      console.log('Triggering live update')
      @update()

  onError: (e) =>
    @error =
      if e?.data?.errors?.length > 0 then e.data.errors[0].title
      else e.statusText
    console.log(@error)

  state: =>
    if @error then 'error'
    else if @updating then 'updating'
    else if @$scope.settings.$dirty then 'dirty'
    else 'upToDate'

  update: =>
    @updating = true
    @knitting = null
    @error = null
    @project.put().then(=>
      @$scope.settings.$setPristine()
      @Projects.getKnitting(@project.id).then((knitting) =>
        @knitting = knitting
        @updating = false
      )).catch(@onError)

module.exports = (m) ->
  m.controller("ProjectCtrl", ['Projects', 'Products', '$routeParams', '$scope', '$interval', ProjectCtrl])
