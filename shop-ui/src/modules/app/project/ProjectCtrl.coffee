'use strict'

_ = require('underscore')

class ProjectCtrl
  constructor: (@Projects, @Products, $routeParams) ->
    id = $routeParams.projectId
    @updating = true
    @Projects.get(id).then((project) =>
      @project = project
      productType = _.keys(project.product)[0]
      @product = @Products.get(productType)
      @Projects.getKnitting(id).then((knitting) =>
        @knitting = knitting
        @updating = false
      ))

  update: =>
    @updating = true
    @knitting = null
    @project.put().then(=>
      @Projects.getKnitting(@project.id).then((knitting) =>
        @knitting = knitting
        @updating = false
      ))

module.exports = (m) ->
  m.controller("ProjectCtrl", ['Projects', 'Products', '$routeParams', ProjectCtrl])
