'use strict'

_ = require('underscore')

class ProjectCtrl
  constructor: (@Projects, @Products, $routeParams) ->
    id = $routeParams.projectId
    @Projects.get(id).then((project) =>
      @project = project
      productType = _.keys(project.product)[0]
      @product = @Products.get(productType)
    )

module.exports = (m) ->
  m.controller("ProjectCtrl", ['Projects', 'Products', '$routeParams', ProjectCtrl])
