'use strict'

class ProjectCtrl
  constructor: (@Projects, $routeParams) ->
    id = $routeParams.projectId
    @Projects.get(id).then((project) =>
      @project = project
    )

module.exports = (m) ->
  m.controller("ProjectCtrl", ['Projects', '$routeParams', ProjectCtrl])
