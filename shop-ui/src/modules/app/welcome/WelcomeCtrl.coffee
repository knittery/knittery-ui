'use strict'

class WelcomeCtrl
  constructor: (@Restangular) ->

  create: =>
    @Restangular.all('projects').post()
      .then((r) -> r.id)
      .then((id) => @message = "Created project #{id}.")

module.exports = (m) ->
  m.controller("WelcomeCtrl", ['Restangular', WelcomeCtrl])
