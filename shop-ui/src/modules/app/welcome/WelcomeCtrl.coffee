'use strict'

class WelcomeCtrl
  constructor: (@Projects, @Products, @$location) ->
    @products = @Products.list()

  create: (product) =>
    @message = 'Your project is being created...'
    @Projects
    .create(product)
    .then((id) => @$location.path("/project/#{id}"))

module.exports = (m) ->
  m.controller("WelcomeCtrl", ['Projects', 'Products', '$location', WelcomeCtrl])
