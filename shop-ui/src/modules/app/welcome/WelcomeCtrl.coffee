'use strict'

class WelcomeCtrl
  constructor: (@Projects, @Products) ->
    @products = @Products.list()

  create: (product) =>
    @Projects
    .create(product)
    .then((id) => @message = "Created project #{id}.")

module.exports = (m) ->
  m.controller("WelcomeCtrl", ['Projects', 'Products', WelcomeCtrl])
