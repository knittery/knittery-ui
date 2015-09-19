'use strict'

_ = require('underscore')

module.exports = (m) ->
  m.provider('Products', ->
    products = []

    ###
      Required interface:
      - name: string
      - label: string, i18n key
      - newName: () -> name for newly created product
      - create: () -> new settings json
      - settings: [ settings panel
            - title: string, i18n key of the settings panel
            - directive: string, name of the directive that renders the settings
          ]
    ###
    register: (product) ->
      products.push(product)
    $get: ->
      list: -> angular.copy(products)
      get: (name) -> _.find(products, (p) -> p.name == name)
  )
