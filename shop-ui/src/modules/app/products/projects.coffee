'use strict'

module.exports = (m) ->
  m.service('Projects', (Restangular) ->
    create: (product, name = product.newName()) ->
      settings = product.create()
      Restangular.all('projects')
      .post().get('id')
      .then((id) ->
        project = Restangular.one('projects', id)
        project.name = name
        project.product = settings
        project.put().then(-> id)
      )
  )
