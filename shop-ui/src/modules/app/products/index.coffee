'use strict'

m = angular.module('knittery-shop.products', [])

require('./products')(m)
require('./settings-section')(m)

module.exports = m2 = angular.module('knittery-shop.products.all', [
  'knittery-shop.products',
  require('./utils').name
  require('./laptop-case').name
])
