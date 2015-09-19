'use strict'

module.exports = m = angular.module('knittery-shop.products', [])

require('./products')(m)

require('./laptop-case')(m)
