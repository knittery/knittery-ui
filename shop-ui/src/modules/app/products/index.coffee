'use strict'

module.exports = m = angular.module('knittery-shop.products', [])

require('./projects')(m)
require('./products')(m)

require('./laptop-case')(m)
