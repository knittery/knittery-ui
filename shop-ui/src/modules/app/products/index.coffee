'use strict'

module.exports = m = angular.module('knittery-shop.products', [])

require('./products')(m)
require('./settings-section')(m)

require('./laptop-case')(m)
