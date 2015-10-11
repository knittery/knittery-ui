module.exports = m = angular.module('knittery-shop.products.laptop-case', [
  'knittery-shop.products',
  'knittery-shop.products.utils'
])

defaultCheckerboard =
  size: 2
  color1: '#8925ad'
  color2: '#b5cf32'
  dissolveExponent: 1.5
  seed: Math.floor(Math.random() * 1000000000)

defaultGradient =
  color1: '#006fff'
  color2: '#ffffff'
  color3: '#cca219'
  seed: Math.floor(Math.random() * 1000000000)

m.config((ProductsProvider) ->
  ProductsProvider.register(
    name: 'laptopCase'
    label: 'Laptop Case' #TODO i18n
    newName: -> 'My Laptop Case'
    create: ->
      laptopCase:
        width: 25
        height: 36
        thickness: 1.5
        frontGap: 2
        lashLength: 10
        pattern:
          checkerboard: defaultCheckerboard
    settings: [
      {
        title: 'Measurements' #TODO i18n
        directive: 'laptop-case-settings-measurements'
      }
      {
        title: 'Lash' #TODO i18n
        directive: 'laptop-case-settings-lash'
      }
      {
        title: 'Pattern' #TODO i18n
        directive: 'laptop-case-settings-pattern'
      }
    ]
  )
)

m.directive('laptopCaseSettingsMeasurements', ->
  templateUrl: 'app/products/laptop-case/measurements.html'
  scope: {settings: '='}
  link: (scope) -> scope.m = scope.settings.laptopCase
)

m.directive('laptopCaseSettingsLash', ->
  templateUrl: 'app/products/laptop-case/lash.html'
  scope: {settings: '='}
  link: (scope) -> scope.m = scope.settings.laptopCase
)

m.directive('laptopCaseSettingsPattern', ->
  templateUrl: 'app/products/laptop-case/pattern.html'
  scope: {settings: '='}
  link: (scope) ->
    scope.checkerboard = angular.extend({}, defaultCheckerboard, scope.settings.laptopCase.pattern.checkerboard)
    scope.gradient = angular.extend({}, defaultGradient, scope.settings.laptopCase.pattern.gradient)

    scope.$watch('active', (active) ->
      if active == 'checkerboard'
        scope.settings.laptopCase.pattern =
          checkerboard: scope.checkerboard
      else if active == 'gradient'
        scope.settings.laptopCase.pattern =
          gradient: scope.gradient
    )

    scope.active = if scope.settings.laptopCase.pattern.checkerboard? then "checkerboard" else "gradient"
)
