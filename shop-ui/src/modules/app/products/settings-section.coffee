module.exports = (m) ->
  m.directive('settingsSection', ($compile) ->
    restrict: 'E'
    scope:
      settingType: '='
      settings: '='
    link: (scope, element) ->
      directive = scope.settingType.directive
      template = """<#{directive} settings="settings"></#{directive}>"""
      compiled = $compile(template)(scope)
      element.replaceWith(compiled)
  )
