require('angular-minicolors')
require('jquery-minicolors')
require('ngjs-color-picker')

module.exports = m = angular.module('knittery-shop.products.utils', ['minicolors', 'ngjsColorPicker'])

m.directive('lengthInput', ->
  require: 'ngModel'
  scope:
    label: '@'
    id: '@'
    ngModel: '='
  template: """
    <div class="form-group">
      <div class="col-sm-4">
        <label for="{{id}}" class="control-label">{{label}}</label>
      </div>
      <div class="col-sm-8">
        <div class="input-group">
          <input type="number" class="form-control input-sm text-right" id="{{id}}" placeholder="{{label}}"
                 ng-model="ngModel" ng-pattern="/^[0-9]+(\.[0-9]{1,2})?$/" step="0.1" >
          <div class="input-group-addon">cm</div>
        </div>
      </div>
    </div>
    """
)

m.directive('colorInput', ->
  require: 'ngModel'
  scope:
    label: '@'
    id: '@'
    colorOptions: '='
  template: """
    <div class="form-group">
      <div class="col-sm-4">
        <label for="{{id}}" class="control-label">{{label}}</label>
      </div>
      <div class="col-sm-8">
            <ngjs-color-picker selected="selected" options="colorOptions" ng-click="changed"></ngjs-color-picker>
      </div>
    </div>
    """
  link: (scope, elem, attrs, ngModel) ->
    updating = false
    ngModel.$render = ->
      updating = true
      scope.selected = ngModel.$viewValue

    scope.$watch('selected', (value) ->
      if updating
        updating = false
      else
        console.log("Value changed to #{value}")
        ngModel.$setViewValue(value))
)


