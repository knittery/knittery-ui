require('angular-minicolors')
require('jquery-minicolors')
require('ngjs-color-picker')

module.exports = m = angular.module('knittery-shop.products.utils', ['minicolors', 'ngjsColorPicker'])

m.directive('lengthInput', ->
  require: 'ng-model'
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
  require: 'ng-model'
  scope:
    label: '@'
    id: '@'
    colorOptions: '='
    ngModel: '='
  template: """
    <div class="form-group">
      <div class="col-sm-4">
        <label for="{{id}}" class="control-label">{{label}}</label>
      </div>
      <div class="col-sm-8">
            <ngjs-color-picker selected="selected" options="colorOptions"></ngjs-color-picker>
      </div>
    </div>
    """
  link: (scope, elem, attrs) ->

)


