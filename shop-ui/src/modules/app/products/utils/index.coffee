module.exports = m = angular.module('knittery-shop.products.utils', [])

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
                 ng-model="ngModel">
          <div class="input-group-addon">cm</div>
        </div>
      </div>
    </div>
    """
)
