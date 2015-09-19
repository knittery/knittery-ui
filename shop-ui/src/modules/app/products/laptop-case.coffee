module.exports = (m) ->
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
            checkerboard:
              size: 2
              color1: '#ff0000'
              color2: '#0000ff'
              dissolveExponent: 1.5
              seed: Math.floor(Math.random() * 1000000000)
      settings: [
        {
          title: 'Measurements' #TODO i18n
          directive: 'laptop-case-settings-measurements'
        }
      ]
    )
  )

  m.directive('laptopCaseSettingsMeasurements', ->
  )
