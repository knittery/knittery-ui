jQuery.fn.extend({
  ###
    Model that triggers jQuery events when changed.
    Usage example: $().model("name", "age", "country")
  ###
  model: (properties...) ->
    model = $({})
    addProperties(model, properties)
    model

  ###
    Links model properties to an aspect of a jQuery element.
    Usage examples:
     - $("h1").link().text(model, "title")
     - $("h1").link().attr("class")(model, "something")
     - $("h1").link().triggerClass("highlight")(model, "something", (v) -> v > 10)
  ###
  link: ->
    me = this
    {
      fun: linkFun
      text: (to, property, transform = undefinedToEmpty) ->
        linkFun(to, property, (v) -> me.text(transform(v)))
      attr: (attr) -> (to, property, transform = undefinedToEmpty) ->
        linkFun(to, property, (v) -> me.attr(attr, transform(v)))
      triggerClass: (className) -> (to, property, f = (v) -> v) ->
        linkFun(to, property, (v) ->
          if (f(v)) then me.addClass(className)
          else me.removeClass(className)
        )
    }
})

addProperties = (obj, properties) ->
  addProperty(obj, name) for name in properties
  
addProperty = (obj, name) ->
  value = undefined
  Object.defineProperty(obj, name,
    get: -> value
    set: (v) ->
      value = v
      obj.trigger(name+":change", [v, obj])
      obj.trigger("change", [obj, name])
    enumerable: true
    configurable: true
  )

linkFun = (to, property, f) ->
  to.bind(property+":change", (e, newValue) ->
    f.apply(this, [to[property]])
  )
  f.apply(this, [to[property]])

undefinedToEmpty = (v) -> if v? then v else ""
  