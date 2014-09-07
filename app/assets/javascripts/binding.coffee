###
  Model that triggers jQuery events when changed.
  Usage example:
    m = model("firstName", "lastName", "age", "country")
    m.derived("name", ["firstName", "lastName"], (first, last) ->first + " " + last)

  Extension for jQuery to link model properties to jQuery element.
  Usage examples:
   - $("h1").link().text(model, "title")
   - $("h1").link().attr("class")(model, "something")
   - $("h1").link().switchClass("highlight")(model, "something", (v) -> v > 10)
###
define(["jquery"], ($) ->
  model = (properties...) ->
    model = $({})
    addProperties(model, properties)
    model.derived = addDerived(model)
    model

  addProperties = (obj, properties) ->
    addProperty(obj, name) for name in properties

  addProperty = (obj, name) ->
    value = undefined
    Object.defineProperty(obj, name,
      get: -> value
      set: (v) ->
        value = v
        obj.trigger(name + ":change", [v, obj])
        obj.trigger("change", [obj, name])
      enumerable: true
      configurable: true
    )

  addDerived = (obj) -> (name, dependsOn, fun) ->
    me = this
    if (!typeIsArray(dependsOn)) then dependsOn = [dependsOn]
    Object.defineProperty(obj, name,
      get: ->
        args = (obj[p] for p in dependsOn)
        fun.apply(me, args)
      enumerable: true
      configurable: true
    )
    for p in dependsOn
      obj.bind(p + ":change", -> obj.trigger(name + ":change", [obj[name], name]))
    obj

  linkFun = (to, property, f) ->
    to.bind(property + ":change", (e, newValue) ->
      f.apply(this, [to[property]])
    )
    f.apply(this, [to[property]])

  typeIsArray = Array.isArray || (value) -> return {}.toString.call(value) is '[object Array]'
  undefinedToEmpty = (v) -> if v? then v else ""

  $.fn.extend({
    link: ->
      me = this
      obj =
        fun: linkFun
        text: (to, property, transform = undefinedToEmpty) ->
          linkFun(to, property, (v) -> me.text(transform(v)))
        width: (to, property, unit = "", transform = undefinedToEmpty) ->
          linkFun(to, property, (v) -> me.width(transform(v) + unit))
        attr: (attr) -> (to, property, transform = undefinedToEmpty) ->
          linkFun(to, property, (v) -> me.attr(attr, transform(v)); me.trigger(attr + ":attr"))
        data: (data) -> (to, property, transform = undefinedToEmpty) ->
          linkFun(to, property, (v) -> me.data(data, transform(v)); me.trigger(data + ":data"))
        switchClass: (className) -> (to, property, f = (v) -> v) ->
          linkFun(to, property, (v) ->
            if (f(v)) then me.addClass(className)
            else me.removeClass(className)
          )
        disabled: (to, property, transform = (v) -> v) ->
          f = (v) -> if (transform(v)) then me.attr("disabled", "disabled") else me.removeAttr("disabled")
          linkFun(to, property, f)
      obj
  })

  model
)