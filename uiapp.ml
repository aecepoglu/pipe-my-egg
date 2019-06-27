type 'a model_t = 'a
type 'm view_t = 'm -> unit
type ('e, 'm) update_t = 'm -> 'e -> 'm

class ['m, 'e] application model update view = object(_self)
  val mutable model : 'm model_t = model
  val view : 'm view_t = view
  val update : ('e, 'm) update_t = update

  method process msg =
    model <- update model msg;
    view model;
end

