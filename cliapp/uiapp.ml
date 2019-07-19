type 'a model_t = 'a
type 'm view_t = 'm -> unit
type ('m, 'e) update_t = 'm -> 'e -> 'm

module App = struct
  type ('m, 'e) t = {
    mutable model: 'm model_t;
    update: ('m, 'e) update_t;
    view: 'm view_t
  }

  let init (m:'a) (u: 'a -> 'e -> 'a) (v: 'a -> unit) :('a, 'e) t = {
    model = m;
    update = u;
    view = v;
  }

  let process_event t ev =
    t.model <- t.update t.model ev;
    t.view t.model
end

