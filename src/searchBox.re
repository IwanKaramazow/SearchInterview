type state = {
  searchText: string,
  inputRef: option(Dom.element)
};

let component = ReasonReact.statefulComponent("SearchBox");

let setInputRef (theRef, {ReasonReact.state: state}) =
  ReasonReact.SilentUpdate({...state, inputRef: Js.Null.to_opt(theRef)});

let handleChange (event, {ReasonReact.state: state}) = {
  let searchText = (ReactDOMRe.domElementToObj(ReactEventRe.Form.target(event)))##value;
  ReasonReact.Update({...state, searchText})
};

let make (:submit, _children) = {
  let handleKeyDown (event, {ReasonReact.state: state}) =
    switch (ReactEventRe.Keyboard.which(event)) {
    | 13 /* enter */ => submit(state.searchText)
    | _ => ()
    };
  let handleClick (_, {ReasonReact.state: state}) = submit(state.searchText);
  {
    ...component,
    initialState: () => {searchText: "", inputRef: None},
    didMount: ({ReasonReact.state: state}) => {
      switch (state.inputRef) {
      | Some(inputNode) => (ReactDOMRe.domElementToObj(inputNode))##focus()
      | None => ()
      };
      ReasonReact.NoUpdate
    },
    render: ({ReasonReact.state: state, update, handle}) =>
      <div>
        <input
          ref=(update(setInputRef))
          value=state.searchText
          onChange=(update(handleChange))
          onKeyDown=(handle(handleKeyDown))
        />
        <button onClick=(handle(handleClick))> (ReasonReact.stringToElement("Search")) </button>
      </div>
  }
};
