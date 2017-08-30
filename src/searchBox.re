type state = {
  searchText: string,
  mutable inputRef: option(Dom.element)
};

type actions =
  | Search(string);

let component = ReasonReact.reducerComponent("SearchBox");

/* let setInputRef (theRef) = state.inputRef = Js.Null.to_opt(theRef); */
let handleChange (event) = {
  let searchText = (ReactDOMRe.domElementToObj(ReactEventRe.Form.target(event)))##value;
  Search(searchText)
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
    reducer: (action, state) =>
      switch (action) {
      | Search(text) => ReasonReact.Update({...state, searchText: text})
      },
    render: ({ReasonReact.state: state, reduce, handle}) =>
      <div>
        <input
          ref=((inputNode) => state.inputRef = Js.Null.to_opt(inputNode))
          value=state.searchText
          onChange=(reduce(handleChange))
          onKeyDown=(handle(handleKeyDown))
        />
        <button onClick=(handle(handleClick))> (ReasonReact.stringToElement("Search")) </button>
      </div>
  }
};
