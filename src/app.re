open Types;

type state = {
  counter: int,
  error: option(Error.t),
  cache: SearchCache.t,
  searchText: option(string)
};

type actions =
  | SubmitSearch(string)
  | ResponseSuccess(array(Company.t))
  | ResponseFailure(Error.t);

let component = ReasonReact.reducerComponent("App");

let handleResponse (resp) =
  try ({
         let newCompanies = Json.Decode.(array(Company.company_of_json, resp));
         ResponseSuccess(newCompanies)
       }) {
  | Json.Decode.DecodeError(_) => ResponseFailure(ResponseParseError)
  };

let handleNetworkFailure () = ResponseFailure(NetworkError);

let errorMessage (error) =
  switch (error) {
  | Some(error) =>
    let text = Error.string_of_error(error);
    <div> (ReasonReact.stringToElement(text)) </div>
  | None => ReasonReact.nullElement
  };

let searchCount (count) =
  <div> (ReasonReact.stringToElement("Total times searched: " ++ string_of_int(count))) </div>;

let submitSearch ({ReasonReact.state: state, reduce}, searchText) = {
  if (searchText !== "" && SearchCache.mem(searchText, state.cache) === false) {
    let _ =
      Js.Promise.(
        Api.search(searchText)
        |> then_((json) => reduce(handleResponse, json) |> resolve)
        |> catch((_) => reduce(handleNetworkFailure)() |> resolve)
      );
    ()
  };
  reduce(() => SubmitSearch(searchText))()
};

let make () = {
  ...component,
  initialState: () => {counter: 0, error: None, cache: SearchCache.init(), searchText: None},
  reducer: (action, state) =>
    switch (action) {
    | SubmitSearch(searchText) =>
      if (searchText !== "" && SearchCache.mem(searchText, state.cache) === false) {
        ReasonReact.Update({...state, searchText: Some(searchText)})
      } else {
        ReasonReact.NoUpdate
      }
    | ResponseFailure(error) =>
      ReasonReact.Update({...state, searchText: None, error: Some(error)})
    | ResponseSuccess(newCompanies) =>
      switch (state.searchText) {
      | Some(text) =>
        ReasonReact.Update(
          {
            ...state,
            counter: state.counter + 1,
            cache: SearchCache.put(text, newCompanies, state.cache),
            error: None
          }
        )
      | None => ReasonReact.NoUpdate
      }
    },
  render: ({ReasonReact.state: state} as self) =>
    <div>
      (searchCount(state.counter))
      <SearchBox submit=(submitSearch(self)) />
      (errorMessage(state.error))
      <Grid searchText=state.searchText cache=state.cache />
    </div>
};

ReactDOMRe.renderToElementWithId(ReasonReact.element(make()), "app");
