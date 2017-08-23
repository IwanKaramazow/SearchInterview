open Types;

type state = {
  counter: int,
  error: option(Error.t),
  cache: SearchCache.t,
  searchText: option(string)
};

let component = ReasonReact.statefulComponent("App");

let handleResponse (resp, {ReasonReact.state: state}) =
  try ({
         let newCompanies = Json.Decode.(array(Company.company_of_json, resp));
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
       }) {
  | Json.Decode.DecodeError(_) =>
    ReasonReact.Update({...state, searchText: None, error: Some(ResponseParseError)})
  };

let handleNetworkFailure ((), {ReasonReact.state: state}) =
  ReasonReact.Update({...state, searchText: None, error: Some(NetworkError)});

let search (searchText, {ReasonReact.state: state, update}) =
  if (searchText !== "" && SearchCache.mem(searchText, state.cache) === false) {
    let _ =
      Js.Promise.(
        Api.search(searchText)
        |> then_((json) => update(handleResponse, json) |> resolve)
        |> catch((_) => update(handleNetworkFailure)() |> resolve)
      );
    ReasonReact.Update({...state, searchText: Some(searchText)})
  } else {
    ReasonReact.NoUpdate
  };

let errorMessage (error) =
  switch (error) {
  | Some(error) =>
    let text = Error.string_of_error(error);
    <div> (ReasonReact.stringToElement(text)) </div>
  | None => ReasonReact.nullElement
  };

let searchCount (count) =
  <div> (ReasonReact.stringToElement("Total times searched: " ++ string_of_int(count))) </div>;

let make () = {
  ...component,
  initialState: () => {counter: 0, error: None, cache: SearchCache.init(), searchText: None},
  render: ({ReasonReact.state: state, update}) =>
    <div>
      (searchCount(state.counter))
      <SearchBox submit=(update(search)) />
      (errorMessage(state.error))
      <Grid searchText=state.searchText cache=state.cache />
    </div>
};

ReactDOMRe.renderToElementWithId(ReasonReact.element(make()), "app");
