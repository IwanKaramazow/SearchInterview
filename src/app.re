open Types;

type state = {
  companies: Js.Array.t(Company.t),
  counter: int,
  error: option(Error.t),
  cache: SearchCache.t,
  searchText: string
};

let component = ReasonReact.statefulComponent("App");

let handleResponse (resp, {ReasonReact.state: state}) =
  try {
         let newCompanies = Json.Decode.(array(Company.company_of_json, resp));
         ReasonReact.Update(
           {
             ...state,
             cache: SearchCache.put(state.searchText, newCompanies, state.cache),
             companies: Js.Array.concat(state.companies, newCompanies),
             error: None
           }
         )
       } {
  | Json.Decode.DecodeError(message) =>
    Js.log(message);
    ReasonReact.Update({...state, error: Some(ResponseParseError)})
  };

let handleNetworkFailure ((), {ReasonReact.state: state}) =
  ReasonReact.Update({...state, error: Some(NetworkError)});

let search (searchText, {ReasonReact.state: state, update}) =
  if (searchText !== "") {
    let _ =
      Js.Promise.(
        Api.search(searchText)
        |> then_(
             (json) =>
               {
                 let () = Js.log(json);
                 update(handleResponse, json)
               }
               |> resolve
           )
        |> catch((_) => update(handleNetworkFailure)() |> resolve)
      );
    ReasonReact.Update({...state, searchText})
  } else {
    ReasonReact.NoUpdate
  };

let renderErrorMessage (error) =
  switch (error) {
  | Some(error) =>
    let text = Error.string_of_error(error);
    <div> (ReasonReact.stringToElement(text)) </div>
  | None => ReasonReact.nullElement
  };

let make () = {
  ...component,
  initialState: () => {
    companies: [||],
    counter: 0,
    error: None,
    cache: SearchCache.init(),
    searchText: ""
  },
  render: ({ReasonReact.state: state, update}) =>
    <div>
      <SearchBox submit=(update(search)) />
      (renderErrorMessage(state.error))
      <Grid
        searchText=state.searchText
        results=(SearchCache.find(state.searchText, state.cache))
      />
    </div>
};

ReactDOMRe.renderToElementWithId(ReasonReact.element(make()), "app");
