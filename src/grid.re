open Types;

type state = option(string);

type actions =
  | HoverStart(string)
  | HoverEnd;

let component = ReasonReact.reducerComponent("Grid");

let gridItem (hoveredCompany, company, handleHoverStart, handleHoverEnd) = {
  open Company;
  let {domain, name, logo} = company;
  <div
    key=domain
    onMouseEnter=((_) => handleHoverStart(domain))
    onMouseLeave=((_) => handleHoverEnd(domain))
    style=(ReactDOMRe.Style.make(:display "flex", :flexDirection "column")())>
    <a href=domain target="new"> <img src=logo /> </a>
    (
      switch (hoveredCompany) {
      | Some(c) =>
        if (c === domain) {
          <span> (ReasonReact.stringToElement(name)) </span>
        } else {
          ReasonReact.nullElement
        }
      | None => ReasonReact.nullElement
      }
    )
  </div>
};

let hoverStart (companyName) = HoverStart(companyName);

let hoverEnd (_) = HoverEnd;

let gridStyle = ReactDOMRe.Style.make(:display "flex", :justifyContent "space-between")();

let make (:searchText, :cache, _children) = {
  ...component,
  initialState: () => None,
  reducer: (action, _state) =>
    switch (action) {
    | HoverStart(companyName) => ReasonReact.Update(Some(companyName))
    | HoverEnd => ReasonReact.Update(None)
    },
  render: ({ReasonReact.state: state, reduce}) => {
    let content =
      switch (searchText) {
      | Some(text) =>
        switch (SearchCache.find(text, cache)) {
        | Some(companies) =>

          Js.Array.length(companies) > 0 ?
            {
              let elements =
                Js.Array.map(
                  (company) => gridItem(state, company, reduce(hoverStart), reduce(hoverEnd)),
                  companies
                );
              ReasonReact.arrayToElement(elements)
            } :
            ReasonReact.stringToElement("No search results found...")
        | None => ReasonReact.stringToElement("Loading...")
        }
      | None => ReasonReact.nullElement
      };
    <div style=gridStyle> content </div>
  }
};
