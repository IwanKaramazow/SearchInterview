open Types;

type hoveredCompany = option(string);

let component = ReasonReact.statefulComponent("Grid");

let gridItem (hoveredCompany, company, handleHoverStart, handleHoverEnd) = {
  open Company;
  let {domain, name, logo} = company;
  <div
    key=domain
    onMouseEnter=((_) => handleHoverStart(name))
    onMouseLeave=((_) => handleHoverEnd(name))>
    <a href=domain target="new"> <img src=logo /> </a>
    (
      switch (hoveredCompany) {
      | Some(c) =>
        if (c === name) {
          <span> (ReasonReact.stringToElement(name)) </span>
        } else {
          ReasonReact.nullElement
        }
      | None => ReasonReact.nullElement
      }
    )
  </div>
};

let hoverStart (companyName, _self) = ReasonReact.Update(Some(companyName));

let hoverEnd (_, _self) = ReasonReact.Update(None);

let gridStyle = ReactDOMRe.Style.make(:display "flex")();

let make (:searchText, :cache, _children) = {
  ...component,
  initialState: () => None,
  render: ({ReasonReact.state: state, update}) => {
    let content =
      switch (searchText) {
      | Some(text) =>
        switch (SearchCache.find(text, cache)) {
        | Some(companies) =>
          Js.Array.length(companies) > 0 ?
            {
              let elements =
                Js.Array.map(
                  (company) => gridItem(state, company, update(hoverStart), update(hoverEnd)),
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
