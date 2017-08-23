open Bs_fetch;

let baseUrl = "https://autocomplete.clearbit.com/v1/companies/suggest?query=";

let search (searchQuery) = {
  /* https://autocomplete.clearbit.com/v1/companies/suggest?query=piesync  */
  let apiUrl = baseUrl ++ searchQuery;
  Js.Promise.(fetch(apiUrl) |> then_(Response.json))
};


