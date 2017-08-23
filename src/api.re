let baseUrl = "https://autocomplete.clearbit.com/v1/companies/suggest?query=";

let search (searchQuery) = {
  /* https://autocomplete.clearbit.com/v1/companies/suggest?query=searchQueryComesHere  */
  let apiUrl = baseUrl ++ searchQuery;
  Js.Promise.(Bs_fetch.fetch(apiUrl) |> then_(Bs_fetch.Response.json))
};
