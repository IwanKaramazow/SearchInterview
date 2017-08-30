module Company = {
  type t = {
    logo: string,
    name: string,
    domain: string
  };
  let company_of_json (json) =
    Json.Decode.{
      logo: json |> field("logo", string),
      name: json |> field("name", string),
      domain: json |> field("domain", string)
    };
};

module Error = {
  type t =
    | ResponseParseError
    | NetworkError;
  let string_of_error (error) =
    switch (error) {
    | ResponseParseError => "There seems to be some trouble with the data we received from Clearbit. You should probably contact the person who made this thing."
    | NetworkError => "We're having trouble connecting to the Clearbit API. Are you connected to the internet?"
    };
};

module SearchCache: {
  type t;
  let find: (string, t) => option(array(Company.t));
  let put: (string, array(Company.t), t) => t;
  let mem: (string, t) => bool;
  let init: unit => t;
} = {
  module Backend = Map.Make(String);
  type t = Backend.t(array(Company.t));
  let find (key, cache) =
    try ({
           let companies = Backend.find(key, cache);
           Some(companies)
         }) {
    | _ => None
    };
  let put (key, companies, cache) = Backend.add(key, companies, cache);
  let mem = Backend.mem;
  let init () = Backend.empty;
};

