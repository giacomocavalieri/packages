import edit_distance
import ethos.{type BagTable}
import gleam/bool
import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option
import gleam/order
import gleam/result
import gleam/string
import packages/error.{type Error}
import packages/storage
import porter_stemmer

pub opaque type TextSearchIndex {
  TextSearchIndex(table: BagTable(String, String))
}

pub fn new() -> TextSearchIndex {
  TextSearchIndex(ethos.new())
}

pub fn insert(
  index: TextSearchIndex,
  name name: String,
  description description: String,
) -> Result(Nil, Error) {
  case storage.is_ignored_package(name) {
    True -> Ok(Nil)
    False ->
      { name <> " " <> string.replace(name, "_", " ") <> " " <> description }
      |> stem_words
      |> list.try_each(fn(word) { ethos.insert(index.table, word, name) })
      |> result.replace_error(error.EtsTableError)
  }
}

pub fn update(
  index: TextSearchIndex,
  name name: String,
  description description: String,
) -> Result(Nil, Error) {
  use _ <- result.try(remove(index, name))
  insert(index, name, description)
}

pub type LookupOutcome {
  Packages(packages: List(String))
  DidYouMean(suggestion: String)
}

pub fn lookup(
  index: TextSearchIndex,
  phrase: String,
) -> Result(LookupOutcome, Error) {
  let phrase = string.lowercase(phrase)

  stem_words(phrase)
  |> list.flat_map(expand_search_term)
  |> list.try_map(ethos.get(index.table, _))
  |> result.replace_error(error.EtsTableError)
  |> result.try(fn(packages_names) {
    case rank_results(list.flatten(packages_names), phrase) {
      [_, ..] as packages -> Ok(Packages(packages:))
      // If there's no results matching the given query we try and guess what
      // the user might have wanted to type.
      [] -> {
        use words <- result.try(
          ethos.keys(index.table)
          |> result.replace_error(error.EtsTableError),
        )
        case did_you_mean(given: phrase, one_of: words) {
          Ok(suggestion) -> Ok(DidYouMean(suggestion:))
          Error(_) -> Ok(Packages(packages: []))
        }
      }
    }
  })
}

fn did_you_mean(
  given phrase: String,
  one_of words: List(String),
) -> Result(String, Nil) {
  // We want to limit the maximum edit distance. Otherwise we could end up
  // suggesting fixes that are not related at all to the original query.
  let phrase_length = string.length(phrase)
  let limit = int.max(1, phrase_length) / 3

  list.filter_map(words, fn(word) {
    let word_length = string.length(word)
    let minimum_distance = int.absolute_value(word_length - phrase_length)

    // If the minimum distance is greater than the allowed limit then we don't
    // even waste any time computing the edit distance of the two strings!
    use <- bool.guard(when: minimum_distance > limit, return: Error(Nil))
    let distance = edit_distance.levenshtein(phrase, word)
    case distance > limit {
      True -> Error(Nil)
      False -> Ok(#(word, distance))
    }
  })
  // We only pick the word with the smallest possible edit distance that's below
  // the given threshold
  |> list.sort(fn(one, other) { int.compare(one.1, other.1) })
  |> list.first
  |> result.map(fn(suggestion) { suggestion.0 })
}

/// Given a list with packages matching the searched phrase we assign them a
/// ranking and return them in a list with no duplicates sorted from best match
/// to worst match.
///
fn rank_results(packages: List(String), phrase: String) -> List(String) {
  let ranking =
    // Each package starts with a score that is the number of times it appears
    // in the search results coming from `ethos`.
    count_occurrences(packages)
    |> dict.to_list
    |> list.map(fn(pair) {
      // We then tweak the ranking to prioritise some official Gleam packages.
      let #(package_name, score) = pair
      case package_name {
        "gleam_stdlib"
        | "gleam_javascript"
        | "gleam_erlang"
        | "gleam_otp"
        | "gleam_json"
        | "gleam_time" -> #(package_name, score + 10)
        _ -> pair
      }
    })

  list.sort(ranking, fn(one, other) {
    case one, other {
      // Exact matches always come first.
      #(name, _), _ if name == phrase -> order.Lt
      _, #(name, _) if name == phrase -> order.Gt
      // Otherwise compare the scores.
      #(_, score_one), #(_, score_other) -> int.compare(score_other, score_one)
    }
  })
  |> list.map(fn(pair) { pair.0 })
}

fn count_occurrences(list: List(a)) -> Dict(a, Int) {
  list.fold(list, dict.new(), fn(counters, name) {
    dict.upsert(counters, name, fn(occurrences) {
      option.unwrap(occurrences, 0) + 1
    })
  })
}

/// Some words have common misspellings or associated words so we add those to
/// the search to get all appropriate results.
fn expand_search_term(term: String) -> List(String) {
  case term {
    "postgres" | "postgresql" -> ["postgres", "postgresql"]
    "mysql" | "mariadb" -> ["mysql", "mariadb"]
    "redis" | "valkey" -> ["redis", "valkey"]
    "regex" | "regexp" -> ["regex", "regexp"]
    "luster" -> ["luster", "lustre"]
    "mail" -> ["mail", "email"]
    term -> [term]
  }
}

fn remove(index: TextSearchIndex, name: String) -> Result(Nil, Error) {
  ethos.delete_value(index.table, name)
  |> result.replace_error(error.EtsTableError)
}

fn stem_words(phrase: String) -> List(String) {
  phrase
  |> string.lowercase
  |> string.replace("-", " ")
  |> string.replace("_", " ")
  |> string.replace(",", " ")
  |> string.replace(".", " ")
  |> string.replace("!", " ")
  |> string.replace("/", " ")
  |> string.replace("'", "")
  |> string.split(" ")
  |> list.filter(fn(word) { word != "" })
  |> list.map(normalise_spelling)
  |> list.map(porter_stemmer.stem)
  |> list.unique
}

fn normalise_spelling(word: String) -> String {
  case word {
    "analyze" -> "analyse"
    "authorize" -> "authorise"
    "behavior" -> "behaviour"
    "categorize" -> "categorise"
    "color" -> "colour"
    "customization" -> "customisation"
    "customize" -> "customise"
    "honor" -> "honour"
    "initialize" -> "initialise"
    "labeled" -> "labelled"
    "labor" -> "labour"
    "license" -> "licence"
    "modeling" -> "modelling"
    "normalization" -> "normalisation"
    "normalize" -> "normalise"
    "optimization" -> "optimisation"
    "optimize" -> "optimise"
    "organize" -> "organise"
    "parameterize" -> "parameterise"
    "deserialization" -> "deserialisation"
    "deserialize" -> "deserialise"
    "serialization" -> "serialisation"
    "serialize" -> "serialise"
    "standardize" -> "standardise"
    "summarize" -> "summarise"
    "synchronize" -> "synchronise"
    "tokenize" -> "tokenise"
    _ -> word
  }
}
