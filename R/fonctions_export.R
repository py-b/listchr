#' Simple list to character
#'
#' Donne une représentation textuelle d'une liste dont les éléments
#' de plus bas niveau sont des vecteurs atomiques.
#'
#' La valeur retournée est toujours une chaine, même si la liste
#' a plusieurs niveaux de hiérarchie. Pour conserver la hiérarchie,
#' appeler lapply ou purr::map_chr à un niveau hiérarchique donné.
#'
#' @param x une liste dont les éléments de plus bas niveau sont des
#'   vecteurs atomiques.
#' @param atom_sep séparateur des éléments atomiques.
#' @param list_sep séparateur des éléments de la liste.
#' @param simplify_threshold nombre d'éléments atomiques à partir duquel
#'   la chaîne de caractères est simplifiée (premier et dernier éléments
#'   séparés par un symbole, "->" par défaut).
#' @param simplify_symbol chaîne de caractère, si la chaîne est simplifiée.
#' @return Une chaîne de caractères, la représentation texte de la liste.
#' @examples
#' list_to_chr(list(letters[1:2], 1:3, 4))
#' list_to_chr(list(letters, LETTERS), simplify_threshold = 10)
#' @export

list_to_chr <- function(x,
                        atom_sep = ";",
                        list_sep = "|",
                        simplify_threshold = Inf,
                        simplify_symbol = "->") {

  if (is.null(x)) return(NA_character_)

  if (is.atomic(x)) {
    if (length(x) >= simplify_threshold) {
      res <- paste0(x[1], simplify_symbol, rev(x)[1])
    } else {
      res <- paste0(x, collapse = atom_sep)
    }
    return(res)
  }

  res <- ""
  for (ele in x) {
    res <-
      paste(
        res,
        list_to_chr(
          x = ele,
          atom_sep = atom_sep,
          simplify_threshold = simplify_threshold
        ),
        sep = list_sep
      )
  }
  substring(res, nchar(list_sep) + 1)

}


#' Simple list to character (for a data frame)
#'
#' Convertit toutes les colonnes-listes d'un data.frame en vecteurs
#' de chaînes de caractères.
#'
#' @param df un data.frame contenant des colonnes-listes.
#' @inheritParams list_to_chr
#' @return Un data.frame, les colonnes simples (atomiques) restent
#'   inchangées, les colonnes-listes (recursives) sont transformées
#'   en vecteurs de chaînes de caractères.
#' @examples
#' df <- data.frame(x = 1:2)
#' df$y <- list(letters[1:3], 1:2)
#' df$z <- list(list(3:4, 5:7), "d")
#' listcol_to_chr(df)
#' @export
#' @importFrom dplyr "%>%"

listcol_to_chr <- function(df,
                           atom_sep = ";",
                           list_sep = "|",
                           simplify_threshold = Inf,
                           simplify_symbol = "->") {

  df %>%
    dplyr::mutate_if(
    is.recursive,
    function(lc) {
      lc %>%
        vapply(
          list_to_chr,
          FUN.VALUE = character(1),
          atom_sep,
          list_sep,
          simplify_threshold,
          simplify_symbol
        )
    }
  )

}


#' Separated character to list
#'
#' Convertit une chaîne de caractère en liste,
#' selon des séparateurs définis.
#'
#' Si tous les éléments de la liste en sortie sont
#' atomiques, la liste est simplifiée
#'
#' @param chr chaîne de caractères à convertir.
#' @inheritParams list_to_chr
#' @return Une liste (NULL si la chaîne est vide ou NA).
#' @examples
#' chr_to_list("1;2;3|a;b")
#' chr_to_list("1;2;3")
#' chr_to_list("1|2|3")
#' chr_to_list(c("1;3", "2|4"))
#' chr_to_list(c("1;3", "2;4"))
#' @export

chr_to_list <- function(chr,
                        atom_sep = ";",
                        list_sep = "|") {

  chr <- as.character(chr)
  chr[chr == ""] <- NA

  res <- strsplit(chr, list_sep, fixed = TRUE)
  res <- lapply(res, strsplit, atom_sep, fixed = TRUE)
  res <- lapply(res, function(x) if (anyNA(x[[1]])) NULL else x)

  # simplification éventuelle
  inf1 <- all(sapply(res, length) <= 1)
  if (inf1) res <- lapply(res, function(x) x[[1]])

  res

}


#' Separated character to list (for a data frame)
#'
#' Convertit une chaîne de caractère en liste,
#'   selon des séparateurs définis.
#'
#' @param df un data.frame contenant des colonnes à transformer en
#'   colonnes-listes.
#' @param vars colonnes à transformer (noms ou index)
#' @inheritParams list_to_chr
#' @examples
#' df <-
#'   data.frame(
#'     x = c("1;2|3", "a|b"),
#'     y = c("1;3", "2|4"),
#'     z = c("1;3", "2;4")
#'   )
#' chr_to_listcol(df, "x")
#' chr_to_listcol(df, 2:3)
#' @export

chr_to_listcol <- function(df,
                           vars,
                           atom_sep = ";",
                           list_sep = "|") {

  df %>%
    dplyr::mutate_at(
      vars,
      chr_to_list,
      atom_sep,
      list_sep
    )

}
