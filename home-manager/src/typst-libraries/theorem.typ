// Theorems support

#import "@preview/ctheorems:1.1.2": thmrules, thmenv, thmproof

// Basic function

#let thmstyle(
  identifier,
  head,
  supplement: auto,
  numbering: "1.1.",
  refnumbering: "1.1",
  titlefmt: auto,
  namefmt: x => [ (#x):],
  bodyfmt: x => x,
  separator: h(0.2em),
  base: "heading",
  base_level: none,
  ..blockargs
) = {
  if (head == auto) { head = upper([#identifier.first()]) + identifier.slice(1, identifier.len()) }
  if (supplement == auto) { supplement = head }

  let fmt(name, number, body, title: auto) = {
    if (name != none) { name = namefmt(name) } else { name = [] }
    if (title == auto) { title = head }
    if (number != none) { title += " " + number }

    if (titlefmt == auto) {
      if (numbering == none) {
        title = [ *#title.* ]
      } else {
        title = strong(title)
      }
    } else {
      title = titlefmt(title)
    }
    body = bodyfmt(body)

    align(left, block(
      width: 100%,
      breakable: true,
      ..blockargs.named(),
      [#title#name#separator#body]
    ))
  }

  return thmenv(
    identifier,
    base,
    base_level,
    fmt
  ).with(
    supplement: supplement,
    numbering: numbering,
    refnumbering: refnumbering
  )
}

// Shorthands

#let plainstyle(identifier, head, breakable: false, ..args) = thmstyle(
  identifier,
  head,
  breakable: breakable,
  ..args
)
#let statestyle(identifier, head, breakable: false, ..args) = thmstyle(
  identifier,
  head,
  breakable: breakable,
  bodyfmt: emph,
  ..args
)
#let proofstyle(identifier, head, breakable: true, ..args) = thmproof(
  identifier,
  head,
  breakable: breakable,
  ..args,
  inset: (left: 10pt, bottom: 2pt),
  radius: 0pt,
  stroke: (left: black),
  separator: [_:_#h(0.2em)]
)

#let blockstyle(inset: 10pt, radius: 10pt, stroke: black, ..args) = thmstyle.with(
  inset: inset,
  radius: radius,
  stroke: stroke,
  ..args
)

// Further shorthands

#let def = plainstyle("definition", "Definition")
#let nota = plainstyle("notation", "Notation")
#let exam = plainstyle("example", "Example")
#let exer = plainstyle("exercise", "Exercise")
#let note = plainstyle("note", "Note")

#let th = statestyle("theorem", "Theorem")
#let lm = statestyle("lemma", "Lemma")
#let prop = statestyle("proposition", "Proposition")
#let prb = statestyle("problem", "Problem")
#let cor(base: "heading") = statestyle("corollary", "Corollary", base: base)

#let pf = proofstyle("proof", "Proof")

// Russian versions

#let rdef = plainstyle("definition", "Определение")
#let rnota = plainstyle("notation", "Нотация")
#let rexam = plainstyle("example", "Пример")
#let rexer = plainstyle("exercise", "Упражнение")
#let rnote = plainstyle("note", "Замечание")

#let rth = statestyle("theorem", "Теорема")
#let rlm = statestyle("lemma", "Лемма")
#let rprop = statestyle("proposition", "Предложение")
#let rprb = statestyle("problem", "Задача")
#let rcor(base: "heading") = statestyle("corollary", "Следствие", base: base)

#let rpf = proofstyle("proof", "Доказательство")

// Main show rule

#let theorem = doc => {
  show: thmrules.with(qed-symbol: $square.filled.medium$)
  show link: underline
  doc
}
