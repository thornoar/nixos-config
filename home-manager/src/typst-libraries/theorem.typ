// Theorems support

#import "@preview/ctheorems:1.1.3": thmenv, thmrules, thmproof

// Basic function

#let thmstyle(
  identifier,
  head,
  supplement: auto,
  numbering: "1.1",
  refnumbering: "1.1",
  titlefmt: auto,
  namefmt: x => [ (#x)],
  bodyfmt: x => x,
  separator: h(0.2em),
  base: "heading",
  base_level: none,
  ..blockargs
) = {
  // if (head == auto) {
  //   head = upper(
  //     [#identifier.first()]
  //   ) + identifier.slice(1, identifier.len())
  // }
  if (supplement == auto) { supplement = head }

  let fmt(name, number, body, title: auto) = {
    if (name != none) { name = namefmt(name) } else { name = [] }
    if (title == auto) { title = head }
    if (number != none) { title += " " + number }
    title = [#title.]

    if (titlefmt == auto) {
      title = strong(title)
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

#let plainstyle(head, breakable: true, ..args) = thmstyle(
  "thm",
  head,
  breakable: breakable,
  // inset: (top: 0.1in),
  ..args
)
#let statestyle(head, breakable: false, ..args) = thmstyle(
  "thm",
  head,
  breakable: breakable,
  bodyfmt: emph,
  // inset: (top: 0.1in),
  ..args
)
#let proofstyle(breakable: true, ..args) = thmproof(
  "proof",
  "Proof",
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

// Main show rule

#let theorem-rule = doc => {
  show: thmrules.with(qed-symbol: $square.filled.medium$)
  // show link: underline
  doc
}
