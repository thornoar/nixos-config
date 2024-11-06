// Positioning
#let centering(shallow: true, content) = {
  if shallow { stack(dir: ltr, spacing: 1fr, [], content, []) } else { align(center)[ #content ] }
}
#let mfrac(a, b) = move(a, dy: -0.2em) + "/" + move(b, dy: 0.2em, dx: -0.1em)
#let vphantom(size) = box(height: size, width: 0pt, [])
#let hphantom(size) = box(height: 0pt, width: size, [])

// Colors
#let palered = rgb("#ffc0c0")
#let palegreen = rgb("#c0ffc0")
#let paleblue = rgb("#c0c0ff")
#let paleyellow = rgb("#ffffc0")
#let palemagenta = rgb("#ffc0ff")
#let palecyan = rgb("#c0ffff")
#let palegray = rgb("#f3f3f3")

// Math
#let circ = math.circle.stroked.tiny
#let hs = h(5pt)
#let lle = math.lt.eq.slant
#let gge = math.gt.eq.slant
