// Positioning
#let centering(shallow: true, content) = {
  if shallow { stack(dir: ltr, spacing: 1fr, [], content, []) } else { align(center)[ #content ] }
}
#let mfrac(a, b) = move(a, dy: -0.2em) + "/" + move(b, dy: 0.2em, dx: -0.1em)
#let vphantom(size) = box(height: size, width: 0pt, [])
#let hphantom(size) = box(height: 0pt, width: size, [])
#let hh = h(.5mm)

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
#let liminf = $limits(op("lim inf"#hh))$
#let limsup = $limits(op("lim sup"#hh))$
#let dx = $d x$
#let dt = $d t$
#let gen(it) = $lr(angle.l it angle.r)$
#let st = $#h(0pt):#h(5pt)$
