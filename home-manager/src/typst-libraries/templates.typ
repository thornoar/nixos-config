#import "@preview/wrap-it:0.1.1": *
#import "@preview/wordometer:0.1.4": total-words, word-count
#import "@preview/equate:0.3.1": equate
#import "@preview/quick-maths:0.2.1": shorthands

// Layout

#let comp(f1, f2) = x => f2(f1(x))

#let font = "TeX Gyre Schola"
#let pagecount(format) = doc => {
  set page(numbering: format)
  counter(page).update(1)
  doc
}
#let mtxt(str) = [ #set text(font: font); #text(str) ]

#let article-rule = doc => {
  set page("a4", margin: (x: 0.7in, y: 1in))
  set text(12pt, font: font)
  set par(justify: true, leading: 0.5em)

  set heading(numbering: "1.1.")
  show heading.where(level:1): it => {
    counter(math.equation).update(0)
    it
  }
  show heading: it => {
    it
    v(0.1cm)
  }

  set math.equation(numbering: n => {
    let h1 = counter(heading).get().first()
    numbering("(1.1.1)", h1, n)
  }, supplement: none)
  show: equate.with(sub-numbering: false, number-mode: "label")
  show: shorthands.with(
    ($>=$, math.gt.eq.slant),
    ($<=$, math.lt.eq.slant)
  )

  show outline.entry.where(level: 1): it => {
    v(1em, weak: true)
    strong(it)
  }

  set figure(gap: 1.5em)

  show link: it => {
    if (type(it.dest) == str) {
      set text(blue)
      it
    } else { it }
  }

  doc
}

// Templates

#let title-rule(
  downstep: 0pt,
  title,
  author: "Roman Maksimovich",
  titlefunction : none,
  keywords: (),
  keywordsfunction: none,
  keywordlength: auto,
  abstract: none,
  abstractfunction: none,
  logo: none,
  date: datetime.today(),
  format: "[month repr:long] [day], [year]",
  index: true,
  indextitle: "Contents",
  index-of-figures: false,
  figurestitle: "Index of Figures",
  index-of-tables: false,
  tablestitle: "Index of Tables",
  index-of-listings: false,
  listingstitle: "Index of Listings",
  pagenumbering: "1"
) = doc => {
  context {
    let fontsize = text.size
    v(downstep)
    set par(first-line-indent: 0pt)
    
    if (titlefunction != none) {
      titlefunction(title, author)
    } else {
      set par(justify: false)
      text(size: 2*fontsize+1pt, strong(title))
      linebreak(); linebreak()
      text(size: fontsize+5pt, author)
    }

    v(1fr)

    if (keywords.len() > 0) {
      if (keywordsfunction != none) {
        keywordsfunction(keywords)
      }
      else {
        let rownum = calc.ceil(calc.sqrt(keywords.len()))
        let colnum = calc.ceil(keywords.len() / rownum)
        let len = keywordlength
        if (len == auto) { len = calc.min(100%, colnum*25%) }

        text(size: fontsize+2pt, strong([ Keywords ])); linebreak()
        line(start: (-2.5%, 0pt), length: len + 5%)
        block(width: len, columns(colnum, gutter: 1em, {
          let count = 0
          for keyword in keywords {
            if (count >= rownum) { colbreak(); count = 0 }
            text(keyword); linebreak(); //linebreak()// text(", ")
            count += 1
          }
        }))
      }
    }

    v(1fr)

    if (abstract != none) {
      if (abstractfunction != none) {
        abstractfunction(abstract)
      }
      else {
        set par(first-line-indent: 0pt)

        text(size: fontsize+2pt, strong([ Abstract ])); linebreak()
        align(center, line(length: 105%))
        abstract
        align(center, line(length: 105%))
        v(1em)
      }
    }

    if (logo != none) {
      align(center)[
        #logo
        #date.display(format)
      ]
    }
  }

  pagebreak()

  set page(numbering: pagenumbering)

  context {
    let hascontents = false
    if index and (query(heading).len() > 0) {
      outline(title: indextitle, target: heading)
      hascontents = true
    }
    v(1fr)
    if index-of-figures and (query(figure.where(kind: image)).len() > 0) {
      outline(title: figurestitle, target: figure.where(kind: image))
      hascontents = true
    }
    v(1fr)
    if index-of-tables and (query(selector(figure.where(kind: table))).len() > 0) {
      outline(title: tablestitle, target: figure.where(kind: table))
      hascontents = true
    }
    v(1fr)
    if index-of-listings and (query(selector(figure.where(kind: raw))).len() > 0) {
      outline(title: listingstitle, target: figure.where(kind: raw))
      hascontents = true
    }
    if hascontents { pagebreak() }
  }

  doc
}

#let assignment-title(
  title: [title],
  due: [date],
  ext1: [],
	ext2: [],
	inset: (left: 8pt, bottom: 8pt),
	fontsize: 14pt
) = {
  let hasextra = ext1 != [] or ext2 != []
  let fields = (
    (
      table.cell(
        colspan: if (hasextra) { 2 } else { 1 }, inset: inset,
        {
          set text(20pt)
          title; h(1fr)
        }
      ),
      table.hline(),
      { text(fontsize, "Roman Maksimovich, ID: 21098878"); h(1fr) }
    ),
    if (hasextra) {
      (
        {
          set text(fontsize)
          ext1
          h(1fr)
        }
      )
    } else { () },
    {
      text(fontsize, "Due date: " + due)
      h(1fr)
    },
    if (hasextra) {
      (
        {
          set text(fontsize)
          ext2
          h(1fr)
        }
      )
    } else { () },
  ).flatten()
  table(
    columns: if (hasextra) { 2 } else {1},
    align: (left, left),
    inset: (left: inset.left, bottom: 3pt),
    stroke: (left: 1pt, top: none, right: none, bottom: none),
    ..fields
  )
}

#let assignment-title-rule(
  header: [header],
	title: [title],
	due: [date],
	ext1: [],
	ext2: [],
	inset: (left: 8pt, bottom: 8pt),
	fontsize: 14pt
) = doc => {
  set page(
    "a4",
    margin: (x: 0.5in, top: 0.8in, bottom: 0.5in),
    header: if (header == none) { none } else {
      box(
        stroke: (left: none, right: none, top: none, bottom: .7pt),
        inset: (bottom: 5pt),
        header
      )
    },
    numbering: "1"
  )
  set text(size: 12pt)
  assignment-title(title: title, due: due, ext1: ext1, ext2: ext2, inset: inset, fontsize: fontsize)

  show: word-count

  doc
}

#let course-assignment-preamble(course, part, due, subnumbering: true) = doc => {
  set math.equation(supplement: none, numbering: "(1.1.1)")
  // set text(font: "TeX Gyre Schola")
  show math.equation: set text(font: "New Computer Modern Math")
  show: equate.with(sub-numbering: subnumbering, number-mode: "label", breakable: true)

  show: assignment-title-rule(
    header: [ #course Homework, #part #h(1fr) Roman Maksimovich ],
    title: course + " Homework, " + part,
    due: due
  )

  doc
}

#let assignment-problem(supplement: "Exercise", number, fmt: comp(strong, underline)) = fmt({
  text(supplement + " " + number + ".");
  linebreak()
})
#let solution = { underline(emph("Solution:")); h(2pt) }

#let diploma(
  barwidth: 38mm,
  barfill: white,
  logo: none,
  logovstep: auto,
  labelfont: "FreeSans",
  palesize: 100pt,
  palefill: auto,
  palevstep: auto,
  palehstep: 0mm,
  palelabel: "DIPLOMA",
  labelsize: 43pt,
  labelfill: auto,
  labelvstep: auto,
  label: none,
  orgsize: auto,
  org: [],
  titlesize: 30pt,
  title: [],
  body: [],
  footsize: auto,
  foot: []
) = {
  if (logo != none and logovstep == auto) { logovstep = (barwidth - logo.width)/2 }
  if (palefill == auto) { palefill = if (barfill != none) { barfill.lighten(20%) } else { none } }
  if (palevstep == auto) { palevstep = logovstep }
  if (labelvstep == auto) { labelvstep = palevstep }

  context {
    assert(page.margin != auto, message: "please specify explicit margins")
    place(
      left,
      dx: -page.margin.left,
      dy: -page.margin.top,
      rect(
        fill: barfill,
        width: barwidth,
        height: 100% + page.margin.top + page.margin.bottom
      )
    )
    if (logo != none) {
      place(
        left+top,
        dx: -page.margin.left + (barwidth - logo.width)/2,
        dy: -page.margin.top + logovstep,
        logo
      )
    }

    let vlabel(center, fontsize, vstep, fill, str) = {
      let content = rotate(
        -90deg,
        text(str, size: fontsize, stroke: fill, fill: fill, font: labelfont)
      )
      let size = measure(content)
      let dx = if center {
        (barwidth - size.width)/2 - page.margin.left
      } else {
        -size.width/2 - page.margin.left - size.height/2 + barwidth + palehstep
      }
      let dy = (size.height - size.width)/2 + page.margin.bottom - vstep
      place(left+bottom, dx: dx, dy: dy, content)
    }

    vlabel(false, palesize, palevstep, palefill, palelabel)
    vlabel(true, labelsize, labelvstep, if (labelfill == auto) { page.fill } else { labelfill }, label)

    if (palehstep > -1mm) {
      place(
        left,
        dx: -page.margin.left + barwidth,
        dy: -page.margin.top,
        rect(
          stroke: none,
          fill: if (page.fill == none) { white } else { page.fill },
          width: 1cm,
          height: 100% + page.margin.top + page.margin.bottom
        )
      )
    }

    {
      set text(stroke: barfill, fill: barfill, size: if (orgsize == auto) { text.size } else { orgsize })
      org
    }
    linebreak()
    linebreak()
    {
      set text(size: titlesize)
      title
    }
    linebreak()
    linebreak()

    body

    if (foot != none) {
      v(1fr)
      set text(size: if (footsize == auto) { text.size } else { footsize })
      foot
    }
  }
}

#let clean-numbering(..schemes) = {
  (..nums) => {
    let (section, ..subsections) = nums.pos()
    let (section_scheme, ..subschemes) = schemes.pos()

    if subsections.len() == 0 {
      numbering(section_scheme, section)
    } else if subschemes.len() == 0 {
      numbering(section_scheme, ..nums.pos())
    }
    else {
      clean-numbering(..subschemes)(..subsections)
    }
  }
}

#let attention(
  body,
  title: [*ATTENTION*],
  font: auto,
  color: red,
  stroke: 0.5pt,
  centering: false
) = context {
  let blockinset = text.size + 2pt
  let blockfont = font
  if (font == auto) {
    blockfont = "Noto Sans"
  } else if (font == none) {
    blockfont = text.font
  }
  let titleshift = text.size / 2
  let titlemargin = text.size / 6
  let res = block(
    above: 2em,
    stroke: stroke + color,
    inset: blockinset,
    {
      set text(font: blockfont, fill: color)
      place(
        top + left,
        dy: -titleshift - blockinset,
        dx: titleshift - blockinset,
        block(fill: white, inset: titlemargin, strong(title))
      )
      body
    }
  )
  if centering {
    stack(dir: ltr, spacing: 1fr, [], res, [])
  } else { res }
}

#let skew(angle, vscale: 1) = content => {
  let (a,b,c,d)= (1,vscale*calc.tan(angle),0,vscale)
  let E = (a + d)/2
  let F = (a - d)/2
  let G = (b + c)/2
  let H = (c - b)/2
  let Q = calc.sqrt(E*E + H*H)
  let R = calc.sqrt(F*F + G*G)
  let sx = Q + R
  let sy = Q - R
  let a1 = calc.atan2(F,G)
  let a2 = calc.atan2(E,H)
  let theta = (a2 - a1) /2
  let phi = (a2 + a1)/2

  set rotate(origin: bottom+center)
  set scale(origin: bottom+center)

  rotate(phi,scale(x: sx*100%, y: sy*100%,rotate(theta,content)))
}

#let named-gaps(
  names,
  length: 10em,
  stroke: 0.5pt,
  row-gutter: 1em,
  column-gutter: .5em,
  shift: auto
) = {
  if shift == auto { shift = row-gutter / 5 }
  let gap = align(bottom, move(dy: shift, line(length: length, stroke: stroke)))
  grid(
    columns: 2,
    rows: names.len(),
    row-gutter: row-gutter,
    column-gutter: column-gutter,
    align: (right, left),
    ..names.map(name => (name+[:], gap)).flatten()
  )
}
