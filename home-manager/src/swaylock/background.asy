import smoothmanifold;

settings.outformat = "png";
settings.render = 8;

size(32 cm, 18 cm, keepAspect = true);

// pen bgcolor = rgb("#0c0c10");
pen bgcolor = rgb("#000000");
pen cwhite = rgb("#abb2bf");
pen cwgreen = rgb("#329c48");
pen cgreen = rgb("#00bc96");
pen ccyan = rgb("#56b6c2");
pen cblue = rgb("#61afef");
pen cdblue = rgb("#3070f0");
pen cpurple = rgb("#c678dd");
pen cmagenta = rgb("#e772c1");
pen cyellow = rgb("#f1fa8c");
pen caltyellow = rgb("#eebb9e");
pen corange = rgb("#ffb86c");
pen cred = rgb("#e86671");

defaultpen(1pt + cwhite);

pen[] clrs = {cwgreen, cgreen, ccyan, cblue, cpurple, cmagenta, corange, cred};
int clsize = clrs.length;

pair sw = (-1,-1);
pair nw = (-1,19);
pair ne = (33,19);
pair se = (33,-1);

path fr = (0,0)--(32,0)--(32,18)--(0,18)--cycle;
fill(fr, bgcolor);

pair far(real ang) { return 40*dir(ang); }

void oldshipout (string prefix, picture pic, orientation orntn, string format, bool wait, bool view, string options, string script, light lt, projection P) = shipout;
shipout = new void (string prefix, picture pic, orientation orntn, string format, bool wait, bool view, string options, string script, light lt, projection P)
{
    clip(fr);
    oldshipout(prefix, pic, orntn, format, wait, view, options, script, lt, P);
};

real r = 3.0;
pair ctr = (16.035,9);

real r2 = r+1.5;

path circ = circle(ctr, r);
path sq = shift(ctr) * scale(r2) * usquare;

filldraw(sq ^^ circ, cwhite, cwhite);
label(ctr, "\texttt{password}", fontsize(24pt));

real r3 = 2.4, r4 = 3.1;
real h = 4.5;

path ssc = subpath(unitcircle, 0, length(unitcircle)/2);

filldraw(
    shift(ctr) * rotate(90) * (
        (r4, r2) -- 
        (shift((0,r2+h)) * scale(r4) * ssc)
        -- (-r4,r2) -- (-r3,r2) --
        reverse(shift((0,r2+h)) * scale(r3) * ssc)
        -- (r3,r2) -- cycle
    ), cwhite, cwhite
);
