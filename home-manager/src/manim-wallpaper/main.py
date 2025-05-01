from manim import *
from math import sin, cos, sqrt
from manim.typing import BezierPath
from numpy import random, arctan2, sqrt, floor

class Main (Scene):
    def construct (self):
        circle = Circle()
        self.play(
            Create(circle),
            run_time = 1
        )
        self.play(
            Uncreate(circle),
            run_time = 1
        )
